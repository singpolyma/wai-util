module Network.Wai.Util (
	handleAcceptTypes,
	noStoreFileUploads,
	bodyBytestring,
	mapHeaders,
	defHeader,
	defHeader',
	replaceHeader,
	replaceHeader',
	string,
	text,
	textBuilder,
	json,
	bytestring,
	redirect,
	redirect',
	stringAscii,
	stringHeader,
	stringHeaders,
	stringHeaders',
	responseToMailPart,
	queryLookup
) where

import Data.Char (isAscii)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Monoid (mappend, mempty)
import Control.Monad (liftM2,join)
import Control.Arrow ((***))
import Data.String (IsString, fromString)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Network.URI (URI, uriIsAbsolute)
import Network.HTTP.Types (statusIsRedirection, Status, ResponseHeaders, Header, notAcceptable406)
import Network.HTTP.Types.QueryLike (QueryLike, QueryKeyLike, toQuery, toQueryKey)
import Network.Wai (Request, Response(ResponseBuilder,ResponseFile,ResponseSource), responseLBS, requestBody, requestHeaders, responseSource)
import Network.Wai.Parse (BackEnd, parseHttpAccept)
import Network.Mail.Mime (Part(..), Encoding(QuotedPrintableText, Base64))
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Conduit (($$), Flush(Chunk))
import Data.Conduit.List (fold, sinkNull)

import Network.HTTP.Accept (selectAcceptType)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)

import qualified Data.ByteString.Lazy as LZ
import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.CaseInsensitive as CI

-- | Build an Application that supports multiple Accept types (Content Negotiation)
handleAcceptTypes :: (Monad m) => [(String, m Response)] -> Request -> m Response
handleAcceptTypes handlers req =
	fromMaybe notAcceptable handler
	where
	handler = lookup acceptType handlers
	notAcceptable = string notAcceptable406 [] (intercalate "\n" supportedTypes)
	acceptType = fromMaybe (head supportedTypes) acceptType'
	acceptType' = (selectAcceptType supportedTypes . parseHttpAccept) =<<
		lookup (fromString "Accept") (requestHeaders req)
	supportedTypes = map fst handlers

-- | 'BackeEnd' for 'parseRequestBody' that throws out any file uploads
noStoreFileUploads :: BackEnd ()
noStoreFileUploads _ _ = sinkNull

-- | Slurp in the entire request body as a 'ByteString'
bodyBytestring :: Request -> ResourceT IO ByteString
bodyBytestring req = requestBody req $$ fold mappend mempty

-- | Run a function over the headers in a 'Response'
mapHeaders :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeaders f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeaders f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeaders f (ResponseSource s h b) = ResponseSource s (f h) b

-- | Set a default value for a header in a 'Response'
defHeader :: Header -> Response -> Response
defHeader h = mapHeaders (defHeader' h)

-- | Set a default value for a header in 'ResponseHeaders'
defHeader' :: Header -> ResponseHeaders -> ResponseHeaders
defHeader' (n, v) headers = case lookup n headers of
		Just _  -> headers
		Nothing -> (n, v):headers

-- | Set the matching header name to this in a 'Response'
replaceHeader :: Header -> Response -> Response
replaceHeader h = mapHeaders (replaceHeader' h)

-- | Set the matching header name to this in 'ResponseHeaders'
replaceHeader' :: Header -> ResponseHeaders -> ResponseHeaders
replaceHeader' (n, v) = ((n,v):) . filter ((/=n) . fst)

-- | Smart constructor to build a 'Response' from a 'String'
string :: (Monad m) => Status -> ResponseHeaders -> String -> m Response
string status headers = return . defHeader defCT . ResponseBuilder status headers . Builder.fromString
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

-- | Smart constructor to build a 'Response' from a 'Text'
text :: (Monad m) => Status -> ResponseHeaders -> Text -> m Response
text status headers = return . defHeader defCT . ResponseBuilder status headers . Builder.fromText
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

-- | Smart constructor to build a 'Response' from a 'Data.Text.Lazy.Builder.Builder'
textBuilder :: (Monad m) => Status -> ResponseHeaders -> TL.Builder -> m Response
textBuilder status headers = return . defHeader defCT . ResponseBuilder status headers . Builder.fromLazyText . TL.toLazyText
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

-- | Smart constructor to build a JSON 'Response' using Aeson
json :: (Monad m, Aeson.ToJSON a) => Status -> ResponseHeaders -> a -> m Response
json status headers = return . defHeader defCT . responseLBS status headers . Aeson.encode . Aeson.toJSON
	where
	Just defCT = stringHeader ("Content-Type", "application/json; charset=utf-8")

class IsByteString a where
	bytestringToBuilder :: a -> Builder.Builder

instance IsByteString ByteString where
	bytestringToBuilder = Builder.fromByteString

instance IsByteString LZ.ByteString where
	bytestringToBuilder = Builder.fromLazyByteString

-- | Smart constructor to build a 'Response' from a 'ByteString'
bytestring :: (IsByteString bs, Monad m) => Status -> ResponseHeaders -> bs -> m Response
bytestring status headers = return . defHeader defCT . ResponseBuilder status headers . bytestringToBuilder
	where
	Just defCT = stringHeader ("Content-Type", "application/octet-stream")

-- | Smart constructor to build a redirect
--
-- Checks if the 'Status' is a redirection and the 'URI' is absolute
redirect :: Status -> ResponseHeaders -> URI -> Maybe Response
redirect status headers uri
	| statusIsRedirection status && uriIsAbsolute uri = do
		uriBS <- stringAscii (show uri)
		return $ responseLBS status ((location, uriBS):headers) mempty
	| otherwise = Nothing
	where
	Just location = stringAscii "Location"

-- | Smart constructor to build a redirect
--
-- Asserts redirect conditions with an irrefutable pattern match, only use
-- on hard-coded values.
redirect' :: (Monad m) => Status -> ResponseHeaders -> URI -> m Response
redirect' status headers uri =
	let Just r = redirect status headers uri in return r

-- | Safely convert a 'String' to types that can only encode ASCII
stringAscii :: (IsString s) => String -> Maybe s
stringAscii s
	| all isAscii s = Just (fromString s)
	| otherwise     = Nothing

-- | Safely convert a pair of 'String' to a pair suitable for use as a
-- 'Header', ensuring only ASCII characters are present.
stringHeader :: (IsString s1, IsString s2) => (String, String) -> Maybe (s1, s2)
stringHeader (n, v) = liftM2 (,) (stringAscii n) (stringAscii v)

-- | Safely convert a list of pairs of 'String' to a pair suitable for
-- use as a 'Header', ensuring only ASCII characters are present.
stringHeaders :: (IsString s1, IsString s2) => [(String, String)] -> Maybe [(s1, s2)]
stringHeaders = mapM stringHeader

-- | Unsafely convert a list of pairs of 'String' to a pair suitable for
-- use as a 'Header', ensuring only ASCII characters are present.
--
-- Asserts success with an irrefutable pattern match, only use on
-- hard-coded values.
stringHeaders' :: (IsString s1, IsString s2) => [(String, String)] -> [(s1, s2)]
stringHeaders' hs = let Just headers = stringHeaders hs in headers

-- | Convert a WAI 'Response' to an email 'Part'
--
-- Useful for re-using 'Application' code/smart constructors to send emails
responseToMailPart :: (MonadIO m) => Bool -> Response -> m Part
responseToMailPart asTxt r = do
	body <- liftIO $ Builder.toLazyByteString `fmap` builderBody
	return $ Part (T.decodeUtf8 contentType) contentEncode Nothing headers body
	where
	chunkFlatAppend m (Chunk more) = m `mappend` more
	chunkFlatAppend m _ = m
	headers = map (CI.original *** T.decodeUtf8) $ filter ((/=contentTypeName) . fst) headers'
	contentType = fromMaybe defContentType $ lookup contentTypeName headers'
	contentEncode  | asTxt     = QuotedPrintableText
	               | otherwise = Base64
	defContentType | asTxt     = fromString "text/plain; charset=utf-8"
	               | otherwise = fromString "application/octet-stream"
	builderBody = runResourceT $ body' $$ fold chunkFlatAppend mempty
	(_, headers', body') = responseSource r
	contentTypeName = fromString "Content-Type"

-- | Lookup a given key in something that acts like a query
queryLookup :: (QueryLike q, QueryKeyLike k) => q -> k -> Maybe Text
queryLookup q k = fmap (T.decodeUtf8With lenientDecode) $ join $ lookup (toQueryKey k) (toQuery q)
