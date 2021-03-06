module Network.Wai.Util (
	handleAcceptTypes,
	noStoreFileUploads,
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
	queryLookup,
	queryLookupAll
) where

import Data.Char (isAscii)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (intercalate)
import Control.Monad (liftM2,join)
import Data.String (IsString, fromString)

import Network.URI (URI, uriIsAbsolute)
import Network.HTTP.Types (statusIsRedirection, Status, ResponseHeaders, Header, notAcceptable406)
import Network.HTTP.Types.QueryLike (QueryLike, QueryKeyLike, toQuery, toQueryKey)
import Network.Wai (Request, responseLBS, requestHeaders)
import Network.Wai.Internal (Response(ResponseBuilder,ResponseFile,ResponseStream,ResponseRaw))
import Network.Wai.Parse (BackEnd, parseHttpAccept)

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
noStoreFileUploads _ _ _ = return ()

-- | Run a function over the headers in a 'Response'
mapHeaders :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeaders f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeaders f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeaders f (ResponseStream s h b) = ResponseStream s (f h) b
mapHeaders f (ResponseRaw io resp)  = ResponseRaw io (mapHeaders f resp)

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
		return $ responseLBS status ((location, uriBS):headers) LZ.empty
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

-- | Lookup a given key in something that acts like a query
queryLookup :: (QueryLike q, QueryKeyLike k) => k -> q -> Maybe Text
queryLookup k = fmap (T.decodeUtf8With lenientDecode) . join . lookup (toQueryKey k) . toQuery

-- | Get all matches for a given key in something that acts like a query
queryLookupAll :: (QueryLike q, QueryKeyLike k) => k -> q -> [Text]
queryLookupAll k = map (T.decodeUtf8With lenientDecode) . mapMaybe f . toQuery
	where
	f (ik, mv)
		| ik == k' = mv
		| otherwise = Nothing
	k' = toQueryKey k
