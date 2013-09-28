GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.5

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSwai-util-$(VERSION).a dist/wai-util-$(VERSION).tar.gz

install: dist/build/libHSwai-util-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Network/Wai/Util.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/wai-util/index.html README

README: wai-util.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/wai-util/index.html: dist/setup-config Network/Wai/Util.hs
	cabal haddock --hyperlink-source

dist/setup-config: wai-util.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSwai-util-$(VERSION).a: dist/setup-config Network/Wai/Util.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/wai-util-$(VERSION).tar.gz: README dist/setup-config Network/Wai/Util.hs
	cabal check
	cabal sdist
