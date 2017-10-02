.PHONY: archive
archive: clean
	stack build --ghc-options -O2 && stack install --local-bin-path .
	strip tag
	tar -pvczf tag.tar.gz tag
	shasum -a 256 tag tag.tar.gz

.PHONY: clean
clean:
	stack clean --full
