.PHONY: archive
archive: clean
	stack build --ghc-options -O2 && stack install --local-bin-path .
	strip tag

.PHONY: clean
clean:
	stack clean --full
