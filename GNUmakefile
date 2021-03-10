exe = postgang

$(exe): Main.hs
	ghc -O2 $< -o $@
	strip $@
	if command -v upx >/dev/null 2>&1; then upx --brute $@; fi


.PHONY: format
format:
	brittany --write-mode inplace Main.hs
	stylish-haskell -i Main.hs

.PHONY: clean
clean:
	$(RM) $(exe)
