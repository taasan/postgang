exe = postgang

$(exe): Main.hs
	ghc -O2 $< -o $@
	strip $@

.PHONY: format
format:
	brittany --write-mode inplace Main.hs
	stylish-haskell -i Main.hs

.PHONY: clean
clean:
	$(RM) $(exe)
