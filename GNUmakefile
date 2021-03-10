GHC ?= ghc
INSTALL_BIN ?= ~/.local/bin

exe = postgang

$(exe): Main.hs
	$(GHC) -O2 $< -o $@
	strip $@
	if command -v upx >/dev/null 2>&1; then upx --brute $@; fi


.PHONY: format
format:
	brittany --write-mode inplace Main.hs
	stylish-haskell -i Main.hs

.PHONY: clean
clean:
	$(RM) $(exe)

.PHONY: install
install: $(exe)
	mkdir -p $(INSTALL_BIN)
	install -m 0755 -t $(INSTALL_BIN) $(exe)
