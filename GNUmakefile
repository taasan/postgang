STACK ?= stack
INSTALL_BIN ?= $(HOME)/.local/bin
exe = postgang

.PHONY: $(exe)
$(exe): Main.hs
	$(STACK) build $@

.PHONY: format
format:
	git ls-files '*.hs' | xargs brittany --write-mode inplace
	git ls-files '*.hs' | xargs stylish-haskell -i

.PHONY: clean
clean:
	$(STACK) clean

.PHONY: install
install:
	$(STACK) --local-bin-path=$(INSTALL_BIN) install $(exe)
	if command -v upx >/dev/null 2>&1; then upx --brute $(INSTALL_BIN)/$(exe); fi
