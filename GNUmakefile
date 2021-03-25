CABAL ?= cabal
GHC ?= ghc
GHC_FLAGS ?= -O2
GHC_VERSION_FLAGS := -DGIT_HASH='"$(shell git rev-parse HEAD)"' -DBUILD_DATE='"$(shell date -I)"'
INSTALL_BIN ?= ~/.local/bin

exe = postgang

# $(exe): Version.hs Main.hs
.PHONY: $(exe)
$(exe): Main.hs
	$(CABAL) build $@
#	$(GHC) $(GHC_VERSION_FLAGS)  $(GHC_FLAGS) Main.hs -o $@
#	strip $@
#	if command -v upx >/dev/null 2>&1; then upx --brute $@; fi

# Version.hs: Version.hs.m4
# 	m4 \
# 	  -DGIT_HASH=$$(git rev-parse HEAD) \
# 	  -DBUILD_DATE=$$(date -I) \
# 	  $< >$@

.PHONY: format
format:
	git ls-files '*.hs' | xargs brittany --write-mode inplace
	git ls-files '*.hs' | xargs stylish-haskell -i

.PHONY: clean
clean:
	$(CABAL) clean

.PHONY: install
install: $(exe)
	mkdir -p $(INSTALL_BIN)
	install -m 0755 -t $(INSTALL_BIN) $(exe)
#	cabal install --installdir "$PWD" --install-method=copy
