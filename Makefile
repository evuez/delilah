SOURCES := $(wildcard */*.hs)

.PHONY: install
install:
	stack build --copy-bins

.PHONY: format
format: $(SOURCES)

.PHONY: $(SOURCES)
$(SOURCES):
	hindent $@

.PHONY: test
test:
	stack test --fast

.PHONY: test.watch
test.watch:
	stack test delilah:delilah-test --file-watch --fast

.PHONY: build.watch
build.watch:
	ghcid --command "stack ghci --ghci-options=-fobject-code" --allow-eval --lint
