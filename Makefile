SCALA_CLI ?= scala-cli
.PHONY: test live check build install fmt

test:
	$(SCALA_CLI) test . -- --exclude-tags=live

live:
	$(SCALA_CLI) test . -- --include-tags=live

check:
	$(SCALA_CLI) compile . && $(SCALA_CLI) fmt --check .

build:
	$(SCALA_CLI) --power package . -o bin/desk --assembly -f

install: build
	mkdir -p $(HOME)/.local/bin && cp bin/desk $(HOME)/.local/bin/desk

fmt:
	$(SCALA_CLI) fmt .
