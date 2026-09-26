SCALA_CLI ?= scala-cli
.PHONY: test live check build install fmt

test:
	$(SCALA_CLI) test . -- --exclude-tags=live

live:
	$(SCALA_CLI) test . -- --include-tags=live

check:
	$(SCALA_CLI) compile --test . && $(SCALA_CLI) fmt --check .

build:
	mkdir -p bin
	$(SCALA_CLI) --power package . -o bin/desk.jar --assembly --preamble=false -f

install: build
	mkdir -p $(HOME)/.local/bin $(HOME)/.local/lib
	cp bin/desk.jar $(HOME)/.local/lib/desk.jar
	printf '#!/bin/sh\nexec "$${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" -jar "$$HOME/.local/lib/desk.jar" "$$@"\n' > $(HOME)/.local/bin/desk
	chmod +x $(HOME)/.local/bin/desk

fmt:
	$(SCALA_CLI) fmt .
