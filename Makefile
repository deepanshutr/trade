SCALA_CLI ?= scala-cli
.PHONY: test test-python live slow check build install fmt

test: test-python
	$(SCALA_CLI) --power test . --server=false -- --exclude-tags=live,slow

test-python:
	python3 -m unittest discover -s test -p 'test_*.py'

live:
	$(SCALA_CLI) --power test . --server=false -- --include-tags=live

slow:
	$(SCALA_CLI) --power test . --server=false -- --include-tags=slow

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
