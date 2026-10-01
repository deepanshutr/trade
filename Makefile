SCALA_CLI ?= scala-cli
.PHONY: test test-python live slow check build install install-launcher fmt

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

install: build install-launcher
	cp bin/desk.jar "$$HOME/.local/lib/desk.jar"

# The shell reads HOME so an odd character in it cannot change the recipe.
# Nothing is written until python3 is found, its path is safe to quote and its libraries import.
# On macOS the shell prints a found command as an absolute path.
install-launcher:
	set -e; \
	python=$$(command -v python3) || { echo "python3 not found" >&2; exit 1; }; \
	case "$$python$$HOME" in *[\"\`\\\$$]*) echo "unsafe character in an install path" >&2; exit 1;; esac; \
	"$$python" -c 'import cryptography, Crypto, eth_account'; \
	mkdir -p "$$HOME/.local/bin" "$$HOME/.local/lib"; \
	cp tools/keychain_wallet.py "$$HOME/.local/lib/desk-keychain.py"; \
	chmod 600 "$$HOME/.local/lib/desk-keychain.py"; \
	printf '#!/bin/sh\nexport DESK_PYTHON="$${DESK_PYTHON:-%s}"\nexport DESK_KEY_HELPER="$${DESK_KEY_HELPER:-%s}"\nexec "$${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" -jar "$$HOME/.local/lib/desk.jar" "$$@"\n' "$$python" "$$HOME/.local/lib/desk-keychain.py" > "$$HOME/.local/bin/desk"; \
	chmod +x "$$HOME/.local/bin/desk"

fmt:
	$(SCALA_CLI) fmt .
