SCALA_CLI ?= scala-cli
.PHONY: test test-python live slow check build windows-image install install-launcher fmt

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

# A Windows package. Its Desk.exe runs Java inside its own process, so one firewall rule can
# name Desk alone. The console calls use a Java 21 feature still in preview, so it is turned on.
# Attach stays off, as in the macOS launcher.
windows-image: build
	rm -rf bin/app bin/image
	mkdir -p bin/app
	cp bin/desk.jar bin/app/
	jpackage --type app-image --name Desk --input bin/app --main-jar desk.jar --dest bin/image \
	  --win-console --java-options --enable-preview \
	  --java-options --enable-native-access=ALL-UNNAMED \
	  --java-options -XX:+DisableAttachMechanism --java-options -Dstdout.encoding=UTF-8

install: build install-launcher
	cp bin/desk.jar "$$HOME/.local/lib/desk.jar"

# The shell reads HOME so an odd character in it cannot change the recipe.
# Nothing is written until python3 is found, its libraries import and the interpreter path
# that python reports for itself is absolute and safe to quote. That path is the one recorded.
# The launcher starts Java with attach disabled so other programs cannot read its memory.
# DESK_VAULT, when given, is the absolute folder of the owner vault. It is checked the same way
# before anything is written and recorded next to the other two.
install-launcher:
	set -e; \
	python=$$(command -v python3) || { echo "python3 not found" >&2; exit 1; }; \
	case "$$DESK_VAULT" in ""|/*) ;; *) echo "DESK_VAULT is not an absolute path" >&2; exit 1;; esac; \
	case "$$python$$HOME$$DESK_VAULT" in *[\"\`\\\$$]*) echo "unsafe character in an install path" >&2; exit 1;; esac; \
	"$$python" -c 'import cryptography, Crypto, eth_account'; \
	real=$$("$$python" -c 'import sys; print(sys.executable)'); \
	case "$$real" in /*) ;; *) echo "python3 did not report an absolute interpreter path" >&2; exit 1;; esac; \
	case "$$real" in *[\"\`\\\$$]*) echo "unsafe character in an install path" >&2; exit 1;; esac; \
	mkdir -p "$$HOME/.local/bin" "$$HOME/.local/lib"; \
	cp tools/keychain_wallet.py "$$HOME/.local/lib/desk-keychain.py"; \
	chmod 600 "$$HOME/.local/lib/desk-keychain.py"; \
	{ printf '#!/bin/sh\nexport DESK_PYTHON="$${DESK_PYTHON:-%s}"\nexport DESK_KEY_HELPER="$${DESK_KEY_HELPER:-%s}"\n' "$$real" "$$HOME/.local/lib/desk-keychain.py"; \
	if [ -n "$$DESK_VAULT" ]; then printf 'export DESK_VAULT="$${DESK_VAULT:-%s}"\n' "$$DESK_VAULT"; fi; \
	printf 'exec "$${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" -XX:+DisableAttachMechanism -jar "$$HOME/.local/lib/desk.jar" "$$@"\n'; } > "$$HOME/.local/bin/desk"; \
	chmod +x "$$HOME/.local/bin/desk"

fmt:
	$(SCALA_CLI) fmt .
