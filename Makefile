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
# Nothing is written until python3 is found, its libraries import and the interpreter path
# that python reports for itself is absolute and safe to quote. That path is the one recorded.
# The launcher starts Java with attach disabled so other programs cannot read its memory.
# DESK_VAULT, when given, is the absolute folder of the owner vault. It is checked the same way
# before anything is written and recorded next to the other two.
# The launcher is trade on every system, because Windows opens Display settings for a bare desk.
# In ~/.local/bin the install replaces trade or removes desk only when its text names the Desk
# jar. A trade that does not stops the install before anything is written. A desk that does,
# from an earlier install, is removed after the new launcher is in place, so the old name
# cannot start a stale launcher.
# The launcher is written to trade.new and moved onto trade, so a link named trade is replaced.
# macOS /bin/sh is bash 3.2, which carries on under set -e when a group's redirection fails, so
# that write exits by hand.
install-launcher:
	set -e; \
	python=$$(command -v python3) || { echo "python3 not found" >&2; exit 1; }; \
	case "$$DESK_VAULT" in ""|/*) ;; *) echo "DESK_VAULT is not an absolute path" >&2; exit 1;; esac; \
	case "$$python$$HOME$$DESK_VAULT" in *[\"\`\\\$$]*) echo "unsafe character in an install path" >&2; exit 1;; esac; \
	"$$python" -c 'import cryptography, Crypto, eth_account'; \
	real=$$("$$python" -c 'import sys; print(sys.executable)'); \
	case "$$real" in /*) ;; *) echo "python3 did not report an absolute interpreter path" >&2; exit 1;; esac; \
	case "$$real" in *[\"\`\\\$$]*) echo "unsafe character in an install path" >&2; exit 1;; esac; \
	launcher="$$HOME/.local/bin/trade"; \
	if { [ -e "$$launcher" ] || [ -L "$$launcher" ]; } && ! grep -qsF '/.local/lib/desk.jar' "$$launcher"; then \
	  echo "$$launcher is not a Desk launcher, so nothing was installed" >&2; exit 1; fi; \
	mkdir -p "$$HOME/.local/bin" "$$HOME/.local/lib"; \
	cp tools/keychain_wallet.py "$$HOME/.local/lib/desk-keychain.py"; \
	chmod 600 "$$HOME/.local/lib/desk-keychain.py"; \
	{ printf '#!/bin/sh\nexport DESK_PYTHON="$${DESK_PYTHON:-%s}"\nexport DESK_KEY_HELPER="$${DESK_KEY_HELPER:-%s}"\n' "$$real" "$$HOME/.local/lib/desk-keychain.py"; \
	if [ -n "$$DESK_VAULT" ]; then printf 'export DESK_VAULT="$${DESK_VAULT:-%s}"\n' "$$DESK_VAULT"; fi; \
	printf 'exec "$${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" -XX:+DisableAttachMechanism -jar "$$HOME/.local/lib/desk.jar" "$$@"\n'; } > "$$launcher.new" || exit 1; \
	chmod +x "$$launcher.new"; \
	mv -f "$$launcher.new" "$$launcher"; \
	if grep -qsF '/.local/lib/desk.jar' "$$HOME/.local/bin/desk"; then rm -f "$$HOME/.local/bin/desk"; \
	  echo "Removed the earlier launcher $$HOME/.local/bin/desk. The command is trade."; fi

fmt:
	$(SCALA_CLI) fmt .
