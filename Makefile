SCALA_CLI ?= scala-cli
.PHONY: java-home test test-python live slow check build windows-image install install-launcher fmt

# DESK_JAVA_HOME, when given, is the absolute folder of the Java that builds, tests and runs Desk.
# The shell reads it, as it reads HOME. Nothing runs or is written until it is absolute, safe to
# quote and holds an executable bin/java. On Linux, where uname -s prints Linux, it must be
# given, so no Java is guessed or fetched. Elsewhere an empty value passes, so the Mac and Git Bash
# on Windows build as before. A failed uname stops the step, so an unknown system never passes
# as a Mac.
# Every scala-cli call that compiles or runs gets it, and the build server stays off.
# make prints nothing for this step, so the text each target prints stays as it was.
java-home:
	@system=$$(uname -s) || { echo "uname -s failed, so the system is unknown" >&2; exit 1; }; \
	case "$$DESK_JAVA_HOME" in ""|/*) ;; *) echo "DESK_JAVA_HOME is not an absolute path" >&2; exit 1;; esac; \
	case "$$DESK_JAVA_HOME" in *[\"\`\\\$$}]*) echo "unsafe character in DESK_JAVA_HOME" >&2; exit 1;; esac; \
	[ -n "$$DESK_JAVA_HOME" ] || [ "$$system" != Linux ] || \
	{ echo "On Linux, DESK_JAVA_HOME must name the Java that builds and runs Desk" >&2; exit 1; }; \
	[ -z "$$DESK_JAVA_HOME" ] || { [ -f "$$DESK_JAVA_HOME/bin/java" ] && [ -x "$$DESK_JAVA_HOME/bin/java" ]; } || \
	{ echo "DESK_JAVA_HOME holds no executable bin/java" >&2; exit 1; }

test: java-home test-python
	$(SCALA_CLI) --power test . --server=false $${DESK_JAVA_HOME:+--java-home "$$DESK_JAVA_HOME"} -- --exclude-tags=live,slow

test-python:
	python3 -m unittest discover -s test -p 'test_*.py'

live: java-home
	$(SCALA_CLI) --power test . --server=false $${DESK_JAVA_HOME:+--java-home "$$DESK_JAVA_HOME"} -- --include-tags=live

slow: java-home
	$(SCALA_CLI) --power test . --server=false $${DESK_JAVA_HOME:+--java-home "$$DESK_JAVA_HOME"} -- --include-tags=slow

check: java-home
	$(SCALA_CLI) compile --test . --server=false $${DESK_JAVA_HOME:+--java-home "$$DESK_JAVA_HOME"} && $(SCALA_CLI) fmt --check .

build: java-home
	mkdir -p bin
	$(SCALA_CLI) --power package . -o bin/desk.jar --assembly --preamble=false -f --server=false $${DESK_JAVA_HOME:+--java-home "$$DESK_JAVA_HOME"}

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

# The jar is copied beside the old one and renamed over it, so a running Desk keeps its old bytes.
install: build install-launcher
	cp bin/desk.jar "$$HOME/.local/lib/desk.jar.new"
	mv -f "$$HOME/.local/lib/desk.jar.new" "$$HOME/.local/lib/desk.jar"

# Exits the recipe line with 0 unless uname -s prints Linux and no line of ~/.bashrc equals the
# first line of tools/bashrc-path.sh. A failed read stops it under set -e.
BASHRC_NEEDS_LINES = system=$$(uname -s); [ "$$system" = Linux ] || exit 0; \
	marker=$$(head -n 1 tools/bashrc-path.sh); ! grep -qsxF -e "$$marker" "$$HOME/.bashrc" || exit 0

# The shell reads HOME so an odd character in it cannot change the recipe.
# Nothing is written until python3 is found, its libraries import and the interpreter path
# that python reports for itself is absolute and safe to quote. That path is the one recorded.
# Safe to quote means no quote, backtick, backslash, dollar sign or closing brace, since each
# recorded value sits inside a quoted ${NAME:-value} in the launcher.
# The launcher starts Java with attach disabled so other programs cannot read its memory.
# Its Java is DESK_JAVA_HOME's bin/java, which the java-home step checked, else Homebrew's.
# DESK_VAULT, when given, is the absolute folder of the owner vault. It is checked the same way
# before anything is written and recorded next to the other two.
# The launcher is trade on every system, because Windows opens Display settings for a bare desk.
# In ~/.local/bin the install replaces trade or removes desk only when its text names the Desk
# jar. A trade that does not stops the install before anything is written. A desk that does,
# from an earlier install, is removed after the new launcher is in place, so the old name
# cannot start a stale launcher.
# The launcher is written to trade.new and moved onto trade, so a link named trade is replaced.
# On Linux, where uname -s prints Linux, the install then appends a newline and the two lines of
# tools/bashrc-path.sh to ~/.bashrc, unless a line there already equals the file's first line.
# So each new interactive bash puts ~/.local/bin at the end of PATH, once. uname and that first
# line are read before anything is written. When the lines are due, ~/.bashrc must then be a
# file that can be read and written, or be missing from a home that can be written. A folder,
# a read-only file, a file that cannot be read, which would hide the lines and get them again,
# or a link to a missing file stops the install before anything is written, so a new launcher
# never sits beside the old jar. These ~/.bashrc steps are silent recipe lines of
# their own, so make prints the same recipe text on the Mac as before.
# macOS /bin/sh is bash 3.2 by default, which carries on under set -e when a group's redirection
# fails, so the launcher write and the ~/.bashrc append exit by hand. set -e does not apply inside
# a group followed by ||, so the writes in each group are joined by && and any failed one fails
# the group.
install-launcher: java-home
	@set -e; $(BASHRC_NEEDS_LINES); \
	if [ -e "$$HOME/.bashrc" ]; then [ -f "$$HOME/.bashrc" ] && [ -r "$$HOME/.bashrc" ] && [ -w "$$HOME/.bashrc" ]; \
	else [ ! -L "$$HOME/.bashrc" ] && [ -w "$$HOME" ]; fi || \
	{ echo "$$HOME/.bashrc cannot be read or written, so nothing was installed" >&2; exit 1; }
	set -e; \
	python=$$(command -v python3) || { echo "python3 not found" >&2; exit 1; }; \
	case "$$DESK_VAULT" in ""|/*) ;; *) echo "DESK_VAULT is not an absolute path" >&2; exit 1;; esac; \
	case "$$python$$HOME$$DESK_VAULT" in *[\"\`\\\$$}]*) echo "unsafe character in an install path" >&2; exit 1;; esac; \
	"$$python" -c 'import cryptography, Crypto, eth_account'; \
	real=$$("$$python" -c 'import sys; print(sys.executable)'); \
	case "$$real" in /*) ;; *) echo "python3 did not report an absolute interpreter path" >&2; exit 1;; esac; \
	case "$$real" in *[\"\`\\\$$}]*) echo "unsafe character in an install path" >&2; exit 1;; esac; \
	launcher="$$HOME/.local/bin/trade"; \
	if { [ -e "$$launcher" ] || [ -L "$$launcher" ]; } && ! grep -qsF '/.local/lib/desk.jar' "$$launcher"; then \
	  echo "$$launcher is not a Desk launcher, so nothing was installed" >&2; exit 1; fi; \
	mkdir -p "$$HOME/.local/bin" "$$HOME/.local/lib"; \
	cp tools/keychain_wallet.py "$$HOME/.local/lib/desk-keychain.py"; \
	chmod 600 "$$HOME/.local/lib/desk-keychain.py"; \
	{ printf '#!/bin/sh\nexport DESK_PYTHON="$${DESK_PYTHON:-%s}"\nexport DESK_KEY_HELPER="$${DESK_KEY_HELPER:-%s}"\n' "$$real" "$$HOME/.local/lib/desk-keychain.py" && \
	if [ -n "$$DESK_VAULT" ]; then printf 'export DESK_VAULT="$${DESK_VAULT:-%s}"\n' "$$DESK_VAULT"; fi && \
	printf 'exec "$${DESK_JAVA:-%s/bin/java}" -XX:+DisableAttachMechanism -jar "$$HOME/.local/lib/desk.jar" "$$@"\n' \
	"$${DESK_JAVA_HOME:-/opt/homebrew/opt/openjdk}"; } > "$$launcher.new" || exit 1; \
	chmod +x "$$launcher.new"; \
	mv -f "$$launcher.new" "$$launcher"; \
	if grep -qsF '/.local/lib/desk.jar' "$$HOME/.local/bin/desk"; then rm -f "$$HOME/.local/bin/desk"; \
	  echo "Removed the earlier launcher $$HOME/.local/bin/desk. The command is trade."; fi
	@set -e; $(BASHRC_NEEDS_LINES); \
	{ echo && cat tools/bashrc-path.sh; } >> "$$HOME/.bashrc" || exit 1; \
	echo "Added ~/.local/bin to the end of PATH in ~/.bashrc. Open a new terminal, then type trade."

fmt:
	$(SCALA_CLI) fmt .
