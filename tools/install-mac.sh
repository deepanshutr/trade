#!/bin/bash
# Installs Desk on macOS. Run it from anywhere: tools/install-mac.sh. A rerun is the upgrade.
#
# Homebrew installs Java 21, Scala CLI and Python. The key helper's libraries, pinned in
# tools/requirements.txt, go into their own Python environment, ~/.local/lib/desk-venv.
# make install then builds the jar and writes the trade launcher into ~/.local/bin, recording
# that Python and Homebrew's Java 21. Last, unless ~/.local/bin is already on PATH, two lines
# at the end of ~/.zshrc put it at the end of PATH, once, so a system command of the same name
# still wins. Any failed step stops the install there.
set -euo pipefail

if [ "$(uname -s)" != Darwin ]; then
  echo "This install is for macOS. docs/workspace.md describes Linux and Windows." >&2
  exit 1
fi
if ! command -v brew >/dev/null 2>&1; then
  echo "Homebrew is needed. Install it from https://brew.sh, then run tools/install-mac.sh again." >&2
  exit 1
fi
cd "$(dirname "$0")/.."

brew install openjdk@21 scala-cli python
java_home=$(brew --prefix openjdk@21)
python=$(brew --prefix)/bin/python3
venv="$HOME/.local/lib/desk-venv"
"$python" -m venv "$venv"
"$venv/bin/python3" -m pip install --quiet --requirement tools/requirements.txt
PATH="$venv/bin:$PATH" make install DESK_JAVA_HOME="$java_home"

case ":$PATH:" in
  *":$HOME/.local/bin:"* | *":$HOME/.local/bin/:"*) ;;
  *)
    marker='# Desk: ~/.local/bin at the end of PATH, for the trade command. Added by tools/install-mac.sh.'
    line='case ":$PATH:" in *":$HOME/.local/bin:"*|*":$HOME/.local/bin/:"*) ;; *) PATH="${PATH:+$PATH:}$HOME/.local/bin" ;; esac'
    if ! grep -qsxF -e "$marker" "$HOME/.zshrc"; then
      printf '\n%s\n%s\n' "$marker" "$line" >> "$HOME/.zshrc"
    fi
    ;;
esac
echo "Desk is installed. Open a new terminal, then type trade."
