# Desk: ~/.local/bin at the end of PATH, for the trade command. Added by make install.
case ":$PATH:" in *":$HOME/.local/bin:"*|*":$HOME/.local/bin/:"*) ;; *) PATH="${PATH:+$PATH:}$HOME/.local/bin" ;; esac
