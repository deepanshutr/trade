# trade

Desk is a trading screen for the terminal. With it you watch a Hyperliquid perpetual
market, practise on a simulated account, or trade your own account. You start it by
typing `trade`.

## Install on a Mac

The install is made for Macs with Apple silicon. You need Homebrew, the Mac package
manager from [brew.sh](https://brew.sh). Then open Terminal and paste:

```sh
git clone https://github.com/deepanshutr/trade.git
cd trade
tools/install-mac.sh
```

Open a new terminal and type `trade XMR` to watch the Monero market.

The first install takes a few minutes. Homebrew gives you Java 21, Scala CLI (the
build tool) and Python. The key helper's Python libraries go into their own folder,
`~/.local/lib/desk-venv`. You get the program as `~/.local/lib/desk.jar` and the
`trade` command in `~/.local/bin`. If that folder is not yet on your PATH, the list of
folders your terminal searches for commands, the install adds it at the end of PATH
in `~/.zshrc`.

To upgrade, run `git pull` in the `trade` folder, then `tools/install-mac.sh` again.

## Use

- `trade XMR` watches a market. You need no account for this.
- `trade XMR --prop` trades a simulated account kept on your computer.
- `trade XMR --trade` trades your real account. Read [live trading](docs/trading.md)
  first.
- Press `s` for settings and `q` to quit.

The [workspace guide](docs/workspace.md) explains the screen, the keys and the install
on Linux and Windows.

## Risk

Live trading places real orders with real money. You use this software at your own
risk.

## License

MIT. See [LICENSE](LICENSE).
