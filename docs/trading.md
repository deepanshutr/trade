# Use the trading controls

Run `desk XMR` to view the market. Trading stays disabled unless you start
`desk XMR --trade` with the required account settings. A missing position is shown as
unavailable. Public market data cannot establish that an account is flat.

The screen needs at least 100 columns and 30 rows. The bottom four rows hold the order
controls. A market switch disables those controls until the new market has been written
to the screen and its account and market data are ready.

| Key | Action |
|---|---|
| `1` | Hard small buy |
| `2` | Hard big buy |
| `3` | Hard small sell |
| `4` | Hard big sell |
| `5` | Best small buy |
| `6` | Best big buy |
| `7` | Best small sell |
| `8` | Best big sell |
| `9` | Select a Target buy price |
| `0` | Select a Target sell price |
| Up and down arrows | Raise or lower the selected target price by one valid step |
| Enter | Submit the selected target |
| Escape | Cancel target selection or the market prompt |
| `m` | Open the market prompt and cancel target selection |
| `c` | Reserved for the coming candle chart control |
| Up and down arrows without a target | Zoom in or out |
| Left and right arrows without a target | Move backward or forward through the chart |
| `q` or Control with `c` | Quit |

Hard and Best keys are ignored while a target is selected. Left and right arrows do not
change a target. Market prompt text has no trading meaning. Complete bracketed paste is
ignored, including numeric text. A terminal which sends plain pasted bytes cannot be
distinguished from typed keys. Check actual paste behaviour before enabling trading there.
Control with `c` still quits inside an unfinished paste or terminal response.
Escape sequences with intermediate bytes and mouse reports are consumed completely.
Some older mouse-report bytes can also form a UTF-8 character. In that ambiguous case,
the guard can consume a later typed key while finishing the report.
After submitting or cancelling a market prompt, orders stay blocked until that prompt's
state change has been applied. Enter alone does not release orders against an older screen.

The default settings file is `~/.config/desk/config.json`. If that file is absent, viewing
uses the defaults. `--config /path/to/config.json` selects another file. Create a private
regular file manually and set its permissions with `chmod 600 ~/.config/desk/config.json`.
This viewing example contains no account addresses:

    {
      "small": "500",
      "big": "1000",
      "target": "1000",
      "network": "mainnet",
      "references": ["BTC", "PAXG"]
    }

Dollar amounts and optional age limits must be quoted numeric strings. The small and big
amounts are total order value before fees, not margin. Target has its own dollar ceiling.
The defaults are $500, $1000 and $1000. Orders require five times leverage. An opposite
fill reduces an existing position before any remainder opens the other direction.

Hard uses the configured dollar size and a price limit no more than 0.1 percent from the
captured best quote. Best chooses a smaller size with a price limit strictly inside that
range. The displayed Best amounts follow the visible book. Target takes available volume
through the selected limit, subject to its dollar ceiling. Visible depth is limited and
an estimate does not promise that every order at the venue has been seen.

To enable trading, add the `owner` public address and its approved delegated `agent` public
address to the settings. Keep the signing key out of that file. The protected password
item uses the `keyService` setting, which defaults to `desk`. Manually create a generic
password item in the system key store with service `desk`, account equal to the `agent`
address, and the private hexadecimal signing key as its password. Set this up through the
protected store, never by placing the key in a command line or a file. The protected store
may ask for access. A denied or timed out read leaves trading disabled.

Use `network` set to `testnet` for the test network. The public feeds and trading account
use the same selected network. The configured owner must own the approved delegated
signing address on that network. This guide does not create an account, grant approval,
fund a position or submit an order.

The current account position appears beside Hard. A pending press or selected Target has
a separate position estimate beside Best. A press estimate was captured when the key was
pressed and may differ from a later account position. It is labelled `press estimate`.
A selected Target is labelled `target estimate`. Neither appears for an unknown position.

The third row shows requested tokens, estimated filled tokens, gross dollars and average
while a Target is selected or a press is pending. The last row is reserved for readiness
and the worker message, including rejection, actual fill or recovery. Once a press resolves,
its old estimate disappears. The confirmed account position and outcome remain visible.
A new Target can show a preview while that outcome stays on the last row.

A leading `~` marks a rounded display number. Calculations keep full precision. Preview
figures are before fees and are not confirmed fills. An immediate order may fill partly
or not at all as prices and available volume change.

Local order state is stored under `~/.local/state/desk`, separated by network and delegated
signing address. One process holds the corresponding state lock. An uncertain order blocks
another submission until a matching final status is found. Do not delete or reset that
state blindly to make the controls available. Check the account order history and resolve
the existing order first.

Readiness also needs recent account data. The account stream does not promise a steady
update rate, so a quiet account can become unavailable even while market quotes continue.
The status row explains the current block. A pending order is never resent to improve its
price or work around a slow reply.

After installing a new build, restart any already running `desk` process. It keeps the
previous program until restarted.
