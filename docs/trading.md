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
| `c` | Switch the main and reference charts between lines and candles |
| Up and down arrows without a target | Zoom in or out |
| Left and right arrows without a target | Move backward or forward through the chart |
| `q` or Control with `c` | Quit |

Candle mode starts at one minute. Hollow bodies close above their opening price, filled
bodies close below it, and a flat body closes unchanged. Wicks show the high and low.
Missing candles stay blank. Returning to lines restores the saved line zoom. Each reference
has its own price scale, so their heights do not show equal percentage changes.

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

The idle Hard and Best rows keep their keys in fixed columns as amounts change. The
confirmed account position appears beside Hard. The next row shows the Target selectors
and viewing keys. Connection activity and its age stay at the start of the header, even
while the market prompt is open. An active connection does not establish order readiness.
Selected market depth and account freshness have separate checks in the footer.
A long market label ends in `...`. The footer then keeps the full name, or disables order
keys if that name cannot fit. The prompt keeps its latest input and cursor visible.

Selecting Target uses the same four rows for its market, side, exact limit, requested
tokens, estimated filled tokens, gross dollars and average. Enter says whether it can
send, and Escape cancels selection. Keys 1 through 8 and left and right arrows are visibly
inactive. A failed preview keeps its actual reason. A pending press shows the captured
Hard, Best or Target plan in these rows. Its estimate does not change with later quotes.

Confirmed position stays separate from `target estimate` or `press estimate`. A press
estimate can differ from a later confirmed position. An unknown position stays unavailable
and has no resulting position estimate. A zero estimated fill has no average.

The current blocking reason and last completed outcome remain visible together. A new
permitted press replaces the previous outcome, including when planning rejects that new
attempt. A blocked key or failed account refresh keeps the completed outcome. Resolved
press estimates disappear. A new Target can show a preview beside the retained outcome.
Long exact limits or result labels can use spare space elsewhere in the four rows.
A limit that fills a whole row sits directly below its `limit $` label. If required text
cannot fit, a width message appears and order keys stay disabled until a complete frame
has been written. Optional viewing help yields first. The chart caption says `history`
when scrubbed, while the book and midpoint continue to use current data.

A leading `~` marks a rounded display number. Calculations keep full precision. Very large
exact limits can use scientific notation without that marker. For example, `1e100` is one
followed by one hundred zeros. Preview figures are before fees and are not confirmed fills.
An immediate order may fill partly or not at all as prices and available volume change.

Local order state is stored under `~/.local/state/desk`, separated by network and delegated
signing address. One process holds the corresponding state lock. An uncertain order blocks
another submission until a matching final status is found. Do not delete or reset that
state blindly to make the controls available. Check the account order history and resolve
the existing order first.

Readiness also needs recent account data. The account stream does not promise a steady
update rate, so a quiet account can become unavailable even while market quotes continue.
The footer shows the current blocking reason beside a retained fill, rejection or recovery
message. The outcome names its original market after a market switch. A pending order is
never resent to improve its price or work around a slow reply.

After installing a new build, restart any already running `desk` process. It keeps the
previous program until restarted.
