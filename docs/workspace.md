# Trading workspace

Run `desk XMR --prop` for prop trading on Monero, `desk XMR` for viewing, or
`desk XMR --trade` for the configured real account. Prop trading simulates fills
in a local wallet. Press `s` for settings: sizes, slippage, the trading key, the
owner and its approval, funding and key bindings. No command-line flag is needed
after install. See [Settings](#settings) and [live trading settings](trading.md).
Read the mode and full market name above the trade buttons. The minimum size is 100
columns by 30 rows. Use 120 by 40 for more chart space.

Choose an amount, then click a trade button once to submit it. Each button covers
five rows and repeats the selected amount. There is no separate Send step.
Mouse submission requires a press and release on the same currently visible
button. A changed market, mode, size or terminal geometry invalidates old controls.
So does a position change that changes a button's label.
Switching between prop and real trading still needs an explicit confirmation.

In prop trading, the right side shows the token's current dollar value,
spendable stables, open profit and total net profit. Open profit covers the tokens
still held at their current price, excluding past sales and fees. Buy spends stables plus the fee.
Sell returns stables minus the fee and sells at most the tokens held. A sell that
would leave less than ten dollars of tokens at the quote sells all of them. Net profit
equals stables plus the current token value minus the starting balance.
For real trading, the token value is signed position exposure and the stable
figure depends on the venue's account mode. Unknown or stale values show `--`.
Real open profit comes from the venue. Real lifetime net profit is currently unavailable.
The live heading shows the observed leverage and the current position direction.
Real orders on every market require observed leverage of five times.
Entry and liquidation prices use the graph's existing price scale. Off-screen
prices have an arrow. Prop trading has no liquidation price.

In real trading, a button amount is the money put up. The position value is the
amount times the observed leverage, so at 5x `$20` means a $100 position. The two
trade buttons keep their place and keys, and their labels follow the position. Each
label starts with its key. `x5` is the observed leverage. It is missing while the
leverage is unknown.

| Position | Key 1 or 2 | Key 3 or 4 |
| --- | --- | --- |
| Flat or unknown | `LONG $20 x5` | `SHORT $20 x5` |
| Long | `LONG $20 x5` | `REDUCE $20 x5` |
| Short | `REDUCE $20 x5` | `SHORT $20 x5` |

A key acts on the position at the moment of the press, while a click acts only if
the button still shows the same label.

REDUCE only shrinks the position and stops at flat. It never opens the other side.
To turn a long into a short, press CLOSE, then SHORT. For 2 seconds after a REDUCE or
CLOSE finishes, real trading refuses any order that would open or grow a position,
with the notice "Opening paused for 2 s after closing".

CLOSE sits on the size row after the two amounts. Press `x` or click it to exit the
whole position. Its label names the position, for example `x Close 0.2 XMR`. On a
narrow row the label drops the market, then the quantity. It is enabled only in real
trading with ready account data and a fresh nonzero position, while no order is
pending or saved, no mode confirmation or market prompt is open, no market change is
pending, and the heading fits the row. Otherwise it reads `x Close` and is dimmed.
With ready account data and no position, `x` shows "Not sent: controls changed". A
close that fills in part leaves the rest open. Press CLOSE again. Prop trading keeps
Buy and Sell, and CLOSE stays disabled there for now. In prop or view mode `x` does
nothing. See [live trading settings](trading.md) for amounts and the price allowance.

| Input | Action |
| --- | --- |
| 1 or 3 with the small amount selected | Buy or sell that amount immediately |
| 2 or 4 with the large amount selected | Buy or sell that amount immediately |
| x | Close the whole real position immediately |
| p or l | Open the prop or real mode confirmation |
| Enter with a mode confirmation | Confirm the displayed mode |
| Escape | Cancel mode confirmation or market entry |
| m | Change market |
| c | Switch line and candle charts |
| Arrows | Zoom or move through chart history |
| v | Mute or resume price sounds |
| s | Open settings, except s remains text during market entry |
| q or Control with c | Quit, except q remains text during market entry |

These are the default keys. Each one except Enter, Escape, the arrows and Control C
can be rebound in settings. Buttons and labels show the current key.

The graph retains its three price-aligned columns. From left to right they show
Short, Mid and Long levels. Book, tape, bitcoin and gold comparisons use equal
widths and four distinct colors. Each correlation marker runs from -1 through
zero to +1 and includes its observed sample count. Missing or incomplete data
stays unavailable. Read the calculation in [outlook and alerts](outlook-and-alerts.md).
Ask, text analysis, Details and news have been removed from the normal workspace.

Returning to a market restores its retained chart history immediately, clears its
old quote and depth, and requests full replacement history. Replies started before
the switch cannot overwrite the new selection. Up to 32 selected markets retain
bounded chart history. A configured pool market remains view-only and has no
history before observation began. See [market settings](trading.md).

Prop trading starts with $10,000 and a 0.05 percent fee per fill. It uses
fresh received depth, permits partial fills, and saves the account before
reporting success. A newer quote can reduce known quantity without extending
the age of the full depth. Delayed older depth cannot restore that quantity.
Prices normally expire after one second. Fresh backup depth
can support orders during stream recovery. Delayed or wrong-market replies cannot
restore readiness. A slow save shows Saving with elapsed seconds and blocks another
order until the result is known. A failed save preserves the last confirmed account.

The prop wallet holds one market at a time. Cash, fees, realized results and
held tokens survive restarts. The latest 1,000 fills and lifetime count are retained.
Selling cannot borrow tokens or open a short position. Funding and liquidation
are excluded. The new wallet starts at the configured prop balance and saves
to `prop/wallet-mainnet.json` or `prop/wallet-testnet.json`. The old `paper/` folder,
with its wallets and old margin simulation files, stays untouched and is never
read again. Later settings changes do not reset an existing wallet.
The wallet saves its starting deposit so net profit follows current wallet value.
Incomplete saved wallets are refused and left untouched.
Prop and real accounts remain separate, and prop mode does not poll the
real account. No fill, partial fill, refusals and repeated results appear in one
short status row, with a separate action number for each result.

Local state defaults to `~/.local/state/desk`. Change it with `--state /absolute/path`.
Message history is no longer kept. Existing message files are left untouched.
The order result log keeps the most recent 1000 prop and live results.
Order outcomes alone are appended to `order-results.jsonl` for replay, with a separate
`order-results.lock` held during each append. Two view-only desks may run at once. Account files, public data, diagnostics and
live recovery state use separate locks. Unreadable saved state is preserved.
Public recording retains up to seven days or one GB. Gaps remain gaps. Run `desk XMR --record` for the
background public recorder. Inspect its Mac service with `python3 tools/recorder.py`.

The interactive process writes private diagnostic records under `diagnostics/`
in the chosen state directory, even when a separate process owns public recording.
Four files retain at most 8 MiB. These records include validated market selections,
history counts, quote age, disconnects, slow input and frames, and fixed failure
categories. They exclude typed text, account data, orders and raw error messages.
Records written before the paper mode became prop name it `simulated`.
Later records name it `prop`.
Logging uses a bounded background queue. `!logs` marks unavailable diagnostic
storage. See [logging details](diagnostics.md).

Restart an already running Desk after installation to load the new build.

## Settings

Press `s` to open settings. The workspace stops drawing while settings are open, but
the market feed, the workers, alerts and sounds keep running. An order already sent
completes, and its outcome shows when you leave. Settings close any open mode
confirmation. During market entry `s` is text. Esc leaves settings. Control C quits
Desk, as everywhere.

The page has three sections: Trading, Keys and Key bindings. Up and Down move the
cursor. Enter edits or acts on the row under it. `r` reads the settings, the key and the
owner again. While a live order is pending, `r` reads only the owner and says "Wait for
the pending order". The last line shows the latest result or refusal and clears on the
next key. On a terminal smaller than 100 by 30, lines are cut at the width and the page
scrolls to keep the cursor visible.

Enter on Small amount, Big amount or Slippage opens a field. It takes digits and one
dot. Enter saves and Esc cancels. Slippage is shown and typed in percent. A refused
value stays in the field with the reason. A saved value is written to the settings
file at once and applies to the next order. An order already reviewed still sends
what it showed. The Keys section is described in [live trading settings](trading.md).

Enter on a key binding row records a new key:

1. Press the key, or the first of two keys. Esc cancels.
2. Press a second key to make a two-key sequence, or Enter to keep one key.
3. Press the same again to confirm. A mismatch starts again.

Desk then checks the binding, writes it and applies it at once. A refusal shows the
reason and starts again. A binding is one key or two keys pressed one after the other.
The second key must follow within one second. Otherwise the first is dropped and the
second acts on its own. Shift needs no rule: a capital letter or a shifted symbol is
its own key. Control combinations work. Command combinations do not.

Option combinations work with the Apple Terminal defaults, where Option types a
character such as `∫` for Option B. Desk stores the character's bytes. Option
characters depend on the keyboard layout, so a layout change can break an Option
binding. With "Use Option as Meta key" turned on, an Option key is not a key for
Desk. On the US layout Option E, I, N, U and the backtick are dead keys that wait for
a second press. Desk refuses their accents and the accented letters they make, with
"That key needs a second press on this layout".

Desk refuses Enter and Control J, Escape, Tab, the arrows, Delete and Control H,
Control C, Control D, Control Q, Control S, Control Z, Control backslash, Control
Space, the paste markers and mouse reports, and anything that is not one key. A binding
equal to another is refused, and so is a key that starts another action's two-key
sequence. Two sequences may share their first key.

A row shows each binding by name: the key itself, `Space`, `Control K`, an Option
character with its US Option key such as `∫ Option B`, `F1` to `F12`, or the bytes in
hexadecimal. A two-key binding shows as `g then m`. Buttons use a short form, such as
`^K` or `g m`.

Bindings live in `~/.config/desk/bindings.json`, mode `600`, at most 16 KiB:

```json
{"version":1,"bindings":{"quit":["q"],"market":["g","m"],"close":["∫"]}}
```

Each key is the exact bytes the terminal sends, as a JSON string. An action that is
missing keeps its default. A hand edit that breaks any rule stops Desk at start with
"invalid key bindings: REASON (bindings.json)". Settings check every binding before
writing, so only a hand edit can cause that.

Settings and bindings writes need the folder `~/.config/desk` to be private. A folder
that the group or others can read, such as one made by hand with mode 755, refuses
every write with "local storage must be private"
and the folder's path, and nothing changes on disk. Run `chmod 700` on that folder once.

## Development

Install Scala CLI, `python3` and `expect` to run the checks. `make test` runs the
offline Scala suite excluding `slow` tests, and `make test-python`, which discovers
`test/test_*.py`. Scala test targets disable the build server.
The key-helper tests skip with a reason when `cryptography`, `pycryptodome`
(`Crypto`) or `eth-account` is missing. They use fixture keys and fake stores,
plus native access objects on macOS, without creating or reading keychain items.
`make live` runs tests tagged `live` against public endpoints.
`WRITE_GOLDEN=1` is the historical golden-output write variable. The current
screen tests have no golden-file writer, so setting it does not update fixtures.
