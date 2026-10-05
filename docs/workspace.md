# Trading workspace

Run `desk XMR --prop` for prop trading on Monero, `desk XMR` for viewing, or
`desk XMR --trade` for the configured real account. Prop trading simulates fills
in a local wallet. Press `s` for settings: sizes, slippage, the trading key, the
owner and its approval, funding and key bindings. No command-line flag is needed
after install. See [Settings](#settings) and [live trading settings](trading.md).
The panel's first row names the mode, the market, the leverage and the position's
direction. The minimum size is 100 columns by 30 rows. Use 120 by 40 for more chart space.

## One screen

Live, prop and view draw the same screen and behave the same. Only placing orders
differs. View has no account. It draws no position, order, ticket or fill on the
price axis or the chart, and its order controls are faint. Its panel reads
`VIEW XMR --`, `--` for every figure and `View only: p Prop or l Live to trade`.
Every order key in view answers with that sentence. Prop's heading reads
`PROP XMR 1x`, since prop trades at one times leverage for now, and its third row
shows the prop balance as `Prop`.

| Region | 100 by 30 | 120 by 40 |
| --- | --- | --- |
| Price axis and depth, left of the chart | 38 columns | 38 columns |
| Chart | 23 cells | 43 cells |
| Price rows | 9 | 19 |
| Outlook, right of the chart | 38 columns | 38 columns |
| Panel, bottom left | 38 by 12 | 38 by 12 |
| Gauges, bottom right | 61 by 12 | 81 by 12 |

Under the price rows sit the volume row, three rows each for the bitcoin and gold
references, and the time axis. On BTC and on gold the chart has three more price
rows, since a market is not its own reference. The panel used to sit at the bottom
right. It now sits at the bottom left, under the price axis. The gauges moved to the
bottom right, and the price scale at the right of the chart is gone.

The header starts with the market name in bold. The mid, mark, oracle, 24 hour
change, funding, open interest and volume follow while whole fields fit. A faint
`ping 0.3s` ends the chart's first row. It counts the seconds since the last market
message. It reads `ping stale` after 5 seconds without one and `ping down` while the
connection is down.

### The price axis

Each price row starts with 38 columns: the amount in 7, a space, the price in 8, a
marker column and the depth bar in 21. Asks draw their depth above the current price
in red, bids at and below it in green. Prices use the market's tick decimals, so XMR
reads `546.09`, BTC reads `99000` and a market whose tick is a millionth reads
`0.012345`. A tick is the smallest price step the venue allows at that price.

The axis also lists Desk's orders on this market. Each kind of row has its colour:

| Row | Amount | Price | Colour |
| --- | --- | --- | --- |
| Current price | blank | the mid, then `>` | text colour, bold |
| Entry | money the position puts up | the entry price | the position's side colour, bold |
| Buy orders at one price | money put up, summed | the price | green |
| Sell orders at one price | money put up, summed | the price | red |
| Ticket | money the ticket puts up | the ticket's price | the ticket's side colour, bold |
| Liquidation | blank | the position's liquidation price | the colour of the side that closes the position: red for a long, green for a short |
| Selected row | as its kind | as its kind | inverse and bold |
| Other rows | blank | the middle price of every third row and of the lowest | faint |

The current price always sits on its own row. Other marks move around it when they
would share a row. A mark shown above its true row carries `'` after its price, and
one shown below carries `,`. The range always holds every marked price, so no order
goes out of view. It widens so an order just beyond the current price keeps a row.
Marks are packed back from an edge, so none is left off while its side has a free
row. When a side has more prices than rows, the farthest are left off and the status
row reads `2 more above` or `2 more below`. An amount is faint while the account data
is stale, or while a cancel of that row runs.

The live position's liquidation price sits on the axis too, but it never widens the
range. Beyond the range it sits on the nearest edge row, with `'` below the range or `,`
above it. It takes a row only where the orders leave one, so it never pushes an order
off the axis. The cursor never stops on it. Prop has no liquidation price.

### The chart and the time axis

Desk's own fills on this market show in the chart at their time and price, `▲` in
bold green for a buy and `▼` in bold red for a sell. Of two fills in one cell the
larger in dollars shows, the later one on a tie. A fill timed after now shows in the
last column. A fill Desk learns of only from a status lookup, such as after a lost
connection, carries no time and draws no mark. Fill marks are kept in memory, at most 200 for each market, and clear
when Desk restarts. Live's marks also clear whenever its worker stops: on a switch to
prop, before a settings key or owner step, and when the owner or the key changes.
Prop's marks stay through settings. While a ticket is open, its price row shows a
dotted `⠒` line in the ticket's side colour across the empty chart cells. Nothing else
is drawn across the chart.

The row under the chart starts with its caption, such as `c line 2s/col`, right
aligned in the left 38 columns. It names the chart type and the time each cell
covers, and adds `history` while you look back. Across the chart a `|` marks a round
local time, 30 seconds apart at 2 seconds a cell. The step is the smallest of 5s, 10s,
15s, 30s, 1m, 5m, 15m, 1h, 4h and 1d that keeps the marks at least fourteen cells
apart. Its time follows the mark when nine cells remain to the right edge. A day mark
shows the date.

### The panel

The panel has twelve rows of 38 columns, each field in a fixed place:

```
LIVE XMR 5x LONG
Val  $109.22 Open   $0.44 Net      --
Stables $1000.00  Usable  $880.00
Filled $21.84 #7
[$20 *] [$100] [x Close $21.84]
[t Limit] [o Orders] v Sound  1 on BTC
[1 LONG $20 x5] [3 REDUCE $20 x5]
SELL LIMIT XMR reduce only
Price 549.00  ask 546.10 +290t +0.53%
Size $10.98 of $21.84 held  fee $0.01
[Enter Place] [Esc Cancel] step 0.01
[p Prop] [l Live] m Market c Chart
```

| Row | Holds |
| --- | --- |
| 1 | The mode in bold, the market, the leverage and the direction in its side's colour |
| 2 | The position value, the open profit and the net profit |
| 3 | The stables and the usable margin, or the prop balance and its usable cash |
| 4 | The status row |
| 5 | The two sizes and CLOSE |
| 6 | The order type, the orders on the axis, the sound and Desk's resting orders on other markets, such as `1 on BTC` |
| 7 | The two trade buttons |
| 8 to 11 | The ticket or the mode confirmation. Otherwise a long status wraps onto them |
| 12 | The mode keys, the market key and the chart key |

A long key name on row 6 drops the count first, then the sound.

The status row names Desk's own outcomes with their dollars and a result number:

| Outcome | Status row |
| --- | --- |
| `XMR filled $21.84 at 546.09` | `Filled $21.84 #7` |
| `XMR part filled $5.46 of $21.84 at 546.09` | `Part filled $5.46 #8` |
| `XMR resting $19.99 at 546.09` | `Resting $19.99 #9` |
| `XMR canceled $16.38 at 546.09`, or a cancel by the venue | `Canceled $16.38 #10` |
| `XMR canceled before sending` | `Not sent #11` |
| `XMR canceling 1 of 3`, while a cancel runs | `Canceling` |
| `XMR cancel rejected` | `Cancel refused #12` |
| `XMR order rejected. Resting reduce orders may block it, press o` | `Close refused. Press o for orders #13` |

A prop market fill reads `Bought $20.00 #14` or `Sold $20.00 #15`, the money it put up
at the fill price, and `Part filled` when the depth held less. Its full text, such as
`PROP XMR buy filled $20.00 at 546.09` with the fee after it, goes to the order result
log.

Every size on screen is the money put up: the quantity times the price divided by
the leverage, rounded half up to cents. That covers the axis amounts, CLOSE, the
ticket, the status row and the buttons. The position value at full size shows in one
place, `Val`. An amount can read a few cents under its button, since the quantity is
rounded down to the smallest size the market allows. So a `$20` press can show
`$19.99`. Amounts read `--` until the first account read gives the leverage.

Buy, long, bids and buy fills use the terminal's own green. Sell, short, asks and
sell fills use its red. The entry and the ticket take their side's colour in bold.
The market name and the current price use the text colour in bold. Bitcoin is
orange, gold is dark gold, book readings are green and tape readings are blue. Rails,
scales and quiet labels are faint. Every fixed colour keeps at least 3 to 1 contrast
on Solarized light, Solarized dark, white and black.

## Market and limit orders

`t` switches the order type between market and limit. It starts as market every
session and stays the same when you switch between live and prop. Row 6 shows it as
`[t Market]` or `[t Limit]`. The button is faint while an order is pending, a ticket
is open or a mode confirmation shows, and `t` then answers as a click on it does,
without a switch.

In market mode a trade key or a click on a trade button sends the order at once.
There is no separate Send step. Mouse submission requires a press and release on the
same currently visible button. A changed market, mode, size or terminal geometry
invalidates old controls. So does a position change that changes a button's label.
Switching between prop and real trading still needs an explicit confirmation.

In limit mode a trade key or button opens a ticket on rows 8 to 11 instead of
sending. A limit order is post only. It rests on the book and never trades at once.
See [limit orders](trading.md#limit-orders) for the venue rules.

| Ticket row | Shows |
| --- | --- |
| 8 | The side in its colour, `LIMIT`, the market and `post only` or `reduce only` |
| 9 | The price, the best price on the order's own side, the distance in ticks such as `+290t`, and the distance in percent |
| 10 | The money put up, `of $21.84 held` for a reduce, and the fee at the venue's base maker rate |
| 11 | The two buttons and the step of the last move |

When the preview refuses the order, row 10 shows the reason in place of the size. A
ticket row too wide for 38 columns drops its last field first.

The ticket starts at the best bid for a buy and the best ask for a sell. Up and Down
move the price by 0.01, Left and Right by 0.1. Each move lands on the nearest valid
price and moves at least one tick. Row 11 shows the real step, so on BTC above 1000
dollars both pairs move by 1. When no further price exists, the price stays and the
status row reads `no further step`. While a ticket is open the arrows move only its
price, and the trade buttons are off.

Enter places the order. Desk previews it again first and sends only when the fresh
preview is the same order the ticket shows. Otherwise the ticket shows the new order,
the status row reads `Order changed. Enter again to send`, and the next Enter sends
it once the screen shows it. A price at or through the other side is refused with
`Takes liquidity. Move below the ask` or `Takes liquidity. Move above the bid`. A
refusal keeps the ticket open with its reason. Once the order is on its way the
ticket closes, the status row reads `Sending` and the arrows move the chart again.
Esc closes the ticket and sends nothing. A market change, the market prompt, a mode
confirmation, settings, and new sizes or key bindings close it too.

## Orders on the price axis

The price axis is also the list of Desk's orders on this market. Resting orders on
other markets show only as the count on row 6. Switch market to act on them. A market
change cancels nothing.

`o` moves the cursor to the order row nearest the current price. Without an order
row it answers `No orders on XMR`. `o` again, or Esc, leaves the axis. While a ticket
is open `o` does nothing. Up and Down move the cursor between the order rows and the
entry row, and Left and Right still move the chart. The selected row is inverse and
bold. Other keys keep working while the cursor is on the axis. The cursor leaves by
itself when no order row remains, and when a ticket, a mode confirmation or the
market prompt opens. When its row goes, it moves to the nearest order row. The mouse
does not reach the axis rows.

| Key on the axis | Action |
| --- | --- |
| Enter on an order row | Cancel every Desk order at that side and price |
| Enter on the entry row | Exactly what `x` does |
| `a` on an order row | Open a ticket at that row's side and price, in either order type |
| `a` on the entry row | Nothing |

While the cursor is on an order row, the status row says what a fill of that row
would do now: `opens`, `reduce`, `flip $5.44 XMR if filled` or `void`. See
[both sides](trading.md#both-sides).

Keys on the axis act only on the rows the screen showed. When the orders change after
a screen was drawn, the keys wait for the next one. A row whose orders have all gone
answers `Nothing to cancel at that price`. Enter and Esc go first to the market
prompt, then the ticket, then the mode confirmation, then the axis.

## Trade buttons and CLOSE

In prop trading, the panel shows the token's current dollar value as `Val`, open
profit, total net profit, the prop balance and `Usable`. Usable is the balance left
after the value and maker fee of every resting buy. Open profit covers the tokens
still held at their current price, excluding past sales and fees. Buy spends stables plus the fee.
Sell returns stables minus the fee and sells at most the tokens held. A sell that
would leave less than ten dollars of tokens at the quote sells all of them. Net profit
equals stables plus the current token value minus the starting balance.
For real trading, the token value is signed position exposure and the stable
figure depends on the venue's account mode. Unknown or stale values show `--`.
Real open profit comes from the venue. Real lifetime net profit is currently unavailable.
The live heading shows the observed leverage and the current position direction.
Real orders on every market require observed leverage of five times.

In real trading, a button amount is the money put up. The position value is the
amount times the observed leverage, so at 5x `$20` means a $100 position. The two
trade buttons keep their place and keys, and their labels follow the position. Each
label starts with its key. `x5` is the observed leverage. It is missing while the
leverage is unknown. Prop's buttons read `x1`.

| Position | Key 1 or 2 | Key 3 or 4 |
| --- | --- | --- |
| Flat or unknown | `LONG $20 x5` | `SHORT $20 x5` |
| Long | `LONG $20 x5` | `REDUCE $20 x5` |
| Short | `REDUCE $20 x5` | `SHORT $20 x5` |

A key acts on the position at the moment of the press, while a click acts only if
the button still shows the same label.

REDUCE only shrinks the position and stops at flat. It never opens the other side.
To turn a long into a short, press CLOSE, then SHORT. For 2 seconds after a REDUCE or
CLOSE finishes, or after a resting order fills and shrinks the position, real trading
refuses any order that would open or grow a position, with the notice "Opening paused
for 2 s after closing". The pause stops only new sends. An order already resting fills
whenever the market reaches it. Prop pauses only after a CLOSE or a resting sell fill.

CLOSE sits on row 5 after the two amounts. Press `x`, click it, or press Enter on the
entry row of the axis to exit the whole position at once. Its label names the money
the position puts up, for example `x Close $21.84`, or `x Close --` while the leverage
is unknown. On a narrow row the label drops the amount. It is enabled in live and prop
with ready account data and a fresh nonzero position, while no order is pending or
saved, no mode confirmation or market prompt is open, no market change is pending,
and the heading fits the row. Otherwise it reads `x Close` and is faint. With ready
account data and no position, `x` shows "Not sent: controls changed". A close that
fills in part leaves the rest open. Press CLOSE again. In prop, CLOSE sells the whole
holding at once with the prop fee. CLOSE never cancels a resting order. See
[CLOSE and resting orders](trading.md#close-and-resting-orders). In view `x` answers
the view sentence. See [live trading settings](trading.md) for amounts and the price
allowance.

| Input | Action |
| --- | --- |
| 1 or 3 with the small amount selected | Buy or sell that amount at once, or open a ticket in limit mode |
| 2 or 4 with the large amount selected | Buy or sell that amount at once, or open a ticket in limit mode |
| t | Switch between market and limit orders |
| x | Close the whole live or prop position at once |
| o | Move the cursor onto the orders on the price axis, or off them |
| a on an order row | Open a ticket at that row's side and price |
| Up or Down with a ticket open | Move the ticket's price by 0.01, or the nearest valid step |
| Left or Right with a ticket open | Move the ticket's price by 0.1, or the nearest valid step |
| Up or Down on the price axis | Move the cursor between the order rows and the entry row |
| Arrows otherwise | Zoom or move through chart history |
| Enter with a ticket open | Place the order the ticket shows |
| Enter on an order row | Cancel the orders at that price |
| Enter on the entry row | Close the position, as `x` does |
| Enter with a mode confirmation | Confirm the displayed mode |
| Escape | Cancel market entry, the ticket or the mode confirmation, or leave the price axis |
| p or l | Open the prop or real mode confirmation |
| m | Change market |
| c | Switch line and candle charts |
| v | Mute or resume price sounds |
| s | Open settings, except s remains text during market entry |
| q or Control with c | Quit, except q remains text during market entry |

These are the default keys. Each one except Enter, Escape, the arrows, `a` and
Control C can be rebound in settings. Buttons and labels show the current key.

## Outlook, markets and saved state

The graph retains its three price-aligned columns. From left to right they show
Short, Mid and Long levels. Book, tape, bitcoin and gold comparisons use equal
widths and four distinct colors. Each correlation marker runs from -1 through
zero to +1 and includes its observed sample count. Missing or incomplete data
stays unavailable. Read the calculation in [outlook and alerts](outlook-and-alerts.md).
Ask, text analysis, Details and news have been removed from the normal workspace.
The Polymarket reader is in the code but not wired, so it sends no request. Project 5
wires it.

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
Prop limit orders rest in the wallet and fill from the market's trades with the maker
fee. See [prop limit orders](trading.md#prop-limit-orders).

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
confirmation or ticket and take the cursor off the price axis. During market entry `s`
is text. Esc leaves settings. Control C quits
Desk, as everywhere.

The page has three sections: Trading, Keys and Key bindings. Up and Down move the
cursor. Enter edits or acts on the row under it. `r` reads the settings, the key and the
owner again. While a live order is pending, `r` reads only the owner and says "Wait for
the pending order". The last line shows the latest result or refusal and clears on the
next key. On a terminal smaller than 100 by 30, lines are cut at the width and the page
scrolls to keep the cursor visible. Enter, Esc, the arrows and `r` are fixed inside
settings, whatever the workspace binds them to.

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

Desk refuses Enter and Control J, Escape, Tab, the arrows, `a`, Delete and Control H,
Control C, Control D, Control Q, Control S, Control Z, Control backslash, Control
Space, the paste markers and mouse reports, and anything that is not one key. `a`
places at the selected order's price on the price axis. A binding equal to another is
refused, and so is a key that starts another action's two-key sequence. Two sequences
may share their first key.

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

A file written before limit mode still loads. Its order type and orders actions take
`t` and `o`. When the file already binds `t` or `o` to another action, that binding
stays, and order type or orders is left unbound until you set a key. A binding in the
file that uses `a` is dropped, and its action is left unbound. Its row in settings
reads `unbound`.

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
