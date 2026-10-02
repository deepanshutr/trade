# Trading workspace

Run `desk XMR --prop` for prop trading on Monero, `desk XMR` for viewing, or
`desk XMR --trade` for the configured real account. Prop trading simulates fills
in a local wallet. Run `desk --keys` to make, restore, rename or choose the
trading key. It opens a key screen before any workspace and exits when you leave
it. See [live trading settings](trading.md).
Read the mode and full market name above Buy and Sell. The minimum size is 100
columns by 30 rows. Use 120 by 40 for more chart space.

Choose an amount, then click Buy or Sell once to submit it. Each button covers
five rows and repeats the selected amount. There is no separate Send step.
Mouse submission requires a press and release on the same currently visible
button. A changed market, mode, size or terminal geometry invalidates old controls.
Switching between prop and real trading still needs an explicit confirmation.

In prop trading, the right side shows the token's current dollar value,
spendable stables, open profit and total net profit. Open profit covers the tokens
still held at their current price, excluding past sales and fees. Buy spends stables plus the fee.
Sell returns stables minus the fee and sells at most the tokens held. Net profit
equals stables plus the current token value minus the starting balance.
For real trading, the token value is signed position exposure and the stable
figure depends on the venue's account mode. Unknown or stale values show `--`.
Real open profit comes from the venue. Real lifetime net profit is currently unavailable.
The live controls show Buy/Long and Sell/Short, the observed leverage, and the
current position direction. XMR orders require observed leverage of five times.
Entry and liquidation prices use the graph's existing price scale. Off-screen
prices have an arrow. Prop trading has no liquidation price.

| Input | Action |
| --- | --- |
| 1 or 3 with the small amount selected | Buy or sell that amount immediately |
| 2 or 4 with the large amount selected | Buy or sell that amount immediately |
| p or l | Open the prop or real mode confirmation |
| Enter with a mode confirmation | Confirm the displayed mode |
| Escape | Cancel mode confirmation or market entry |
| m | Change market |
| c | Switch line and candle charts |
| Arrows | Zoom or move through chart history |
| v | Mute or resume price sounds |
| q or Control with c | Quit, except q remains text during market entry |

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
