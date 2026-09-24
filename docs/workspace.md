# Trading workspace

Run `desk XMR --paper` for simulated Monero trading, `desk XMR` for viewing, or
`desk XMR --trade` for the configured real account. Read the mode and full market
name above Buy and Sell. The minimum size is 100 columns by 30 rows. Use 120 by
40 for more chart space.

Choose an amount, then click Buy or Sell once to submit it. Each button covers
five rows and repeats the selected amount. There is no separate Send step.
Mouse submission requires a press and release on the same currently visible
button. A changed market, mode, size or terminal geometry invalidates old controls.
Switching between simulated and real trading still needs an explicit confirmation.

The right side shows the selected token's signed dollar value, stable collateral
and net profit after fees. A negative token value means a short position. The
simulated stable figure is margin collateral cash, not a spendable spot balance.
For real trading, stable collateral depends on the venue's account mode. Unknown
or stale values show `--`. Real lifetime net profit is currently unavailable.
Entry and liquidation prices use the graph's existing price scale. Off-screen
prices have an arrow. Simulated trading has no liquidation price.

| Input | Action |
| --- | --- |
| 1 or 3 with the small amount selected | Buy or sell that amount immediately |
| 2 or 4 with the large amount selected | Buy or sell that amount immediately |
| p or l | Open the simulated or real mode confirmation |
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

Simulated trading starts with $10,000 and a 0.05 percent fee per fill. It uses
fresh received depth, permits partial fills, and saves the account before
reporting success. Prices normally expire after one second. Fresh backup depth
can support orders during stream recovery. Delayed or wrong-market replies cannot
restore readiness. A slow save shows Saving with elapsed seconds and blocks another
order until the result is known. A failed save preserves the last confirmed account.

The simulation supports one open market at a time and one-times exposure. Cash,
fees, realized results and the position survive restarts. The latest 1,000 fills
and lifetime count are retained. Funding and liquidation are excluded. An opposite
order can reduce a position, pass through zero and open the other direction.
Simulated and real accounts remain separate, and simulated mode does not poll the
real account. No fill, partial fill, refusals and repeated results appear in one
short status row, with a separate action number for each result.

Local state defaults to `~/.local/state/desk`. Change it with `--state /absolute/path`.
Account files, message history, public data and live recovery state use separate
locks. Unreadable saved state is preserved. Public recording retains up to seven
days or one GB. Gaps remain gaps. Run `desk XMR --record --no-analysis` for the
background public recorder. Inspect its Mac service with `python3 tools/recorder.py`.

The interactive process writes private diagnostic records under `diagnostics/`
in the chosen state directory, even when a separate process owns public recording.
Four files retain at most 8 MiB. These records include validated market selections,
history counts, quote age, disconnects, slow input and frames, and fixed failure
categories. They exclude typed text, account data, orders and raw error messages.
Logging uses a bounded background queue. `!logs` marks unavailable diagnostic
storage and `!save` marks a message-history save failure. See [logging details](diagnostics.md).

Restart an already running Desk after installation to load the new build.
