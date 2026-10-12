# Outlook and announcements

The normal workspace draws three level columns on the graph, immediately to the
right of its price axis. From left to right, Short covers the next hour, Mid
the next four hours and Long the next day. Each column uses twelve characters.
All three use the same visible price scale as the graph.
Trade controls remain below the columns.
The arrow describes the current trend. Support, resistance and target come from
the last 50 complete candles. Long uses hourly candles, Mid uses fifteen-minute
candles and Short uses five-minute candles.

The trend is up when the last close is above the 20-candle average and that average
is above the 50-candle average. It is down when both comparisons reverse.
Otherwise the trend is mixed. An up target is resistance, the highest high in
that window. A down target is support, the lowest low. These are descriptive
levels, not instructions to trade.

`S` marks support, `R` resistance and `T` target. A horizontal mark sits on the
corresponding price row. Line-chart marks also follow the plotted dot height
within that row. Combined letters share a tick. When different prices share one
tick, a star replaces the single price label. An up or down mark
means the level is above or below the visible scale. Those levels do not stretch
the graph or acquire a false edge price.

## Reading the percentages

`Test` is the percentage distance from the last closed price to the displayed
target. It is the distance used in the historical test, not the remaining distance
from the live quote. Compact
prices use rounded significant digits and, when needed, scientific notation.
If no value fits within one percent precision, the compact value appears as `--`.

`Base` is the percentage of eligible past periods that touched that same relative
target distance within the named time. `Match` uses the same test but keeps only
periods with the same moving-average trend. `n` lists the Base and Match sample
counts. When evidence is unavailable,
its status appears in place of the compact Test distance.

Each historical signal uses only candles closed by its starting time. Outcome
periods are aligned to the calendar and do not overlap. At least 100 Base samples,
50 Match samples and 90 percent complete scheduled periods are needed to display
a rate. These thresholds are product choices.

These numbers describe past target touches. They are neither forecast confidence
nor the chance of a profitable trade. Fees, entry timing, liquidation and which
level is reached first are not part of this test.

Missing, stale, inconsistent or gapped evidence is labelled. Unavailable rates
appear as `--`, not zero. A reached target suppresses its rates until its reference
target changes. Initial exchange history is limited to the latest 5,000 candles
per interval. Local capture and repair help with continuity but do not create
unavailable older exchange history.

## Rail measures

The rails of the indicator band read five windows: 5m, 15m, 1h, 4h and 24h. The
correlation now sits on the rails. See the [workspace guide](workspace.md#the-indicator-band)
for how they draw. A measure that is missing or stale reads `--` and draws no marker.

The change of a coin over a window is its latest price over its price one window ago,
minus one, in percent. For 5m and 15m Desk reads the one second samples. The old price is
the last sample at or before the window's start, when it is within five seconds of it.
For 1h, 4h and 24h Desk reads the one minute candles. The old price is the close of the
bar that ends at or before the window's start. The distance from the average is the
latest price over the mean of the window's prices, minus one, in percent, from the same
sources. It needs at least half of the window's expected points. Both are stale when the
latest sample is older than five seconds for 5m and 15m, or the latest one minute bar is
older than two minutes for the longer windows.

The correlation of bitcoin and of gold with the selected market uses candles from 15m to
24h: 15 one minute returns for 15m, 60 one minute returns for 1h, 48 five minute returns
for 4h and 96 fifteen minute returns for 24h. Desk reads one more closed bar than each
count, such as 16 one minute bars for 15m. For 5m it uses a ten second grid over the
last five minutes. At each of the grid's 31 points it takes each series' last sample at
or before the point, which gives thirty returns for each series. A grid value is missing
when its sample is more than 30 seconds older than the grid point. The 5m correlation
needs all thirty returns of both series and a latest sample within five seconds for each.

The book's share of buying over a window is the mean of each book update's depth
weighted imbalance across the window's minutes, mapped from minus one and plus one onto
0 and 100 percent. The depth weighted imbalance is bid size minus ask size over their sum
across the top ten levels, the nearer levels counting more. The tape's share is the buy
size minus the sell size over the total size of the window's trades, mapped the same way.
It reads `--` when the window holds no trade. Both shares read `--` until the book has
updated in at least half of the window's clock minutes, three of the 5m window's five.
Partial minutes count, the one Desk starts in and the current one. So the 5m shares can
show about a minute after start, when Desk starts near the end of a minute, and the 24h
shares after about twelve hours. They start again from nothing when Desk starts and when
the market changes.

## Announcements

Desk watches fresh prices for the selected market. A rise or fall greater than
five percent within less than one hour creates a price sound.
Exactly five percent and exactly one hour do not qualify. Startup and disconnected
periods are not replayed. A direction must reset below four percent and pass a
one-minute cooldown before another announcement in that direction.

Voice plus a chime is the default on macOS. Press `v` or click Mute to cancel current
and queued sound. Click Sound to resume new announcements. Start with `--chime`
for chime only or `--mute` for initial silence. Desk does not change system volume.
Sound and historical calculations run separately from trading input. The compact
workspace has no text announcements, connected news reader or text assistant.
A missing or failed sound does not stop trading.

## Method references

- [Exchange candle history contract](https://hyperliquid.gitbook.io/Hyperliquid-docs/for-developers/api/info-endpoint#candle-snapshot)

The test runner uses synthetic prices, recorded public fixtures and fake audio.
It does not submit live orders or establish speaker delivery or forecast accuracy.
