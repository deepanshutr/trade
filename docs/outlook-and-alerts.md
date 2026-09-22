# Outlook and announcements

The normal workspace draws three level columns on the graph, immediately to the
right of its price axis. From left to right, Short covers the next hour, Mid
the next four hours and Long the next day. Each column uses twelve characters.
All three use the same visible price scale as the graph.
Trade controls remain below the columns, including while Details is open.
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
the graph or acquire a false edge price. Details retains every exact level.

## Reading the percentages

`Test` is the percentage distance from the last closed price to the displayed
target. It is the distance used in the historical test, not the remaining distance
from the live quote. Method shows the exact reference and level values. Compact
prices use rounded significant digits and, when needed, scientific notation.
If no value fits within one percent precision, the compact value appears as `--`.
Method retains the full value.

`Base` is the percentage of eligible past periods that touched that same relative
target distance within the named time. `Match` uses the same test but keeps only
periods with the same moving-average trend. `n` lists the Base and Match sample
counts. Method includes a rough 95 percent interval for each historical count,
the reference close and the observed date range. When evidence is unavailable,
its status appears in place of the compact Test distance.

Each historical signal uses only candles closed by its starting time. Outcome
periods are aligned to the calendar and do not overlap. At least 100 Base samples,
50 Match samples and 90 percent complete scheduled periods are needed to display
a rate. These thresholds are product choices. Market observations can remain
related even when periods do not overlap, so the count intervals are approximate.

These numbers describe past target touches. They are neither forecast confidence
nor the chance of a profitable trade. Fees, entry timing, liquidation and which
level is reached first are not part of this test. The text assistant receives the
same numerical method and evidence rather than supplying its own probability.

Missing, stale, inconsistent or gapped evidence is labelled. Unavailable rates
appear as `--`, not zero. A reached target suppresses its rates until its reference
target changes. Initial exchange history is limited to the latest 5,000 candles
per interval. Local capture and repair help with continuity but do not create
unavailable older exchange history.

## Details and controls

Press `h` or click Details for Method, Sources and Answers. Method explains the
calculation. Sources shows publication dates, retrieval coverage and retained
announcements. Answers contains the conversation and accept/reject controls for
the visible analysis. Use the scroll controls for longer content and Escape to
return to the compact view.

Press `a` to ask a question. Typed or pasted trading keys remain text while the
question is open. Trade buttons still prepare a review. Sending an order still
requires the complete, current review to be on screen. Failed actions show a short
notice beside the controls, with the full message retained in Answers.

## Announcements

Desk watches fresh prices for the selected market. A rise or fall greater than
five percent within less than one hour creates a visual and audible announcement.
Exactly five percent and exactly one hour do not qualify. Startup and disconnected
periods are not replayed. A direction must reset below four percent and pass a
one-minute cooldown before another announcement in that direction.

The selected headline sources are CoinDesk for crypto, FXStreet for gold,
UN News for peace and security, the Zcash Foundation and the Monero project.
The existing labor-statistics and central-bank calendars remain available to
the assistant. Sources are sampled every two minutes. Coverage is a selected
sample, not complete market news. A recently fetched source can contain older
publications, and both times remain inspectable.

The first successful batch from each source establishes a silent baseline.
Subsequent new relevant headlines must have a publication time within the last
hour. Missing dates, future dates, untrusted publisher links and stale retrievals
do not create announcements. Tracking-link duplicates are suppressed.

Voice plus a chime is the default on macOS. Press `v` or click Mute to cancel current
and queued sound. Click Sound to resume new announcements. Start with `--chime`
for chime only or `--mute` for initial silence. Desk does not change system volume.
If the audio queue is full or sound is unavailable, visual announcements remain
available and the sound status reports the condition. Sound and historical
calculations run separately from trading input. `--no-analysis` also disables the
connected news reader, while local price alerts and outlook calculations remain.

## Method references

- [Exchange candle history contract](https://hyperliquid.gitbook.io/Hyperliquid-docs/for-developers/api/info-endpoint#candle-snapshot)
- [NIST description of the Wilson count interval](https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm)

The test runner uses synthetic prices, recorded public fixtures and fake audio.
It does not submit live orders or establish speaker delivery or forecast accuracy.
