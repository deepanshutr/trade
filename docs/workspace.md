# Live and paper workspace

Run `desk XMR --paper` for Monero practice trading. `XMR` is the venue's Monero
symbol. Run `desk XMR` for viewing, or `desk XMR --trade` for the configured real
account. The right panel always names the current mode. The supported minimum is
100 columns by 30 rows. Use 120 by 40 for more chart and message space.

The book sits above the trade tape. Bitcoin and gold comparisons sit beside them.
BTC means bitcoin. PAXG is a token tied to gold and is the gold reference here.
Each comparison reports paired candle coverage. Missing, stale or incomplete
readings stay unavailable. A correlation describes past movement together. It
does not establish what either market will do next.

The right side shows Long one-day, Mid four-hour and Short one-hour outlooks below
the trade controls. Historical percentages describe measured target touches, with
sample counts and unavailable states. The normal view keeps transcripts and logs
out of the way. See [outlook and announcements](outlook-and-alerts.md) for the exact
method, source coverage and sound controls.

Click a trade button or press its numbered key to prepare an order. Read the
quantity, exact limit, value and account settings, then press Enter or click Send.
Each review expires after 30 seconds. A changed price, position or account setting
requires another review. Mouse activation requires a press and release on the
same visible button. Resizing or switching markets invalidates the old controls.

| Input | Action |
| --- | --- |
| 1 to 8 | Prepare the displayed Hard or Best order |
| 9 or 0 | Select a target buy or sell price |
| Up or down with a target | Move its limit by one valid price step |
| Enter with a target | Prepare its review |
| Enter with a complete review | Send that reviewed order |
| Escape | Cancel the current review, target or text entry |
| p or l | Open a paper or live mode confirmation |
| a | Ask the text assistant |
| h | Open or close Details, with Method, Sources and Answers pages |
| v | Mute or resume new announcements |
| y or n | Accept or reject the identified visible analysis |
| [ or ] | Scroll up or down in Details |
| Status button | Read full account, capture, news and assistant status in messages |
| m | Change the market |
| c | Switch between line and candle charts |
| Arrows without a target | Zoom or move through chart history |
| q or Control with c | Quit, except q is ordinary text while typing |

Paper mode starts with $10,000 and a 0.05 percent fee per fill. It uses visible
depth, allows partial fills and saves the account before reporting success. It
supports one open market at a time. Cash, realized results and the open position
survive a restart. The latest 1,000 fill records and the lifetime count are saved.
It models one-times exposure and excludes funding and liquidation. It is not a
simulation of every live exchange rule. Paper and live accounts stay separate
when changing modes.

The text assistant uses the existing Codex sign-in. It receives public market
measurements, the selected question and a fixed set of public news sources.
It has no trading, account, file or shell tools. It cannot accept an order or
change the trade controls. Accept and reject record feedback for the displayed
answer and never submit a trade. Numbers and pasted text remain text while the
question box is focused.

Automatic analysis runs at most once every five minutes when enough closed bars
exist. Questions take priority over automatic work. The analysis covers short,
medium and long bar intervals together. Moving averages and support or resistance
are computed locally. Requested average-crossing studies report their lookback,
entry, exit period, sample count and cost assumptions. Historical results are
measurements, not predicted odds. `--no-analysis` disables the connected assistant
and news. Local measurements remain available when a connection cannot answer.

News comes from the Federal Reserve, the US Bureau of Labor Statistics, CoinDesk,
FXStreet, UN News, the Zcash Foundation and the Monero project. Each fact keeps its
source, publication date when supplied, and retrieval date. A failed source
remains unavailable, so missing news does not imply a quiet market. Calendar
dates without a published time stay dates. The source set is not a complete news
service.

Local state defaults to `~/.local/state/desk`. Choose another directory with
`--state /absolute/path`. Paper account files, message history, public market
records and live order recovery state have separate locks. A second writer is
refused. Unreadable saved state is preserved rather than silently reset.

The public recorder saves observed quotes, depth, trades and candles. It retains
up to seven days or one GB, whichever bound is reached first. Available candle
history is requested at startup and repaired periodically. Older depth and tape
that the exchange does not provide cannot be reconstructed. Closed bars retain
their actual gaps. Computer sleep, logout, connection loss and recording failures
can leave gaps and are not presented as continuous observation.

Run `desk XMR --record --no-analysis` to record without opening a terminal or an
account worker. The Mac user-service definition can be reviewed with
`python3 tools/recorder.py`. It uses the installed jar and saves to the same public
directory. A service continues after Desk closes while the user remains logged
in. The terminal reads its saved candles and reports capture freshness. This
uses the per-user background-job mechanism documented by
[Apple](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html).

Live account configuration and order behavior are described in
[the trading guide](trading.md). Installation checks do not place a real order.
