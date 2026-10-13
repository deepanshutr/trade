# Configure live trading

The [workspace guide](workspace.md) describes the layout, prop account and
trade buttons. In market mode, clicking a trade button submits the selected amount
immediately. [Limit orders](#limit-orders) rest on the book instead.
Switching into real trading requires a visible mode confirmation. Starting the
program preserves observed leverage and margin mode.

The default settings file is `~/.config/desk/config.json`. `--config /path` selects
another file. It must be a private regular file with permissions `600`. An absent
default file permits public viewing and prop trading with these defaults:

```json
{
  "small": "500",
  "big": "1000",
  "target": "1000",
  "network": "mainnet",
  "propBalance": "10000",
  "propFee": "0.0005"
}
```

An `onchain` list adds markets that trade on a BNB Chain automated market maker
instead of the venue. Each entry names the market, the PancakeSwap V2 pair, the
V3 pool that prices the pair's quote asset in a dollar stable, and the JSON-RPC
endpoints to read from, tried in order:

```json
{
  "onchain": [
    {
      "name": "DEMO",
      "pair": "0x3333333333333333333333333333333333333333",
      "quotePool": "0x4444444444444444444444444444444444444444",
      "rpc": ["https://rpc-bsc.48.club", "https://bsc-dataseed.binance.org"]
    }
  ]
}
```

Names follow the reference name rule and cannot be a reference. The feed reads
the pair every two seconds, prices the token in dollars through the quote pool,
shows the pool's own liquidity as the book at fixed impact steps, builds candles
from its readings, and lists swaps as the tape, one reading behind and timed at
the reading that saw them. A node that refuses log queries still serves prices. There is
no candle history before Desk starts. Prop and live orders are not available on an
on-chain market. The bitcoin and gold references keep coming from the venue.

Amounts and age limits are quoted numeric strings. In real trading, small and big
are the money put up. The position value is the amount times the observed leverage,
which must be five times. So the default small of $500 opens a $2,500 position and
the default big of $1,000 opens a $5,000 position. Set `small` and `big` low, for
example `"20"` and `"100"`, before a first live test. In prop trading, small and big
are still the total order value before fees. Target has its own value ceiling.
`propBalance` applies when creating a prop wallet. Existing saved wallet
balances remain in place. Old margin simulation files remain separate and unchanged.
`propFee` is the fraction charged on each simulated fill. It must be from 0 to
`"0.01"` with at most eight decimal places. A settings file that still
has `paperBalance` or `paperFee` stops Desk at start until the two keys are renamed
`propBalance` and `propFee`.

`slippage` is the furthest an order may fill from the captured best quote, as a
quoted fraction. It is optional. The default `"0.001"` is 0.1 percent. It must be
above 0 and at most `"0.01"`, which is 1 percent, with at most eight decimal places.
Trailing zeros count, so `"0.0010000000"` is refused. Any other value stops the start
with "invalid trading settings: invalid slippage". Real orders, prop orders and CLOSE
all use it.

## Settings inside Desk

Press `s` in the workspace for settings. The [workspace guide](workspace.md#settings)
describes the page, the amounts and the key bindings. Desk writes only `small`, `big`
and `slippage` in the settings file and keeps every other field. The first write
renders the whole file again with two-space indentation, so comments and hand
formatting are lost. Each write is checked against the start-up rules, replaces the
whole file at once with mode `600`, so no reader sees half a file, and is refused with
"Settings changed on disk. Leave and open settings again" when another program changed
the file after settings read it. When no settings file exists, the first write creates
`~/.config/desk/config.json`. When Desk was started with `--config PATH`, it writes that
file. A write needs the file's folder to be private. Otherwise it is refused with "local
storage must be private" and the folder's full path. Run `chmod 700` on that folder. A
`--config` file belongs in a private folder of its own, not in your home folder.

A saved size or slippage applies to the next order without a restart. Every order
review carries the sizes and slippage it was made with, so an order already reviewed
still sends what it showed.

Live setup needs the public `owner` address in the settings and an active key. The
`owner` is the address of the wallet that owns the account. It uses the `0x` prefix
followed by 40 hexadecimal characters. The settings take no `agent`. A settings file
that has one stops every start until the field is removed. The message says to remove
`agent` and that the trading key is now chosen in Desk settings. Keep every private
value out of the settings, the command line, files and chat.

Live without a usable key or an owner shows "Live setup required. Press s for settings"
and refuses orders. The sentence names the key bound to settings, `s` by default. A
usable key has an active choice for the selected network, a name on that network, and a
Keychain item for its address. The check reads the item's attributes only. It never
reads the key.

The Keys section of settings has four rows:

- Trading key shows the active key's name and address, or why no key is usable. Enter
  opens the key screen described below.
- Owner shows the owner tooling's status and the owner address. Enter makes the owner
  or continues the step the owner is waiting on, as the status says.
- Approval shows "approved", "not approved" or "not checked". Enter asks the owner to
  approve the trading key at the venue. It runs only when the owner is ready, Desk has
  a usable key and the key is not approved yet. When the key's Keychain read was denied
  or timed out, Enter says "Read the trading key first. Press r".
- Funding shows the balance at the venue and how long ago it was read. Enter copies the
  owner's address. Fund it by a transfer to that address at the venue from an account
  you control. Desk never sends money.

The order of first use on mainnet is Owner, Funding, Approval, then Live.

Desk reads the owner status in the background when settings open, every 15 seconds
while they stay open, after each step and on `r`. A slow read never delays the screen.
The owner tooling is optional. Without it the Owner, Approval and Funding rows read
"Owner tooling is not installed" and Enter does nothing there. A status reply that is
not exactly the expected fields, fails, or is 4096 bytes or more shows "Owner status
unavailable". The approval step works only with a settings file inside
`~/.config/desk`. With a `--config` file elsewhere the Approval row says so.

The key screen and the owner steps run in a short child process that owns the
terminal. Secrets are typed there, never in the running Desk. Before a child starts,
Desk stops its live worker, which releases the saved-order lock so the key screen can
switch keys. While an order is pending, Desk refuses with "Wait for the pending order".
A live worker that does not stop shows "Trading did not stop. Restart Desk" and no
child runs. Resting orders stay at the venue while a child runs. Desk sees no fill
until the child ends, then looks each order up. The child's own prompts and refusals
stay on the terminal. When it exits, Desk prints "Press Enter to return to Desk." A
step that fails shows "The step did not complete (exit N)".

Control C during a child stops the child, not Desk. Control backslash prints a list of
Desk's threads on the terminal and Desk keeps running. Do not press Control Z at an
owner prompt. It stops Desk and the child together, and `fg` in the shell resumes both.

After every child, and on `r`, Desk reads the settings file and the active key again.
The file's sizes and slippage apply at once. A hand edit made while Desk runs waits
for `r` or the next child. A changed network is refused with "Restart Desk to change
the network". When the owner or the active key changed, Desk replaces its live worker.
In view mode the account figures then show `--` until you switch modes.

The key screen opens from settings. It lists, makes, restores, renames and selects
keys.

To make a key, press `n`. Desk makes a random seed and copies it to the clipboard. The
screen never shows the seed. Paste it into Passwords, save it there and press Enter.
Desk clears the clipboard. Then type a passphrase twice. It has 8 to 128 printable
ASCII characters, and spaces are allowed. Desk derives the key, which takes a few
seconds, and shows its address and its approval name. The drill follows. The screen
clears, and you type or paste the seed again, then the passphrase. Paste works only in
a seed field. Desk clears the clipboard after each paste there and discards a paste
anywhere else. A matching address continues. A mismatch offers a retry or a new seed.
Last, you name the key. Desk appends the name to `key-names.jsonl`, stores
the derived key in the Keychain through the key helper, and makes it the active key.
The key comes from Argon2id, a deliberately slow password hashing method, over the seed
and the passphrase. Rebuilding it needs both. Desk saves neither.

Some copies of the seed, the passphrase and the key can stay in Java and BouncyCastle
(the cryptography library) memory buffers. Desk cannot clear them until the process
exits. The key screen runs in its own process that exits when you leave it, so those
copies end with it.

The list shows each key with its name, address, network, approval name and an active
mark. Up and Down move. Enter selects the key. `r` renames it, `n` makes a new key, `s`
restores a key from its seed and `q` leaves. A key without a Keychain item shows "not in
Keychain" and cannot be selected. Nothing is ever deleted. A switch is refused while
the current key has a saved order, or while another Desk holds that key's order lock.
It is also refused while the current key has resting orders, with "Cancel the resting
orders for the current key first".
When a refusal stops a new or restored key from becoming active, the key is still stored
and named, the current key stays active and the screen shows the reason. Select the key
later from the list. Restore asks for the seed and the passphrase, shows the address,
then stores the key and makes it the active key. It asks for a name only when the
address is new and never writes a second record for a known address.

Two public files hold the choices. `~/.config/desk/key-names.jsonl` holds the names. It
has one JSON object per line, Desk only appends to it, and its mode is `600`. Desk
refuses a file with a torn last line or a hand edit, names the line, and never repairs
or truncates it. `~/.config/desk/active-key.json` holds one address per network. Its
mode is `600`, and every change replaces it in one atomic write. Neither file holds a
private value.

The key helper keeps the only Keychain write. `make install` copies it to
`~/.local/lib/desk-keychain.py` and writes the launcher `~/.local/bin/trade` with the
Python it will use. The command is `trade` on every system, because Windows opens
Display settings for a bare `desk`. In `~/.local/bin` the install replaces `trade` or
removes `desk` only when its text names `~/.local/lib/desk.jar`, as every earlier launcher
did. So a `desk` from an earlier install is removed, with one line saying so, and anything
else by that name stays. A `~/.local/bin/trade` that does not name the jar stops the
install before anything is written. Rename or remove it, then install again.
Run `make install DESK_VAULT=/absolute/path/to/the/vault` once to give settings the
owner tooling. The launcher then exports `DESK_VAULT` next to the other two variables.
The path must be absolute and is checked the same way.
Desk gives the helper the derived key on standard input, and the helper stores it as a
generic password under service `desk`. The account is the key's address. A custom
`keyService` setting changes the service name. The helper needs the installed Python
libraries `cryptography`, `pycryptodome` and `eth-account`.
`python3 tools/keychain_wallet.py --check-access` checks the Mac permission settings
without creating a key. The `--create` mode, the setup record files and the `agent`
setting no longer exist. The launcher starts Java with attach disabled, so other
programs cannot read Desk's memory through Java's attach tools.

When the settings name an owner and a usable key exists, Desk reads the key from the
Keychain once, before the workspace starts. It prints "Waiting for the Keychain prompt.
Choose Allow, not Always Allow." and waits up to 120 seconds for the prompt. Use Allow,
not Always Allow. Apple says Always Allow lets the app read the password "without any
further authorization or notice from you". See [Apple's access
guide](https://support.apple.com/en-us/guide/keychain-access/kyca1243/mac). Desk keeps
the key in memory until that process closes. Starting a new chat does not close a
running Desk process. Each live worker gets its own copy and wipes it when the worker
closes. A denied or timed-out read leaves Desk running without a key. Live then shows
its blocking reason. Desk verifies that the loaded key matches the active key and that
the venue reports delegation to the owner.

After settings change the active key, or set the owner while no key is held, Desk reads
the key the same way, with the same prompt line. When the read is denied, the Trading
key row says "The key was not read. Press r to ask again", and each `r` asks again. Each
read leaves one more copy of the key in Desk's memory until Desk exits, so do not press
`r` without a reason. An unchanged key is not read again.

Making a key does not register a trading account. The wallet that owns the account must
approve the key's address at the venue as a named API key. Approve it under the approval
name that Desk shows. The name is `desk-` followed by the first ten hexadecimal
characters of the address after `0x`. Each key has its own name, so up to three Desk
keys can be approved at once. Until the approval is in place the key cannot trade, and
the role check must pass. An approval can expire. A key whose approval has expired
cannot trade until the owner approves it again. The Approval row in settings runs
that step through the owner tooling.

Desk's local signer accepts only two actions. One is a single order, either immediate
(the venue's `Ioc`) or post only (`Alo`). The other is a cancel of one order by Desk's
own client order id (`cancelByCloid`). It rejects orders that rest without the post
only rule (`Gtc`), cancels by the venue's order number, order changes, batches,
scheduled cancels, fund transfers, withdrawals, permission changes and leverage changes
before producing a signature. Desk builds a cancel only from its own saved resting
orders. Hyperliquid's standard agent permission is broader and includes
transfers between balances within the same account. The local restriction does
not constrain a key used outside Desk. See the venue's
[agent-transfer documentation](https://hyperliquid.gitbook.io/hyperliquid-docs/for-developers/api/exchange-endpoint#agent-send-asset).

Start `trade XMR --trade`, or select Live and confirm its account mode. XMR is the
venue's Monero symbol. The panel identifies the current trading mode and market.
Check the owner in the local settings and the address on the key screen before enabling
this mode.
Test network access uses `"network": "testnet"`. Public feeds and the live worker
use the same network. An account, delegation, funding and any real order remain
separate user actions.

Real orders on every market require observed leverage of five times. Otherwise the
controls name the market, for example "Set BTC leverage to 5x", and refuse every
order. Select 5x for that market at the venue. A market whose maximum leverage is
below 5x cannot trade live. Desk preserves the observed margin mode and never signs
a leverage change. The margin mode and leverage are captured with each order and
rechecked before sending. Changing them at the venue invalidates a captured order.
A resting order keeps the size it was placed with. A leverage change at the venue
while orders rest is outside Desk's control. Missing or stale position data remains
unavailable and is never interpreted as a flat account.

A new account may already read five times on XMR. An anonymous read on 2026-10-02
for an address that had never traded showed XMR at 5x and BTC at 20x. That is an
observation, not a venue rule. If your account shows another leverage on XMR, live
trading stays blocked until you set XMR to 5x at the venue.

The worker dates account freshness from the start of each account read and requests
another read immediately after an order completes. Quiet active-account data is
refreshed through one outstanding read on the existing account connection. Late,
wrong-market or replaced-connection replies cannot renew the selected account.

Market mode uses immediate orders. The price limit is the captured best quote
moved by the `slippage` fraction, up for a buy and down for a sell. Available depth
can produce a partial fill or no fill. An order must be worth at least ten dollars
at its quote and its limit. Only a close may be smaller.

In real trading, the button on the side opposite an open position only reduces it.
Desk sends that order reduce-only, so the venue refuses any part that would grow the
position or open the other side. A reduce at least as large as the position, or one
that would leave less than ten dollars at the quote, closes the whole position
instead. CLOSE, on key `x`, sends the whole position reduce-only. To turn a long
into a short, press CLOSE, then SHORT. A close that fills in part leaves the rest
open. Press CLOSE again. A close under ten dollars is still sent, and the venue may
refuse it under its own minimum.

In prop trading, Buy spends stables and Sell is capped at held tokens. A prop sell
that would leave less than ten dollars of tokens at the quote sells the whole
holding. The selected value excludes fees. Calculations retain full precision.
Compact dollar figures are rounded for display. Best and Target orders are no longer
exposed in the normal workspace. [Prop limit orders](#prop-limit-orders) have their own
fill rules.

Live orders keep durable recovery state under the chosen state directory,
separated by network and delegated signer. One process holds each account lock.
The captured quantity, limit and account settings are compared with the current
full plan before queuing and immediately before signing. The plan includes the
reduce-only flag and depends on the position. So a position change between the
press and the send refuses a reduce or a close. It is not resized. An uncertain response
blocks further orders while status is checked. The pending order is not resent.
A parsed top-level `err` reply keeps the saved order pending for a status lookup.
A matching final status settles it as usual. A parsed `unknownOid` lookup releases
a saved order only when the local wall time read before the lookup was sent is past
the signed `expiresAfter` plus a fixed 60-second margin. Before then it stays saved,
because an action still in flight may not show in the lookup yet. After the margin,
an order with an `err` reply releases as rejected without owner and role checks.
Any other order releases as "not placed" under the owner and role checks below.
A limit order may rest, so it releases as "not placed" only on the bound a resting
order has. An open orders read past the margin must list no order at its market, side
and limit with no more than its size, other than Desk's own resting orders. Then three
`unknownOid` answers at least ten seconds apart must follow, each past the margin with
the role proven. Desk reads the open orders as soon as the margin passes, so a lost
limit order is released about 85 seconds after it was sent. Every press waits until
then, CLOSE included.
Desk remembers which order got the `err` reply only in memory, so a restart
applies the normal saved-order rules. Both time releases rely on four assumptions:
the local clock is within the margin of the venue's clock; the venue refuses an
action after its `expiresAfter`, which is unproven until a test-network order; the
venue's status lookup reflects any action applied before expiry within the margin;
and an `err` reply plus an `unknownOid` lookup after the margin means not placed.
The local clock may step, so Desk also waits the same span of real elapsed time,
65 seconds by the monotonic clock, since it first saw the order: when it reserved
the order, or when it loaded the saved order at startup.
Failed lookups keep the order saved regardless of time, and known order statuses
retain their existing handling. Do not delete recovery state manually. Resolve any
remaining block using the account's order history.

A saved order records its owner. A matching final status reported under the
configured owner settles the order whatever its saved owner. Release as "not
placed" after the expiry margin requires `unknownOid`, the saved owner to equal
the configured owner and a fresh role check confirming the signer belongs to that
owner. A saved order with an `err` reply uses the `err` rule above regardless of
owner. While the owners differ, other time-based release remains blocked and
trading stays paused for that signer.
A saved order from an older version without an owner settles only when the venue
reports a matching final status under the configured owner. If the venue never
received it, it shows "waiting for order result", blocks trading for that signer
and never releases by time. Check the account's order history before upgrading
with an unresolved order.

A wallet change pauses live orders for that owner and network across Desk processes
sharing the same home directory, including ones using another state directory.
The marker is `~/.local/state/desk/accounts/<network>-<owner-without-0x>.paused`,
with the owner address in lowercase. Desk only reads this marker. No writer is
provided in this repository. An external wallet-change tool must create and clear
it as part of its account-change procedure.
The controls show the pause and refuse new orders. The pause survives a restart.
Finish or resolve the wallet change before using the replacement account. Do not
remove saved pause state to bypass it. The pause blocks cancels as it blocks orders, so
cancel every resting order before you start a wallet change. Only you can do this
step.

Quoted prices and book depth normally expire after one second. Position and active
account data normally expire after two seconds. Optional `quoteAgeMs` and
`positionAgeMs` accept 100 to 5,000 milliseconds. A healthy public socket
does not establish account readiness. The blocking reason and final order outcome
remain visible in the controls and latest action notice.

Restart an already running Desk after installing a new build. It continues using
its previous loaded program until restarted. No build resends a saved order. A
restart only looks up its status. An older build would still describe a saved
reduce-only order as an ordinary one, so resolve any saved order before installing an
older build. An older build also cannot read the saved order file while orders rest.
Cancel every resting order first. See [saved orders](#saved-orders).

## Polymarket reads

Desk reads the chance of up for bitcoin and gold from Polymarket's public API for the
[Polymarket band](workspace.md#the-polymarket-band). The reads go to Polymarket's hosts,
not the venue's, with their own pacer, the count that holds them to six requests in any
minute. They never count against the venue budget, the pace Desk keeps to stay under the
venue's request limit. They carry no account, key or cookie. They run in live, prop and
view alike. The recorder started with `--record` makes none. After a failed read Desk
waits one minute, and twice as long after each further failure, up to five minutes.

The reads go straight from your own internet address, so Polymarket and the networks
in between see that address. They run at fixed times: one at start, one five seconds
after each five minute mark, and a price read every 30 seconds while a market is open.
Each read names the Java version in its user agent. So a server log can tell when Desk
runs and from where, though not your account, your orders or the market you watch.

## Limit orders

Press `t` in the workspace to switch to limit orders. The
[workspace guide](workspace.md#market-and-limit-orders) describes the ticket and the
price axis. Limit orders work in live and in prop. View places nothing.

Every limit order is post only. Desk sends it with the venue's `Alo` setting, which
means add liquidity only: the venue cancels the order rather than let it trade at
once. So the order rests on the book until it fills or is cancelled, and it pays the
maker fee, the lower fee for an order that rested. The venue's base fees are 0.015
percent for a maker and 0.045 percent for a taker. The ticket shows the fee at the
base maker rate. The fee actually charged depends on the account's fee tier, which
Desk does not read.

The ticket starts at the best bid for a buy and the best ask for a sell, so the order
joins the back of the queue at that price. Enter is refused while the price would
trade at once: a buy at or above the best ask, or a sell at or below the best bid. The
status row then reads "Takes liquidity. Move below the ask" or "Takes liquidity. Move
above the bid". The worker checks again just before signing. A price the market has
crossed since then is refused as "XMR canceled before sending". If the venue still
refuses, the outcome is "XMR order rejected" and nothing rests. On a fast market you
may need two tries.

A limit order is sized at its own price: the amount times the observed leverage
divided by the price, rounded down to the smallest size the market allows. Each order
must be worth at least ten dollars at its price. A press at a price where Desk already
has an order places a second order with its own id. The first keeps its place in the
queue.

Desk holds at most 20 resting orders for each key, across markets. A limit press past
that answers "20 orders rest. Cancel one first". The cap applies to limit presses
only, so market presses and CLOSE still work. It governs new orders only, so an order
Desk takes back after a release can take the list past 20.

What is sent is what the ticket showed. Enter previews the order again and sends it
only when the fresh preview is the same order: the same market, side, kind, size,
price, reduce only flag and post only setting. Otherwise the ticket shows the new
order with "Order changed. Enter again to send" and waits for the screen to show it.
Every order action acts only through the screen that drew it.

A resting order sits on the price axis in its side's colour with the money it puts up,
and the status row reads "Resting $19.99 #9". LONG and SHORT stay free while orders
rest. While Desk sends an order or a cancel, a new press is refused with "Order
pending".

### Both sides

Orders may rest on both sides of the price at once, for example buys below and sells
above. One market has one net position, so fills from both sides offset each other.

The venue sets an order's reduce only flag once, when it is placed, and never changes
it. A press that faces the position when it is placed is sent reduce only and capped
at the free position. A press made while flat, or on the position's side, opens. The
ticket's first row names the flag as `reduce only` or `post only`. An order faces the
position when it is on the side that shrinks it.

Later fills can change what a resting order would do. Desk works this out again on
every screen, and the status row shows it while the cursor is on the order's row. The
rows are tried in this order and the first match wins:

| Shown | When | What a fill would do |
| --- | --- | --- |
| `void` | The order is reduce only and does not face the position, which includes a flat one | Nothing. The venue should cancel it |
| `opens` | The position is flat, or the order is on the position's side | Open or grow the position |
| `reduce` | The order faces the position and fits the free position | Shrink the position |
| `flip $5.44 XMR if filled` | The order faces the position, is larger than the free position and is not reduce only | Close the position and open the other side with the amount shown |
| `reduce` | The same, but reduce only | Fill the reducing part, which is an assumption about the venue |

The free position for an order is the position less the remaining size of the facing
orders nearer the price. So with two facing orders, only the one that would fill
second can read `flip`. A `flip` is the only way the account can change sides without
a press, and it exists only when you placed entries on both sides. Desk cancels
nothing on its own.

Desk adds up the remaining size of every resting order that faces the position,
whatever its flag, up to the position size. That sum is reserved. A new REDUCE,
limit or immediate, is capped at the position less the reserved sum. When nothing is
left it is refused with "Resting orders cover the position". A reduce that would leave
less than ten dollars of the free position closes all of the free position instead.
CLOSE ignores resting orders and is always the whole position.

For 2 seconds after a fill shrinks the position, from an immediate order or a resting
one, Desk refuses any new order that would open or grow a position. A part fill of a
resting order starts the pause too. A fill Desk finds through a status lookup or the
open orders read counts as one that shrank the position. The pause gates only new
sends. An order that rested before it fills whenever the market reaches it,
and Desk cannot stop it.

Resting orders on both sides reserve margin at the venue, so the account's own
capacity check refuses a press it cannot carry. Live sizes each order at 5x when it
is placed. A leverage change at the venue while orders rest is outside Desk's control.

### How fills and cancels arrive

Desk follows its orders on the account connection it already holds. Each time that
connection opens, Desk subscribes to the owner's order updates. A fill, a partial fill
or a cancel at the venue arrives as an update. It shows on the status row, on the axis
and in the order result log:

| At the venue | Text |
| --- | --- |
| Filled | `XMR filled $21.84 at 546.09` |
| Partly filled | `XMR part filled $5.46 of $21.84 at 546.09` |
| Canceled | `XMR canceled $16.38 at 546.09` |
| Canceled by self-trade prevention | `XMR resting buy $19.99 at 546.09 canceled by self-trade prevention` |
| Canceled by the venue for another reason | `XMR canceled by the venue $16.38 at 546.09 (reduceOnlyCanceled)` |

A fill shows at the order's own price, the price a resting order is expected to fill
at. Desk reads an update's size as the size still resting. The venue docs list that
field without saying so. Check it on the first live partial fill: the remaining size
on the axis should match the account's fill history. If it does not, only the shown
remaining size is wrong, since Desk never raises a remaining size and sends nothing
because of it.

A fill update can arrive before the reply to the order itself. Desk holds it until the
order rests, then applies it, so no row shows twice. A fill update also settles an
order whose reply was lost. An update for an order Desk did not place is ignored and
counted in diagnostics. A count above zero means another tool places orders on the
account, or that the venue still held an order Desk let go.

Desk lets an order go when it releases it as "not placed" or settles it as not found.
It keeps such orders in memory, up to 200, until it quits. A later update for one is
counted the same way and named with "after release", such as "XMR filled $19.99 at
546.09 after release". An open one rests again on the axis and in the saved order file,
and Desk follows it from then on.

Updates are missed while the connection is down, so Desk keeps a safety net. When it
starts, it looks up each resting order once. After every connection opens, and once
a minute while orders rest, it reads the account's open orders and looks up any order
missing from them, each no sooner than ten seconds after its last try. A remaining
size never grows from that read. A failed lookup keeps the order. An order missing
from the open orders that the venue does not know on three lookups, each at least ten
seconds apart and all at least 60 seconds after the order was made, settles as
"XMR $21.84 at 546.09 not found at the venue".

Enter on an order row cancels each order at that price in turn, one signed cancel per
order, by Desk's own client order id. The status row reads `Canceling` meanwhile, and
the row's amount is faint. A success reads `Canceled $16.38 #9` on the status row and
"XMR canceled $16.38 at 546.09" in the order result log. A cancel that errors or gets
no clear reply is settled only from one status lookup. An order the lookup finds still
open stays, and the status row reads `Cancel refused`, with "XMR cancel rejected" in
the log. An order the lookup cannot place stays for the next check, and the status row
reads `Confirming result`, with "XMR cancel result unknown" in the log. Desk stops at
the first cancel the venue does not answer with a clear success. The orders after it
stay, and the next Enter on the row cancels them. A cancel waits while an order is
pending, with "Wait for the pending order". It needs neither the selected market nor
fresh prices. Desk sends no cancel you did not pick on the axis, and a market change
cancels nothing.

When the venue refuses an order or a cancel, the order result log keeps the venue's own
words after Desk's text, such as "XMR order rejected | venue: Order has insufficient
margin". Desk keeps their first line, at most 160 characters. The status row shows only
Desk's short word, such as `Not sent` or `Cancel refused`. Record these words when you
run the rollout checks of the venue assumptions.

CLOSE pressed in live while a row cancel runs goes next. Desk finishes the cancel it
is sending, sends none of the others and reads `Close goes after this cancel` on the
status row. CLOSE is then sent as soon as live is ready again, as if pressed at that
moment. If live is not ready within 30 seconds, CLOSE is not sent and the status row
says why. A market change before then drops it too. Esc or opening settings withdraws
it, and the status row reads `Close withdrawn`. The orders after the current cancel stay
for the next Enter.

While prop or view is selected, or while settings run a key or owner step, Desk does
not follow live orders and live fills are not seen. They show when live runs again,
after Desk looks the orders up. Live orders keep resting and can fill after Desk quits
or while prop or view is selected.

### CLOSE and resting orders

CLOSE sends at once, sized at the whole position, and Desk cancels nothing before or
after it. Four consequences follow:

1. A Desk entry resting on the other side within CLOSE's price allowance is cancelled
   by the venue's self-trade prevention. That rule cancels the resting order instead
   of filling two orders of the same account against each other. For example, a
   resting buy at the bid vanishes when CLOSE sells a long through it. It shows as
   "XMR resting buy $19.99 at 546.09 canceled by self-trade prevention". A market
   REDUCE can do the same.
2. Resting reduce only orders left with nothing to reduce are expected to be cancelled
   by the venue, shown as "XMR canceled by the venue $16.38 at 546.09
   (reduceOnlyCanceled)".
3. If the venue refuses a CLOSE sent at once while reduce only orders rest on its
   market, the status row reads `Close refused. Press o for orders`. The order result
   log keeps "XMR order rejected. Resting reduce orders may block it, press o". Cancel
   them from the axis, then press CLOSE again. Any other refusal, such as a posted
   REDUCE that became a close, reads `Not sent` with "XMR order rejected" in the log.
4. After a CLOSE or a REDUCE to flat, an entry resting on the far side without the
   reduce only flag stays on the axis in its side's colour. It opens a new position
   with no press when the market reaches it. Cancel it from the axis if you do not
   want that.

The second and third are assumptions about the venue until a live test shows them. So
is the fifth row of the table under [both sides](#both-sides).

### Saved orders

The saved order file holds the one order in flight and the list of resting orders. It
is version 1, the shape older builds write, while no order rests. While orders rest it
is version 2. An older build cannot read version 2 and blocks live with its saved
order failure. Cancel every resting order and resolve any saved order before
installing an older build. A cancel saves nothing but its nonce, a number each signed
action uses once. If Desk stops during a cancel, the next start looks the order up.

### Prop limit orders

Prop takes limit orders with the same ticket, axis, cap, price rule and cancel as
live. It keeps its resting orders in its wallet file, `prop/wallet-mainnet.json` or
`prop/wallet-testnet.json`, so they survive restarts and settings. A cancel removes
the order at once. Prop has no venue, so it fills resting orders from the market's
trades:

- A resting buy fills when a trade prints below its price, and a sell when a trade
  prints above it.
- It fills whole at its own price, whatever the trade's size, with the maker fee of
  0.015 percent, not the prop fee.
- A trade at the order's price does not fill, since the place in the queue is unknown.
  So prop fills later than live would.
- A trade counts once, only with an id, only when it printed after the order rested,
  and never while Desk uses backup prices.
- One trade through two orders fills the nearest first.
- An order on another market fills only once that market is selected.
- Prop rests limit orders on one market at a time. While it holds tokens of one market,
  or orders rest on one market, a limit order on another market is refused at the ticket
  and at Enter with "Prop holds XMR. Return to that market".
- A buy checks the cash when it rests. Its value and maker fee must fit the cash left
  after the other resting buys, else "Not enough stables for order and fee". `Usable`
  on the panel is that figure. A buy whose cash an immediate buy spent later is
  cancelled at its fill with "(not enough stables)".
- Prop holds only tokens it bought, so a limit sell needs tokens held. A sell larger
  than the tokens held fills what is held, and the rest is cancelled with "(nothing
  left to sell)".
- The cap of 20 counts prop's resting orders and applies to limit presses only.
- The two second pause starts only after a resting sell fills or a CLOSE. An immediate
  sell does not pause, so its proceeds can fund the next buy at once.

CLOSE in prop, from `x`, the CLOSE button or Enter on the entry row, sells the whole
holding at once at market with the prop fee, whatever orders rest. Resting sells then
left with nothing to sell are cancelled when their trade comes.

A downgrade to an older Desk drops prop's resting orders on its next save. Only
simulated money is affected.
