# Configure live trading

The [workspace guide](workspace.md) describes the layout, prop account and
trade buttons. Clicking Buy or Sell submits the selected amount immediately.
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

Amounts and age limits are quoted numeric strings. Small and big are total order
value before fees. Target has its own value ceiling.
`propBalance` applies when creating a prop wallet. Existing saved wallet
balances remain in place. Old margin simulation files remain separate and unchanged.
`propFee` is the fraction charged on each simulated fill. A settings file that still
has `paperBalance` or `paperFee` stops Desk at start until the two keys are renamed
`propBalance` and `propFee`.

Live setup needs the public `owner` address in the settings and an active key. The
`owner` is the address of the wallet that owns the account. It uses the `0x` prefix
followed by 40 hexadecimal characters. The settings take no `agent`. A settings file
that has one stops every start, including `desk --keys`, until the field is removed. The
message says to remove `agent` and that the trading key is now chosen with
`desk --keys`. Keep every private value out of the settings, the command line, files and
chat.

Run `desk --keys` to open the key screen. It opens before the workspace and exits when
you leave it. `desk XMR --trade` opens it first when no usable key exists. It goes on
into the workspace only when you leave with a usable active key. Otherwise Desk exits
with "No trading key". A usable key has an active choice for the selected network, a
name on that network, and a Keychain item for its address. The check reads the item's
attributes only. It never reads the key.

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
exits. `desk --keys` exits when you leave it, so it is the cleaner way to make a key.

The list shows each key with its name, address, network, approval name and an active
mark. Up and Down move. Enter selects the key. `r` renames it, `n` makes a new key, `s`
restores a key from its seed and `q` leaves. A key without a Keychain item shows "not in
Keychain" and cannot be selected. Nothing is ever deleted. A switch is refused while
the current key has a saved order, or while another Desk holds that key's order lock.
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
`~/.local/lib/desk-keychain.py` and writes the launcher with the Python it will use.
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
its blocking reason, and Desk does not ask again until you restart it. Desk verifies
that the loaded key matches the active key and that the venue reports delegation to the
owner.

Making a key does not register a trading account. The wallet that owns the account must
approve the key's address at the venue as a named API key. Approve it under the approval
name that Desk shows. The name is `desk-` followed by the first ten hexadecimal
characters of the address after `0x`. Each key has its own name, so up to three Desk
keys can be approved at once. Until the approval is in place the key cannot trade, and
the role check must pass. An approval can expire. A key whose approval has expired
cannot trade until the owner approves it again. Keep funding and account setup separate
from the key screen.

Desk's local signer accepts only its supported immediate orders. It rejects fund
transfers, withdrawals, permission changes and leverage changes before producing
a signature. Hyperliquid's standard agent permission is broader and includes
transfers between balances within the same account. The local restriction does
not constrain a key used outside Desk. See the venue's
[agent-transfer documentation](https://hyperliquid.gitbook.io/hyperliquid-docs/for-developers/api/exchange-endpoint#agent-send-asset).

Start `desk XMR --trade`, or select Live and confirm its account mode. XMR is the
venue's Monero symbol. The panel identifies the current trading mode and market.
Check the owner in the local settings and the address on the key screen before enabling
this mode.
Test network access uses `"network": "testnet"`. Public feeds and the live worker
use the same network. An account, delegation, funding and any real order remain
separate user actions.

XMR orders require observed leverage of five times. Select that setting at the venue.
Desk preserves the observed margin mode and never signs a leverage change.
Other markets require positive integer leverage within their market's limit. The
margin mode and leverage are captured with each order and rechecked before sending.
Changing them at the venue invalidates a captured order. Missing or stale position
data remains unavailable and is never interpreted as a flat account.

The worker dates account freshness from the start of each account read and requests
another read immediately after an order completes. Quiet active-account data is
refreshed through one outstanding read on the existing account connection. Late,
wrong-market or replaced-connection replies cannot renew the selected account.

The workspace uses immediate orders with the selected value and a price limit
no more than 0.1 percent from the captured best quote. Available depth can produce
a partial fill or no fill. In real trading, an opposite fill reduces the existing
position before its remainder opens the other direction. In prop trading,
Buy spends stables and Sell is capped at held tokens. The selected value excludes fees.
Calculations retain full precision. Compact dollar figures are rounded for display.
Best and Target orders are no longer exposed in the normal workspace.

Live orders keep durable recovery state under the chosen state directory,
separated by network and delegated signer. One process holds each account lock.
The captured quantity, limit and account settings are compared with the current
full plan before queuing and immediately before signing. An uncertain response
blocks further orders while status is checked. The pending order is not resent.
A parsed top-level `err` reply keeps the saved order pending for a status lookup.
A matching final status settles it as usual. A parsed `unknownOid` lookup releases
a saved order only when the local wall time read before the lookup was sent is past
the signed `expiresAfter` plus a fixed 60-second margin. Before then it stays saved,
because an action still in flight may not show in the lookup yet. After the margin,
an order with an `err` reply releases as rejected without owner and role checks.
Any other order releases as "not placed" under the owner and role checks below.
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
remove saved pause state to bypass it.

Quoted prices and book depth normally expire after one second. Position and active
account data normally expire after two seconds. Optional `quoteAgeMs` and
`positionAgeMs` accept 100 to 5,000 milliseconds. A healthy public socket
does not establish account readiness. The blocking reason and final order outcome
remain visible in the controls and latest action notice.

Restart an already running Desk after installing a new build. It continues using
its previous loaded program until restarted.
