# Configure live trading

The [workspace guide](workspace.md) describes the layout, paper account, text
assistant and trade buttons. Every live order needs a visible review and an
explicit Send. Starting the program preserves observed leverage and margin mode.

The default settings file is `~/.config/desk/config.json`. `--config /path` selects
another file. It must be a private regular file with permissions `600`. An absent
default file permits public viewing and paper trading with these defaults:

```json
{
  "small": "500",
  "big": "1000",
  "target": "1000",
  "network": "mainnet",
  "references": ["BTC", "PAXG"],
  "paperBalance": "10000",
  "paperFee": "0.0005"
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
from its readings, and lists swaps as the tape timed at the reading that saw
them. There is no candle history before Desk starts. Paper and live orders are
not available on an on-chain market. The bitcoin and gold references keep coming
from the venue.

Amounts and age limits are quoted numeric strings. Small and big are total order
value before fees. Target has its own value ceiling.
Paper balance applies when creating an account. Existing saved balances remain
in place. Paper fee is the fraction charged on each simulated fill.

Live setup requires the public `owner` address and its approved delegated `agent`
address on the selected network. Both use the `0x` prefix followed by 40 hexadecimal
characters. Add those public addresses to the settings. Keep the private signing
key out of the settings, command line, files and chat.

Create a generic password item through the Mac system key store. Its service is
`desk`, its account is the delegated agent address, and its password is that
agent's private hexadecimal signing key. A custom `keyService` setting changes
the service name. Use the protected store's on-screen interface for this step.
A denied or timed-out read leaves signing unavailable. Desk verifies that the
loaded key matches the agent and that the venue reports delegation to the owner.

Desk's local signer accepts only its supported immediate orders. It rejects fund
transfers, withdrawals, permission changes and leverage changes before producing
a signature. Hyperliquid's standard agent permission is broader and includes
transfers between balances within the same account. The local restriction does
not constrain a key used outside Desk. See the venue's
[agent-transfer documentation](https://hyperliquid.gitbook.io/hyperliquid-docs/for-developers/api/exchange-endpoint#agent-send-asset).

Use **Allow Once** when the Mac asks for the key. **Always Allow** can retain access
for future sessions, as described in [Apple's access guide](https://support.apple.com/en-us/guide/keychain-access/kyca1243/mac).
Desk keeps its loaded trading key in memory until that process closes. Starting
a new chat does not close a running Desk process. The current key-store read has
an eight-second timeout. A denied or timed-out read requires restarting Desk to
try again.

For new local keys, `python3 tools/keychain_wallet.py --check-access` checks the
Mac permission settings without creating a key. After selecting the account role,
`--create owner` creates a wallet key under service `desk-owner`. `--create agent`
creates a separate trading key under service `desk`. The account name is the
generated public address. The helper requires the installed Python libraries
`cryptography`, `pycryptodome` and `eth-account`.

The helper writes only public setup records to
`~/.config/desk/keychain-owner.jsonl` or `keychain-agent.jsonl`. It records the
intended item before adding it, then appends completion after checking its access
settings. Existing records and key-store items are never overwritten. If setup
stops, preserve that record and inspect its named item before retrying. The private
value stays in process memory and the protected store. Library memory copies can
remain until the helper exits.

Creating either key does not register a trading account. The wallet that owns the
account must separately approve Desk's trading address at the venue. Keep funding
and account setup separate from the key-creation command.

Start `desk XMR --trade`, or select Live and confirm its account mode. XMR is the
venue's Monero symbol. The live panel, review and Status name the network and shortened owner address.
Check the full owner and agent in the local settings before enabling this mode.
Test network access uses `"network": "testnet"`. Public feeds and the live worker
use the same network. An account, delegation, funding and any real order remain
separate user actions.

Observed leverage must be a positive integer within the market's limit. Its
margin mode and leverage are captured in each review and rechecked before sending.
Changing them at the venue invalidates a pending review. Missing or stale position
data remains unavailable and is never interpreted as a flat account.

Hard uses its configured value and a price limit no more than 0.1 percent from the
captured best quote. Best chooses a smaller size with a limit strictly inside that
range. Target takes visible volume through its selected price, within the target
value ceiling. All three are immediate orders that can fill partly or not at all.
Visible depth does not promise a fill. An opposite fill reduces an existing
position before its remainder opens the other direction.

The review shows exact quantity and limit. Its maximum value excludes fees. The
live account's fee rate is not currently loaded, so the review states that limit.
Paper mode shows its configured fee estimate. A leading `~` denotes a rounded
display number. Calculations retain full precision. Very large exact numbers
can use scientific notation.

Live orders keep durable recovery state under the chosen state directory,
separated by network and delegated signer. One process holds each account lock.
The accepted review is compared against the current full plan before it is queued
and again immediately before its signed request is sent. An uncertain response
blocks further orders while status is checked. The pending order is not resent.
Do not delete saved recovery state to release this block. Resolve it using the
account's order history.

A wallet change pauses live orders for that owner and network across Desk processes
sharing the same home directory, including ones using another state directory.
The controls show the pause and refuse new orders. The pause survives a restart.
Finish or resolve the wallet change before using the replacement account. Do not
remove saved pause state to bypass it.

Quoted prices and book depth normally expire after one second. Position and active
account data normally expire after two seconds. Optional `quoteAgeMs` and
`positionAgeMs` accept 100 to 5,000 milliseconds. A healthy public socket
does not establish account readiness. The blocking reason and final order outcome
remain visible in the controls and message history.

Restart an already running Desk after installing a new build. It continues using
its previous loaded program until restarted.
