# Local fault records

Run the interactive workspace to collect records in
`~/.local/state/desk/diagnostics/events-0.jsonl`. A custom `--state` directory
changes that root. Each line is a separate JSON record. Older files rotate through
`events-1.jsonl` to `events-3.jsonl`, with a two MiB limit per file. Only one
interactive writer can own the separate `diagnostics.lock`.

The public recorder remains independent. Its existing records cannot reconstruct
past keyboard selections. New diagnostic records record selection requests without
the typed value, the validation result, and the committed public market name.
Follow a switch through its selection, history and first presented frame records.
Check the event names in [Diagnostics.scala](../src/Diagnostics.scala) when
inspecting a particular build.

Quote health requires a strictly newer selected-market quote. Pongs and reference
updates cannot keep the selected quote healthy. Source age and monotonic elapsed
time both contribute. A health record appears every 30 seconds and on a change.
Input, frame construction, frame-lock waits and terminal writes of at least 50 ms
are recorded separately. A two-second frame stall and a ten-second unresolved
selection are separate events.

Execution records show fixed readiness categories and elapsed waits on a change
or every 30 seconds. They distinguish missing market details, prices, account
data, leverage, storage, a pending order and readiness. Unrecognized blocking
reasons remain a fixed `blocked` category. No worker message or account value is saved.

The writer has a bounded 1,024-record queue. Input and rendering do not write to
disk. Lost records are counted, and a stopped writer is reported as unavailable.
Shutdown drains for at most 500 ms. An incomplete drain marks logging unavailable.
The small `!logs` header warning appears when logging fails, including when another
interactive process owns the lock. This is a diagnostic signal, not an order result.
Short order result lock contention is retried without recording a diagnostic. Any order
result failure before its record is saved is recorded, shown as "Order result #n not saved",
and marked by `!results` for the rest of the session. Failures after saving are diagnostic only.

Records contain fixed event names, counts, durations, validated public market names
and the running archive hash. They contain no account identifiers, balances, order
payloads, typed text, exception messages or stack traces. Direct symbolic links in
the log targets are refused. Test development runs identify their build as
development. Historical records do not prove which build a different process uses.

These records continue while Desk runs. They support later debugging and do not
start a separate remote monitoring service or automatically submit trades.
