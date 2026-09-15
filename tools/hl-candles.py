#!/usr/bin/env python3
# Fetches six Hyperliquid candle series (1m, 5m, 15m, 1h, 4h, 1d) for three coins
# (XMR, BTC, PAXG) via the public /info candleSnapshot endpoint and writes them
# to candles_round2.json in the output directory.
# Usage: python3 hl-candles.py <out_dir>
import json, subprocess, sys, time
S = sys.argv[1] if len(sys.argv) > 1 else sys.exit("usage: hl-candles.py <out_dir>")
URL = "https://api.hyperliquid.xyz/info"
now = int(time.time() * 1000)
out = {}
for coin in ("XMR", "BTC", "PAXG"):
    out[coin] = {}
    for interval, days in (("1m", 4), ("5m", 18), ("15m", 53), ("1h", 209), ("4h", 900), ("1d", 4000)):
        body = json.dumps({"type": "candleSnapshot", "req": {"coin": coin, "interval": interval, "startTime": now - days * 86400000, "endTime": now}})
        txt = subprocess.run(["curl", "-s", "-m", "40", "-X", "POST", URL, "-H", "Content-Type: application/json", "-d", body], capture_output=True, text=True).stdout
        d = json.loads(txt)
        out[coin][interval] = [[c["t"], float(c["o"]), float(c["h"]), float(c["l"]), float(c["c"]), float(c["v"])] for c in d]
        print(coin, interval, len(d), "earliest", time.strftime("%Y-%m-%d", time.gmtime(d[0]["t"] / 1000)) if d else "none")
        time.sleep(0.3)
json.dump(out, open(f"{S}/candles_round2.json", "w"), separators=(",", ":"))
print("saved", now)
