#!/usr/bin/env python3
# Fetches Hyperliquid's XMR order book at full resolution and at 4, 3, and 2
# significant figures, all at the same instant, and prints how each resolution's
# buckets round against the full book (best bid/ask, sizes inside each bucket).
# Runs 3 rounds, 1s apart. No arguments; prints to stdout only, writes nothing.
import json, subprocess, time
from concurrent.futures import ThreadPoolExecutor

URL = "https://api.hyperliquid.xyz/info"


def fetch(body):
    out = subprocess.run(["curl", "-s", "-m", "20", "-X", "POST", URL, "-H", "Content-Type: application/json", "-d", json.dumps(body)], capture_output=True, text=True).stdout
    return json.loads(out)


for _ in range(3):
    with ThreadPoolExecutor(max_workers=4) as ex:
        full, sf4, sf3, sf2 = ex.map(fetch, [
            {"type": "l2Book", "coin": "XMR"},
            {"type": "l2Book", "coin": "XMR", "nSigFigs": 4},
            {"type": "l2Book", "coin": "XMR", "nSigFigs": 3},
            {"type": "l2Book", "coin": "XMR", "nSigFigs": 2},
        ])
    fb, fa = full["levels"]
    print("times", full["time"], sf4["time"], sf3["time"], sf2["time"])
    print("full  best bid", fb[0]["px"], "best ask", fa[0]["px"], "bids to", fb[-1]["px"], "asks to", fa[-1]["px"])
    for name, d in (("sf4", sf4), ("sf3", sf3), ("sf2", sf2)):
        b, a = d["levels"]
        print(name, " best bid", b[0]["px"], "best ask", a[0]["px"], "bids to", b[-1]["px"], "asks to", a[-1]["px"], "| first 3 bids", [(l["px"], l["sz"]) for l in b[:3]], "first 3 asks", [(l["px"], l["sz"]) for l in a[:3]])
    # size check: sum full levels inside the first sf4 bucket
    b4 = sf4["levels"][0][0]
    p4 = float(b4["px"])
    inside_floor = sum(float(l["sz"]) for l in fb if p4 <= float(l["px"]) < p4 + 0.1)
    inside_ceil = sum(float(l["sz"]) for l in fb if p4 - 0.1 < float(l["px"]) <= p4)
    print("sf4 first bid bucket", b4["px"], b4["sz"], "| full sizes in [p, p+0.1):", round(inside_floor, 3), "in (p-0.1, p]:", round(inside_ceil, 3))
    a4 = sf4["levels"][1][0]
    q4 = float(a4["px"])
    inside_floor = sum(float(l["sz"]) for l in fa if q4 <= float(l["px"]) < q4 + 0.1)
    inside_ceil = sum(float(l["sz"]) for l in fa if q4 - 0.1 < float(l["px"]) <= q4)
    print("sf4 first ask bucket", a4["px"], a4["sz"], "| full sizes in [p, p+0.1):", round(inside_floor, 3), "in (p-0.1, p]:", round(inside_ceil, 3))
    print()
    time.sleep(1)
