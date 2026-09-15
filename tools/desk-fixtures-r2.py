#!/usr/bin/env python3
# Turns a recording (round2.json) and a candle file (candles_round2.json) into
# the desk test fixtures: r2_mids.json, r2_books.json, r2_coarse.json,
# r2_trades.json, r2_ctx.json, r2_candles.json, written to <dir>/fixtures_r2/.
# Usage: python3 desk-fixtures-r2.py <dir>
import json, os, sys
S = sys.argv[1] if len(sys.argv) > 1 else sys.exit("usage: desk-fixtures-r2.py <dir>  (holds round2.json + candles_round2.json)")
OUT = f"{S}/fixtures_r2"
os.makedirs(OUT, exist_ok=True)
R = json.load(open(f"{S}/round2.json"))
C = json.load(open(f"{S}/candles_round2.json"))


def px(x):
    return f"{x:.3f}".rstrip("0").rstrip(".")


def sz(x):
    return f"{x:.4f}".rstrip("0").rstrip(".")


mids = {}
for coin, arr in R["mids"].items():
    by = {}
    for t, m in arr:
        by[t // 1000] = [t, round(m, 4)]
    mids[coin] = [by[k] for k in sorted(by)]
json.dump(mids, open(f"{OUT}/r2_mids.json", "w"), separators=(",", ":"))


def levels_json(levels):
    return [[{"px": px(p), "sz": sz(s), "n": n} for p, s, n in side] for side in levels]


full = R["books"]["full"]
step = max(1, len(full) // 12)
picked = full[::step][-12:]
msgs = [{"channel": "l2Book", "data": {"coin": "XMR", "time": t, "levels": levels_json(lv)}} for t, lv in picked]
json.dump(msgs, open(f"{OUT}/r2_books.json", "w"), separators=(",", ":"))
coarse = {}
for key, k in (("sf4", 4), ("sf3", 3), ("sf2", 2)):
    t, lv = R["books"][key][-1]
    coarse[str(k)] = {"coin": "XMR", "time": t, "levels": levels_json(lv)}
json.dump(coarse, open(f"{OUT}/r2_coarse.json", "w"), separators=(",", ":"))
trades = [{"coin": "XMR", "side": side, "px": px(p), "sz": sz(s), "time": t, "hash": "0x0", "tid": i, "users": ["0x0", "0x0"]}
          for i, (t, side, p, s) in enumerate(R["trades"])]
json.dump({"channel": "trades", "data": trades}, open(f"{OUT}/r2_trades.json", "w"), separators=(",", ":"))
json.dump({"channel": "activeAssetCtx", "data": {"coin": "XMR", "ctx": R["ctx"]}}, open(f"{OUT}/r2_ctx.json", "w"), separators=(",", ":"))
COUNTS = {"1m": 120, "5m": 60, "15m": 100, "1h": 170, "4h": 185, "1d": 130}
candles = {}
for coin, series in C.items():
    candles[coin] = {}
    for interval, arr in series.items():
        keep = arr[-COUNTS[interval]:]
        candles[coin][interval] = [{"t": t, "T": t + 1, "s": coin, "i": interval, "o": px(o), "h": px(h), "l": px(l), "c": px(c), "v": sz(v), "n": 1}
                                   for t, o, h, l, c, v in keep]
json.dump(candles, open(f"{OUT}/r2_candles.json", "w"), separators=(",", ":"))
print("end", R["end"], "start", R["start"])
print("mids", {k: len(v) for k, v in mids.items()}, "books", len(msgs), "trades", len(trades))
print("last mid XMR", mids["XMR"][-1], "picked book times", [m["data"]["time"] for m in msgs][-2:])
for f in sorted(os.listdir(OUT)):
    print(f, os.path.getsize(f"{OUT}/{f}"))
