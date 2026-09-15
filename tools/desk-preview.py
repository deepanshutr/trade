#!/usr/bin/env python3
# Renders the desk tracking view's preview frames (fine: 1s/column, coarse:
# 1d/column) from a recording (round2.json, e.g. from hl-record-markets.mjs)
# and a candle file (candles_round2.json, e.g. from hl-candles.py). Both are
# read from, and the outputs written back to, the same directory:
# preview3_fine.txt, preview3_coarse.txt, preview3_cells.json (styled cells,
# consumed by desk-preview-page.py).
# Usage: python3 desk-preview.py <dir>
import json, math, datetime, sys
S = sys.argv[1] if len(sys.argv) > 1 else sys.exit("usage: desk-preview.py <dir>  (holds round2.json + candles_round2.json; outputs written here too)")
R = json.load(open(f"{S}/round2.json"))
C = json.load(open(f"{S}/candles_round2.json"))

COLS, LEFT, SCALE, PRICE_ROWS, VOL_ROWS = 100, 30, 9, 14, 2
CELLS = COLS - LEFT - SCALE
DOTS = CELLS * 2
BARW = 21
H = 12
BITS = {(0, 0): 0x01, (0, 1): 0x02, (0, 2): 0x04, (1, 0): 0x08, (1, 1): 0x10, (1, 2): 0x20, (0, 3): 0x40, (1, 3): 0x80}
FULL, LHALF, RHALF, MARK = chr(0x28FF), chr(0x2847), chr(0x28B8), chr(0x28FF)
HORIZONS = [("15m", "1m", 15), ("1h", "1m", 60), ("4h", "5m", 48), ("1d", "15m", 96), ("1w", "1h", 168), ("1M", "4h", 180)]


def rnd(x):
    return math.floor(x + 0.5)


def decs(h):
    return max(0, min(6, 5 - len(str(int(h)))))


def rjust(t, w):
    return (" " * max(0, w - len(t)) + t)[:w]


def fmt_px(p, dec):
    return f"{p:.{dec}f}"


# ---------- styled grid ----------
class Grid:
    def __init__(self, rows):
        self.rows = [[[" ", ""] for _ in range(COLS)] for _ in range(rows)]

    def put(self, r, c, text, style=""):
        for i, ch in enumerate(text):
            if 0 <= c + i < COLS:
                self.rows[r][c + i] = [ch, style]

    def text(self):
        return ["".join(ch for ch, _ in row) for row in self.rows]

    def cells(self):
        return self.rows


class Canvas:
    def __init__(self, cells, rows):
        self.w, self.h = cells * 2, rows * 4
        self.bits = [[0] * cells for _ in range(rows)]
        self.style = [[""] * cells for _ in range(rows)]

    def dot(self, x, y, style=""):
        if 0 <= x < self.w and 0 <= y < self.h:
            self.bits[y // 4][x // 2] |= BITS[(x % 2, y % 4)]
            if style or not self.style[y // 4][x // 2]:
                self.style[y // 4][x // 2] = style

    def run(self, x, y0, y1, style=""):
        for y in range(min(y0, y1), max(y0, y1) + 1):
            self.dot(x, y, style)

    def chars(self):
        return [[(chr(0x2800 + b) if b else " ", self.style[r][c]) for c, b in enumerate(row)] for r, row in enumerate(self.bits)]


# ---------- data ----------
end_ms = R["end"]
end_s = end_ms // 1000


def per_second(mids):
    by = {}
    for t, m in mids:
        by[t // 1000] = m
    return by


def fine_columns(coin, first_s, n):
    by = per_second(R["mids"][coin])
    out, last = [], None
    for k in range(n):
        s = first_s + k
        if s in by:
            last = by[s]
        out.append(last)
    return out


def daily(coin):
    return {c[0] // 86400000: c for c in C[coin]["1d"]}


def pearson(xs, ys):
    n = len(xs)
    if n < 3:
        return None
    mx, my = sum(xs) / n, sum(ys) / n
    sxy = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    sxx = sum((x - mx) ** 2 for x in xs)
    syy = sum((y - my) ** 2 for y in ys)
    return sxy / math.sqrt(sxx * syy) if sxx > 0 and syy > 0 else None


def corr(coin, interval, n):
    a = {c[0]: c[4] for c in C["XMR"][interval]}
    b = {c[0]: c[4] for c in C[coin][interval]}
    ts = sorted(set(a) & set(b))[-(n + 1):]
    ra = [math.log(a[t2] / a[t1]) for t1, t2 in zip(ts, ts[1:])]
    rb = [math.log(b[t2] / b[t1]) for t1, t2 in zip(ts, ts[1:])]
    return pearson(ra, rb)


def track(v, h=H, mark=MARK):
    t = list("." * h + "|" + "." * h)
    if v is not None:
        t[h + max(-h, min(h, rnd(v * h)))] = mark
    return "".join(t)


# ---------- books ----------
books = {k: v[-1][1] for k, v in R["books"].items() if v}
WIDTH = {"full": 0.0, "sf4": 0.1, "sf3": 1.0, "sf2": 10.0}
ORDER = ["full", "sf4", "sf3", "sf2"]


def merged_depth(axis_lo, axis_hi, rows):
    """Return per row depth and side, spreading buckets over the rows they cover."""
    def yrow(p):
        return max(0, min(rows - 1, rnd((axis_hi - p) / (axis_hi - axis_lo) * (rows - 1))))
    depth = [0.0] * rows
    isbid = [False] * rows
    outer_bid, outer_ask = None, None
    for key in ORDER:
        if key not in books:
            continue
        bids, asks = books[key]
        w = WIDTH[key]
        for p, s, _ in bids:
            lo, hi = p, p + w
            if outer_bid is not None and hi > outer_bid:
                continue
            r1, r2 = yrow(hi), yrow(lo)
            if hi > axis_hi and lo > axis_hi or hi < axis_lo and lo < axis_lo:
                continue
            for r in range(r1, r2 + 1):
                depth[r] += s / (r2 - r1 + 1)
                isbid[r] = True
        for p, s, _ in asks:
            lo, hi = p - w, p
            if outer_ask is not None and lo < outer_ask:
                continue
            if lo > axis_hi or hi < axis_lo:
                continue
            r1, r2 = yrow(hi), yrow(lo)
            for r in range(r1, r2 + 1):
                depth[r] += s / (r2 - r1 + 1)
        if bids:
            outer_bid = min(outer_bid if outer_bid is not None else 1e18, bids[-1][0])
        if asks:
            outer_ask = max(outer_ask if outer_ask is not None else -1e18, asks[-1][0])
    return depth, isbid


def bar_cells(n_dots, bid):
    full, half = divmod(n_dots, 2)
    if bid:
        s = FULL * full + (LHALF if half else "")
        return s.ljust(BARW)[:BARW]
    s = (RHALF if half else "") + FULL * full
    return rjust(s, BARW)


# ---------- frame ----------
def frame(zoom, title):
    g = Grid(30)
    fine = zoom < 60
    if fine:
        first_s = end_s - DOTS + 1
        xmr = fine_columns("XMR", first_s, DOTS)
        cols = [(v, v, v, v) if v is not None else None for v in xmr]
        others = {c: fine_columns(c, first_s, DOTS) for c in ("BTC", "PAXG")}
        vol = [0.0] * DOTS
        for t, side, px, sz in R["trades"]:
            k = t // 1000 - first_s
            if 0 <= k < DOTS:
                vol[k] += sz
        labels = [datetime.datetime.fromtimestamp(first_s + k).strftime("%H:%M:%S") for k in (0, DOTS // 2, DOTS - 1)]
        zoom_label = "1s/col"
    else:
        today = end_ms // 86400000
        first_d = today - DOTS + 1
        dx = daily("XMR")
        cols, vol = [], []
        for k in range(DOTS):
            c = dx.get(first_d + k)
            cols.append((c[1], c[3], c[2], c[4]) if c else None)
            vol.append(c[5] if c else 0.0)
        others = {}
        for coin in ("BTC", "PAXG"):
            d = daily(coin)
            others[coin] = [d[first_d + k][4] if (first_d + k) in d else None for k in range(DOTS)]
        labels = [datetime.datetime.utcfromtimestamp((first_d + k) * 86400).strftime("%Y-%m-%d") for k in (0, DOTS // 2, DOTS - 1)]
        zoom_label = "1d/col"
    # axis: xmr content plus the finest book
    prices = [p for c in cols if c for p in (c[1], c[2])]
    fb, fa = books["full"]
    prices += [l[0] for l in fb] + [l[0] for l in fa]
    lo, hi = min(prices), max(prices)
    pad = (hi - lo) * 0.05 or hi * 0.001
    lo, hi = lo - pad, hi + pad
    dec = decs(hi)

    def yrow(p):
        return max(0, min(PRICE_ROWS - 1, rnd((hi - p) / (hi - lo) * (PRICE_ROWS - 1))))

    def ydot(p):
        return max(0, min(PRICE_ROWS * 4 - 1, rnd((hi - p) / (hi - lo) * (PRICE_ROWS * 4 - 1))))

    cv = Canvas(CELLS, PRICE_ROWS)
    # faint lines first: normalized to the first visible xmr price
    base_idx = next((i for i, c in enumerate(cols) if c), None)
    for coin, style in (("BTC", "btc"), ("PAXG", "gold")):
        series = others[coin]
        b0 = series[base_idx] if base_idx is not None else None
        if b0 is None:
            b0 = next((v for v in series if v is not None), None)
        if b0 is None or base_idx is None:
            continue
        xmr0 = cols[base_idx][0]
        for x, v in enumerate(series):
            if v is None:
                continue
            y = ydot(xmr0 * v / b0)
            keep = (x % 4 < 2) if coin == "BTC" else (x % 3 == 0)
            if keep:
                cv.dot(x, y, style)
    prev = None
    newest = None
    for x, c in enumerate(cols):
        if c is None:
            continue
        f, l, h, la = c
        cv.run(x, ydot(prev if prev is not None else f), ydot(f), "main")
        cv.run(x, ydot(l), ydot(h), "main")
        prev = la
        newest = (x // 2, ydot(la) // 4)
    chart = cv.chars()
    # book depth
    depth, isbid = merged_depth(lo, hi, PRICE_ROWS)
    nz = sorted(d for d in depth if d > 0)
    u = nz[(len(nz) - 1) // 2] if nz else 1.0
    r95 = nz[min(len(nz) - 1, int(0.95 * (len(nz) - 1)))] if nz else 1.0
    if r95 <= u:
        r95 = nz[-1] if nz else 1.0
    mid = None
    if fb and fa:
        mid = (fb[0][0] + fa[0][0]) / 2
    mid_row = yrow(mid) if mid is not None else None
    best_rows = {yrow(fb[0][0]), yrow(fa[0][0])} if fb and fa else set()
    # stats
    ctx = R["ctx"]
    mid_txt = f"{mid:.3f}".rstrip("0").rstrip(".")
    pct = (mid / float(ctx["prevDayPx"]) - 1) * 100

    def short(x):
        return f"{x/1e9:.1f}B" if x >= 1e9 else f"{x/1e6:.1f}M" if x >= 1e6 else f"{x/1e3:.1f}k" if x >= 1e3 else f"{x:.1f}"

    stats = f"XMR  {mid_txt}  mark {ctx['markPx']}  orc {ctx['oraclePx']}  24h {pct:+.1f}%  fund {float(ctx['funding'])*100:.4f}%  OI {short(float(ctx['openInterest']))}  vol {short(float(ctx['dayNtlVlm']))}  live 0.2s"
    g.put(0, 0, stats[:COLS])
    for r in range(PRICE_ROWS):
        price = hi - (hi - lo) * r / (PRICE_ROWS - 1)
        label = rjust(fmt_px(price, dec), 8) + (">" if r == mid_row else " ")
        row_style = "bold" if r in best_rows or r == mid_row else ""
        g.put(1 + r, 0, label, row_style)
        s = depth[r]
        if s > 0:
            raw = rnd(BARW * 2 * math.log(1 + s / u) / math.log(1 + r95 / u))
            n = max(1, min(BARW * 2, raw))
            bar = bar_cells(n, isbid[r])
            if raw > BARW * 2:
                bar = (bar[:-1] + ">") if isbid[r] else (">" + bar[1:])
            g.put(1 + r, 9, bar, "bid" if isbid[r] else "ask")
        for c, (ch, st) in enumerate(chart[r]):
            if newest and (c, r) == newest and ch != " ":
                st = "bold"
            g.put(1 + r, LEFT + c, ch, st)
        if r == mid_row and mid is not None:
            g.put(1 + r, LEFT + CELLS, rjust("<" + mid_txt, SCALE), "bold")
        elif r in (0, PRICE_ROWS // 2, PRICE_ROWS - 1):
            g.put(1 + r, LEFT + CELLS, rjust(fmt_px(price, dec), SCALE))
    # volume
    vc = Canvas(CELLS, VOL_ROWS)
    vmax = max(vol) or 1.0
    for x, v in enumerate(vol):
        if v > 0:
            d = max(1, rnd(v / vmax * 7))
            c = cols[x]
            st = "up" if c and c[3] >= c[0] else "down"
            vc.run(x, 8 - d, 7, st)
    vch = vc.chars()
    g.put(1 + PRICE_ROWS, 0, rjust("volume", LEFT - 1) + " ", "dim")
    for i in range(VOL_ROWS):
        for c, (ch, st) in enumerate(vch[i]):
            g.put(1 + PRICE_ROWS + i, LEFT + c, ch, st)
    g.put(1 + PRICE_ROWS, LEFT + CELLS, rjust("v " + short(vmax), SCALE))
    axis_r = 1 + PRICE_ROWS + VOL_ROWS
    g.put(axis_r, 0, rjust(zoom_label, LEFT - 1) + " ", "dim")
    wl = len(labels[0])
    for at, text in zip((0, CELLS // 2 - wl // 2, CELLS - wl), labels):
        g.put(axis_r, LEFT + at, text)
    # gauges
    def imbalance(b):
        bids, asks = b
        q = (bids[0][1] - asks[0][1]) / (bids[0][1] + asks[0][1])
        w = [0.7 ** k for k in range(10)]
        wb = sum(l[1] * wk for l, wk in zip(bids[:10], w))
        wa = sum(l[1] * wk for l, wk in zip(asks[:10], w))
        return (wb - wa) / (wb + wa), q

    ring = R["books"]["full"]
    lenses_book = [("now", 0), ("10s", 10), ("30s", 30), ("1m", 60), ("5m", 300), ("15m", 900)]
    lenses_tape = [("10s", 10), ("1m", 60), ("5m", 300), ("15m", 900), ("1h", 3600), ("4h", 14400)]
    run_s = (end_ms - R["start"]) / 1000
    gr = axis_r + 1
    for i, (name, secs) in enumerate(lenses_book):
        sel = [b for t, b in ring if secs == 0 or t >= end_ms - secs * 1000]
        if secs == 0 or not sel:
            sel = [ring[-1][1]]
        ds = [imbalance(b)[0] for b in sel]
        qs = [imbalance(b)[1] for b in sel]
        d, q = sum(ds) / len(ds), sum(qs) / len(qs)
        label_style = "dim" if secs > run_s else ""
        g.put(gr + i, 0, "BOOK ")
        g.put(gr + i, 5, rjust(name, 4), label_style)
        g.put(gr + i, 10, track(d), "")
        g.put(gr + i, 10 + H + max(-H, min(H, rnd(d * H))), MARK, "bid" if d >= 0 else "ask")
        g.put(gr + i, 36, f"D{d:+.2f} Q{q:+.2f}")
        hz, interval, n = HORIZONS[i]
        rr = corr("BTC", interval, n)
        g.put(gr + i, 51, "BTC ", "btc")
        g.put(gr + i, 56, rjust(hz, 3))
        g.put(gr + i, 60, track(rr))
        if rr is not None:
            g.put(gr + i, 60 + H + max(-H, min(H, rnd(rr * H))), MARK, "btc")
        g.put(gr + i, 86, f"r{rr:+.2f}" if rr is not None else "r --")
    for i, (name, secs) in enumerate(lenses_tape):
        ts = [t for t in R["trades"] if t[0] > end_ms - secs * 1000]
        buy = sum(t[3] for t in ts if t[1] == "B")
        sell = sum(t[3] for t in ts if t[1] != "B")
        f = (buy - sell) / (buy + sell) if buy + sell else None
        label_style = "dim" if secs > run_s else ""
        r0 = gr + 6 + i
        g.put(r0, 0, "TAPE ")
        g.put(r0, 5, rjust(name, 4), label_style)
        g.put(r0, 10, track(f))
        if f is not None:
            g.put(r0, 10 + H + max(-H, min(H, rnd(f * H))), MARK, "bid" if f >= 0 else "ask")
        g.put(r0, 36, (f"F{f:+.2f}" if f is not None else "F --") + f" n={len(ts)}")
        hz, interval, n = HORIZONS[i]
        rr = corr("PAXG", interval, n)
        g.put(r0, 51, "GOLD", "gold")
        g.put(r0, 56, rjust(hz, 3))
        g.put(r0, 60, track(rr))
        if rr is not None:
            g.put(r0, 60 + H + max(-H, min(H, rnd(rr * H))), MARK, "gold")
        g.put(r0, 86, f"r{rr:+.2f}" if rr is not None else "r --")
    return g


out = {}
for zoom, name in ((1, "fine"), (86400, "coarse")):
    g = frame(zoom, name)
    lines = g.text()
    assert all(len(l) == COLS for l in lines) and len(lines) == 30
    open(f"{S}/preview3_{name}.txt", "w").write("\n".join(lines) + "\n")
    out[name] = g.cells()
    print("=== " + name)
    print("\n".join(lines))
    print()
json.dump(out, open(f"{S}/preview3_cells.json", "w"))
