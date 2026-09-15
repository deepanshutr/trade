#!/usr/bin/env python3
# Builds the colour and greyscale HTML preview page (desk_round_two.html) from
# the cells JSON (preview3_cells.json) that desk-preview.py writes.
# Usage: python3 desk-preview-page.py <dir>
import json, html, sys
S = sys.argv[1] if len(sys.argv) > 1 else sys.exit("usage: desk-preview-page.py <dir>  (holds preview3_cells.json; desk_round_two.html written here too)")
cells = json.load(open(f"{S}/preview3_cells.json"))

COLOUR = {"": "", "main": "", "bold": "b", "dim": "d", "bid": "bid", "ask": "ask", "btc": "btc", "gold": "gold", "up": "bid", "down": "ask"}
GREY = {"": "", "main": "", "bold": "b", "dim": "d", "bid": "", "ask": "", "btc": "d", "gold": "d", "up": "", "down": ""}


def render(rows, mapping):
    out = []
    for row in rows:
        line, cur, buf = [], None, []
        for ch, st in row:
            cls = mapping.get(st, "")
            if cls != cur:
                if buf:
                    line.append(f'<span class="{cur}">{html.escape("".join(buf))}</span>' if cur else html.escape("".join(buf)))
                cur, buf = cls, []
            buf.append(ch)
        if buf:
            line.append(f'<span class="{cur}">{html.escape("".join(buf))}</span>' if cur else html.escape("".join(buf)))
        out.append("".join(line).rstrip())
    return "\n".join(out)


sections = []
for key, heading, note in (
    ("fine", "One second per column", "The last two minutes. The book fills every visible row from four resolutions of the venue's depth. Bitcoin and gold ride the same axis as percent moves from the window's first second."),
    ("coarse", "One day per column", "The last 122 days from daily candles, with the 10 dollar book buckets spread across the visible range. The lens rows are the same, they read time not zoom."),
):
    rows = cells[key]
    sections.append(f"""
<section>
  <h2>{heading}</h2>
  <p class="note">{note}</p>
  <div class="pair">
    <figure>
      <figcaption>In colour</figcaption>
      <pre class="frame colour">{render(rows, COLOUR)}</pre>
    </figure>
    <figure>
      <figcaption>Greyscale, as the terminal draws it today</figcaption>
      <pre class="frame grey">{render(rows, GREY)}</pre>
    </figure>
  </div>
</section>""")

page = f"""<title>Desk Round Two</title>
<meta name="description" content="Two preview frames of the desk tracking view after the second round of feedback, in colour and in greyscale.">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;600&display=swap">
<style>
  :root {{
    --ground: #f3f5f7; --panel: #ffffff; --ink: #1c2128; --dim: #7b8592; --rule: #d9dee4;
    --bid: #1e8f3e; --ask: #c43d2b; --btc: #b8720f; --gold: #9a7f0c; --accent: #2a6f8f;
    color-scheme: light dark;
  }}
  @media (prefers-color-scheme: dark) {{
    :root:not([data-theme="light"]) {{
      --ground: #0f1216; --panel: #161a20; --ink: #d9dde3; --dim: #6c7683; --rule: #262c34;
      --bid: #3fb950; --ask: #f0553f; --btc: #f0a53a; --gold: #e3c04b; --accent: #6fc3c9;
    }}
  }}
  :root[data-theme="dark"] {{
    --ground: #0f1216; --panel: #161a20; --ink: #d9dde3; --dim: #6c7683; --rule: #262c34;
    --bid: #3fb950; --ask: #f0553f; --btc: #f0a53a; --gold: #e3c04b; --accent: #6fc3c9;
  }}
  body {{ margin: 0; background: var(--ground); color: var(--ink); font-family: "IBM Plex Sans", "Helvetica Neue", Arial, sans-serif; font-size: 15px; line-height: 1.5; }}
  main {{ max-width: 920px; margin: 0 auto; padding: 32px 20px 56px; display: grid; gap: 28px; }}
  header {{ display: grid; gap: 8px; }}
  h1 {{ margin: 0; font-size: 26px; font-weight: 600; letter-spacing: -0.01em; text-wrap: balance; }}
  h2 {{ margin: 0 0 4px; font-size: 18px; font-weight: 600; }}
  .lede {{ max-width: 62ch; margin: 0; color: var(--ink); }}
  .note {{ max-width: 62ch; margin: 0 0 12px; color: var(--dim); }}
  .legend {{ display: flex; flex-wrap: wrap; gap: 6px 18px; font-size: 13px; color: var(--dim); }}
  .legend span::before {{ content: "\\2588"; margin-right: 6px; }}
  .legend .bid::before {{ color: var(--bid); }} .legend .ask::before {{ color: var(--ask); }}
  .legend .btc::before {{ color: var(--btc); }} .legend .gold::before {{ color: var(--gold); }}
  .legend .d::before {{ color: var(--dim); }} .legend .b::before {{ color: var(--ink); }}
  .pair {{ display: grid; gap: 16px; }}
  figure {{ margin: 0; }}
  figcaption {{ font-size: 12px; letter-spacing: 0.06em; text-transform: uppercase; color: var(--dim); margin-bottom: 6px; }}
  .frame {{ margin: 0; padding: 12px 14px; background: var(--panel); border: 1px solid var(--rule); border-radius: 4px; overflow-x: auto;
    font-family: Menlo, "DejaVu Sans Mono", Consolas, monospace; font-size: 12.5px; line-height: 1.3; color: var(--ink); font-variant-numeric: tabular-nums; }}
  .frame .b {{ font-weight: 700; color: var(--ink); }}
  .frame .d {{ color: var(--dim); }}
  .frame .bid {{ color: var(--bid); }} .frame .ask {{ color: var(--ask); }}
  .frame .btc {{ color: var(--btc); }} .frame .gold {{ color: var(--gold); }}
  .frame.grey {{ color: var(--ink); }}
  footer {{ font-size: 13px; color: var(--dim); max-width: 62ch; }}
</style>
<main>
  <header>
    <h1>Desk Round Two</h1>
    <p class="lede">The tracking view after your second round: no big money section, braille bars instead of hash marks, the book spread across the whole visible range, bitcoin and gold on the chart with correlation lenses, and history back to 2023 at coarse zoom. Both frames are drawn from live Monero data recorded this afternoon.</p>
    <div class="legend">
      <span class="bid">buyers, buying flow, up columns</span>
      <span class="ask">sellers, selling flow, down columns</span>
      <span class="btc">bitcoin line and lens</span>
      <span class="gold">gold line and lens (PAXG)</span>
      <span class="b">bold: the current price row, the newest cell</span>
      <span class="d">dim: lenses the run has not covered yet</span>
    </div>
    <p class="note">In greyscale the two extra lines are dim, bitcoin dashed and gold dotted. The book bars, the volume dots and the gauge markers are braille blocks in both.</p>
  </header>
  {"".join(sections)}
  <footer>Correlation r is the Pearson correlation of log returns over the horizon: 15 minutes and 1 hour from one minute candles, 4 hours from five minute, 1 day from fifteen minute, 1 week from hourly, 1 month from four hour candles. Gold is the venue's PAXG perpetual, one token per ounce.</footer>
</main>
"""
open(f"{S}/desk_round_two.html", "w").write(page)
print("wrote", len(page), "bytes")
