#!/usr/bin/env node
// Records Hyperliquid market data over the public websocket for SECONDS seconds:
// per-second mids for three markets (XMR, BTC, PAXG), XMR's trades, XMR's book
// at four resolutions (full, and nSigFigs 4/3/2), and XMR's asset context.
// Writes round2.json into the output directory.
// Usage: node hl-record-markets.mjs <out_dir>
import { writeFileSync } from "node:fs";
const DIR = process.argv[2];
if (!DIR) { console.error("usage: hl-record-markets.mjs <out_dir>"); process.exit(1); }
const SECONDS = 150;
const start = Date.now();
const mids = { XMR: [], BTC: [], PAXG: [] };
const trades = [];
const books = { full: [], sf4: [], sf3: [], sf2: [] };
let ctx = null;
const ws = new WebSocket("wss://api.hyperliquid.xyz/ws");
ws.onopen = () => {
  const subs = [
    { type: "bbo", coin: "XMR" }, { type: "bbo", coin: "BTC" }, { type: "bbo", coin: "PAXG" },
    { type: "trades", coin: "XMR" }, { type: "activeAssetCtx", coin: "XMR" },
    { type: "l2Book", coin: "XMR" }, { type: "l2Book", coin: "XMR", nSigFigs: 4 },
    { type: "l2Book", coin: "XMR", nSigFigs: 3 }, { type: "l2Book", coin: "XMR", nSigFigs: 2 },
  ];
  for (const s of subs) ws.send(JSON.stringify({ method: "subscribe", subscription: s }));
};
const ping = setInterval(() => ws.send(JSON.stringify({ method: "ping" })), 50000);
function spanKey(levels) {
  const b = levels[0];
  const span = Number(b[0].px) - Number(b[b.length - 1].px);
  if (span > 100) return "sf2";
  if (span > 10) return "sf3";
  if (span > 1.2) return "sf4";
  return "full";
}
ws.onmessage = (ev) => {
  let m; try { m = JSON.parse(String(ev.data)); } catch { return; }
  if (m.channel === "bbo" && m.data.bbo[0] && m.data.bbo[1]) {
    mids[m.data.coin].push([m.data.time, (Number(m.data.bbo[0].px) + Number(m.data.bbo[1].px)) / 2]);
  } else if (m.channel === "trades") {
    for (const t of m.data) trades.push([t.time, t.side, Number(t.px), Number(t.sz)]);
  } else if (m.channel === "l2Book") {
    const key = spanKey(m.data.levels);
    books[key].push([m.data.time, m.data.levels.map(side => side.map(l => [Number(l.px), Number(l.sz), l.n]))]);
  } else if (m.channel === "activeAssetCtx") {
    ctx = m.data.ctx;
  }
};
setTimeout(() => {
  clearInterval(ping);
  ws.close();
  writeFileSync(DIR + "/round2.json", JSON.stringify({ start, mids, trades, books, ctx, end: Date.now() }));
  console.log("mids", Object.fromEntries(Object.entries(mids).map(([k, v]) => [k, v.length])), "trades", trades.length,
    "books", Object.fromEntries(Object.entries(books).map(([k, v]) => [k, v.length])), "ctx", ctx ? "yes" : "no");
  setTimeout(() => process.exit(0), 100);
}, SECONDS * 1000);
