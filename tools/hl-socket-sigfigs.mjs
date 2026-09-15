#!/usr/bin/env node
// Proves the Hyperliquid websocket streams XMR's l2Book at coarser resolutions:
// subscribes to nSigFigs 3, 2, and full in parallel, and logs each push's bid
// span + a count of pushes per span over a 12s window. No arguments, no output
// file; prints to stdout only.
const ws = new WebSocket("wss://api.hyperliquid.xyz/ws");
const seen = {};
ws.onopen = () => {
  ws.send(JSON.stringify({ method: "subscribe", subscription: { type: "l2Book", coin: "XMR", nSigFigs: 3 } }));
  ws.send(JSON.stringify({ method: "subscribe", subscription: { type: "l2Book", coin: "XMR", nSigFigs: 2 } }));
  ws.send(JSON.stringify({ method: "subscribe", subscription: { type: "l2Book", coin: "XMR" } }));
};
ws.onmessage = (ev) => {
  const m = JSON.parse(String(ev.data));
  if (m.channel === "subscriptionResponse") { console.log("ack", JSON.stringify(m.data)); return; }
  if (m.channel === "l2Book") {
    const b = m.data.levels[0];
    const key = b[0].px + ".." + b[b.length - 1].px;
    const span = (Number(b[0].px) - Number(b[b.length - 1].px)).toFixed(2);
    seen[span] = (seen[span] || 0) + 1;
    if (seen[span] <= 2) console.log("book push, bid span", span, "levels", b.length, "best", b[0].px, "time", m.data.time);
  } else if (m.channel === "error") console.log("error", JSON.stringify(m.data));
};
setTimeout(() => { console.log("counts by bid span", JSON.stringify(seen)); ws.close(); setTimeout(() => process.exit(0), 100); }, 12000);
