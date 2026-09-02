import { writeFileSync } from "node:fs";
const DIR = "fixtures/";
const want = new Set(["l2Book", "bbo", "trades", "candle", "activeAssetCtx", "pong", "subscriptionResponse"]);
const got = {};
const ws = new WebSocket("wss://api.hyperliquid.xyz/ws");
const subs = [
  { type: "l2Book", coin: "XMR" }, { type: "bbo", coin: "XMR" }, { type: "trades", coin: "XMR" },
  { type: "candle", coin: "XMR", interval: "1m" }, { type: "activeAssetCtx", coin: "XMR" },
];
ws.onopen = () => { for (const s of subs) ws.send(JSON.stringify({ method: "subscribe", subscription: s })); ws.send(JSON.stringify({ method: "ping" })); };
ws.onmessage = (ev) => {
  const text = String(ev.data);
  let ch = "unknown"; try { ch = JSON.parse(text).channel; } catch {}
  if (want.has(ch) && !got[ch]) { got[ch] = text; writeFileSync(DIR + ch + ".json", text); console.log("got", ch, text.length, "bytes"); }
  if (Object.keys(got).length === want.size) finish();
};
ws.onerror = (e) => { console.log("error", e.message); };
function finish() { ws.close(); console.log("done", Object.keys(got).join(",")); setTimeout(() => process.exit(0), 100); }
setTimeout(() => { console.log("timeout, have", Object.keys(got).join(",")); finish(); }, 25000);
