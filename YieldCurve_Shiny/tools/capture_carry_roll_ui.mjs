import { spawn } from "node:child_process";
import { writeFile } from "node:fs/promises";

const appPort = Number(process.argv[2] || 7855);
const debugPort = Number(process.argv[3] || 9230);
const outPath = process.argv[4] || "C:\\Users\\PC\\Desktop\\R_git\\YieldCurve_Shiny\\current_carry_roll_applied.png";

const chrome = "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe";
const profile = `C:\\Users\\PC\\Desktop\\R_git\\YieldCurve_Shiny\\chrome-cdp-profile-${debugPort}`;
const appUrl = `http://127.0.0.1:${appPort}`;

const browser = spawn(chrome, [
  "--headless=new",
  "--disable-gpu",
  "--no-first-run",
  "--disable-background-networking",
  "--remote-allow-origins=*",
  `--remote-debugging-port=${debugPort}`,
  `--user-data-dir=${profile}`,
  "--window-size=1512,982",
  "about:blank",
], { windowsHide: true, stdio: "ignore" });

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));

async function waitForJson(url, timeoutMs = 20000) {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    try {
      const response = await fetch(url);
      if (response.ok) return response.json();
    } catch {}
    await sleep(300);
  }
  throw new Error(`Timed out waiting for ${url}`);
}

function cdp(wsUrl) {
  let id = 1;
  const ws = new WebSocket(wsUrl.replace("ws://localhost:", "ws://127.0.0.1:"));
  const pending = new Map();
  ws.onmessage = event => {
    const message = JSON.parse(event.data);
    if (message.id && pending.has(message.id)) {
      const { resolve, reject } = pending.get(message.id);
      pending.delete(message.id);
      if (message.error) reject(new Error(JSON.stringify(message.error)));
      else resolve(message);
    }
  };
  const ready = new Promise((resolve, reject) => {
    ws.onopen = resolve;
    ws.onerror = reject;
  });
  return {
    ready,
    send(method, params = undefined) {
      const current = id++;
      const payload = params === undefined ? { id: current, method } : { id: current, method, params };
      return new Promise((resolve, reject) => {
        pending.set(current, { resolve, reject });
        ws.send(JSON.stringify(payload));
      });
    },
    close() {
      ws.close();
    },
  };
}

try {
  await waitForJson(`http://127.0.0.1:${debugPort}/json/version`);
  const target = await fetch(`http://127.0.0.1:${debugPort}/json/new?${encodeURIComponent(appUrl)}`, { method: "PUT" }).then(r => r.json());
  const client = cdp(target.webSocketDebuggerUrl);
  await client.ready;
  await client.send("Page.enable");
  await client.send("Runtime.enable");
  await sleep(6000);

  const script = `
(async () => {
  const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
  const click = selector => {
    const el = document.querySelector(selector);
    if (!el) throw new Error('Missing selector: ' + selector);
    el.click();
  };
  click('a[data-value="Carry & Roll"]');
  await sleep(1800);
  click('#calculate_carry');
  await sleep(5000);
  click('#calculate_curve_trade');
  await sleep(6000);
  window.scrollTo(0, 0);
  await sleep(1200);
  return {
    activeTab: document.querySelector('.nav-link.active')?.textContent?.trim(),
    title: document.querySelector('.tab-pane.active h2')?.textContent?.trim(),
    carry: document.querySelector('#carry_value')?.textContent?.trim(),
    trade: document.querySelector('#trade_total_pnl')?.textContent?.trim(),
    status: document.querySelector('#carry_progress')?.textContent?.trim()
  };
})()
`;
  const evalResult = await client.send("Runtime.evaluate", {
    expression: script,
    awaitPromise: true,
    returnByValue: true,
  });
  const state = evalResult.result.result.value;
  if (state.activeTab !== "Carry & Roll") throw new Error(`Expected Carry & Roll tab, got ${state.activeTab}`);

  const shot = await client.send("Page.captureScreenshot", {
    format: "png",
    captureBeyondViewport: false,
    fromSurface: true,
  });
  await writeFile(outPath, Buffer.from(shot.result.data, "base64"));
  console.log(JSON.stringify(state));
  client.close();
} finally {
  browser.kill();
}
