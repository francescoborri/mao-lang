"use strict";
// Main-thread UI. All interpreter work happens in worker.js, and the whole
// trace stays on the OCaml side: this file only ever holds the window of data
// it is displaying (one page of logs, one page of memory, one environment).
//
//   - rpc(op, ...args)  request/response over postMessage
//   - nav(type, job)    serial queue for navigation work; a newer job of the
//                       same type replaces a pending one, so holding a key
//                       down never builds a backlog in the worker
//   - "Cancel" terminates the worker (the only way to stop a running wasm
//     computation) and starts a fresh one.
(() => {
  const $ = (id) => document.getElementById(id);
  const src = $("src"), mirror = $("mirror"), gutter = $("gutter");
  const runBtn = $("run"), cancelBtn = $("cancel"), formatBtn = $("format");
  const statusEl = $("status"), diagEl = $("diag"), maxLogsEl = $("maxLogs");

  const KINDS = ["decl", "assign", "store", "branch", "def", "call", "invoke"];
  const KIND_LABEL = {
    decl: "declarations", assign: "assignments", store: "array stores",
    branch: "if / while", def: "definitions", call: "function calls",
    invoke: "procedure calls",
  };
  const MEM_PAGE = 128;
  const MEM_MATCHES = 200;
  const STORAGE_KEY = "mao.source";

  const clamp = (x, lo, hi) => Math.max(lo, Math.min(hi, x));
  const fmtN = (n) => n.toLocaleString("en-US");
  const hex = (a) => "0x" + a.toString(16);

  function h(tag, attrs, ...kids) {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs || {})) {
      if (v !== false && v != null) e.setAttribute(k, v);
    }
    for (const c of kids) e.append(c);
    return e;
  }

  // ------------------------------------------------------------------ state
  const S = {
    state: "loading",        // loading | idle | running | failed
    sid: 0,                  // bumped on every run/cancel: invalidates async work
    runSource: null,         // source text the current trace was produced from
    total: 0,                // log entries in the trace
    viewTotal: 0,            // entries in the current (filtered) view
    disabledKinds: new Set(),
    counts: {},
    pageSize: 100, page: 0, rows: [],
    selected: -1, selPos: -1, selRow: null,
    pendingDelta: 0,
    tab: "env", envRows: [],
    memStart: 0, memBound: 0, memTarget: -1,
    stepRange: null,         // {start, end} of the selected entry in the source
    diag: null, diagSource: null,
  };

  // -------------------------------------------------------------- worker RPC
  let worker = null, nextId = 1;
  const pending = new Map();
  let startedAt = 0, ticker = null;

  function rpc(op, ...args) {
    return new Promise((resolve, reject) => {
      if (!worker) return reject(new Error("cancelled"));
      const id = nextId++;
      pending.set(id, { resolve, reject });
      worker.postMessage({ id, op, args });
    });
  }

  function rejectAll(reason) {
    for (const p of pending.values()) p.reject(new Error(reason));
    pending.clear();
  }

  function report(e) {
    if (e && e.message === "cancelled") return;
    console.error(e);
    setDiag("Internal error: " + ((e && e.message) || e), null, src.value);
  }

  // Serial queue. A pending job of a given type is replaced by a newer one.
  const nav = (() => {
    let busy = false;
    const queue = new Map();
    async function pump() {
      if (busy) return;
      busy = true;
      while (queue.size) {
        const [type, job] = queue.entries().next().value;
        queue.delete(type);
        try { await job(); } catch (e) { report(e); }
      }
      busy = false;
    }
    return (type, job) => { queue.set(type, job); pump(); };
  })();

  // ------------------------------------------------------------ status/state
  function setState(next, message) {
    S.state = next;
    runBtn.disabled = next !== "idle";
    formatBtn.disabled = next !== "idle";
    cancelBtn.disabled = next !== "running";
    maxLogsEl.disabled = next === "running";
    statusEl.textContent = message;
  }

  const elapsed = () => ((performance.now() - startedAt) / 1000).toFixed(1);
  function stopTicker() { clearInterval(ticker); ticker = null; }

  function startWorker(message) {
    setState("loading", message || "Loading interpreter…");
    const w = new Worker("worker.js");
    worker = w;
    w.onmessage = (e) => {
      if (w !== worker) return;
      const m = e.data;
      if (m.type === "ready") {
        setState("idle", "Ready (Ctrl/Cmd+Enter to run)");
      } else if (m.type === "fatal") {
        fail(m.message);
      } else if (m.id !== undefined) {
        const p = pending.get(m.id);
        if (!p) return;
        pending.delete(m.id);
        if (m.ok) p.resolve(m.result); else p.reject(new Error(m.error));
      }
    };
    w.onerror = (e) => { if (w === worker) fail(e.message || "The worker crashed."); };
  }

  function fail(message) {
    stopTicker();
    rejectAll("cancelled");
    if (worker) worker.terminate();
    worker = null;
    setState("failed", "The interpreter is unavailable.");
    setDiag(message, null, src.value);
  }

  // ------------------------------------------------------------------ editor
  // The textarea sits on top of a transparent "mirror" holding the same text,
  // in which the highlighted ranges are wrapped in <span>s.
  const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const LH = 21, PAD = 14;

  function renderMirror(ranges) {
    const text = src.value, full = text + "\n", len = full.length;
    const segs = ranges.filter(Boolean).map((r) => {
      const s = clamp(r.start, 0, len - 1);
      return { s, e: clamp(Math.max(r.end, s + 1), s + 1, len), cls: r.cls };
    });
    const pts = [...new Set([0, len, ...segs.flatMap((g) => [g.s, g.e])])].sort((a, b) => a - b);
    let html = "";
    for (let i = 0; i + 1 < pts.length; i++) {
      const a = pts[i], b = pts[i + 1];
      const cls = segs.filter((g) => g.s <= a && g.e >= b).map((g) => g.cls).join(" ");
      const chunk = esc(full.slice(a, b));
      html += cls ? `<span class="${cls}">${chunk}</span>` : chunk;
    }
    mirror.innerHTML = html;
    mirror.scrollTop = src.scrollTop;
    mirror.scrollLeft = src.scrollLeft;
  }

  function renderGutter() {
    let lines = 1;
    for (let i = src.value.indexOf("\n"); i !== -1; i = src.value.indexOf("\n", i + 1)) lines++;
    let t = "1";
    for (let i = 2; i <= lines; i++) t += "\n" + i;
    gutter.textContent = t;
    gutter.scrollTop = src.scrollTop;
  }

  function revealLine(line) {
    const top = (line - 1) * LH;
    if (top < src.scrollTop || top > src.scrollTop + src.clientHeight - 3 * LH) {
      src.scrollTop = Math.max(0, top - src.clientHeight / 3);
    }
  }

  // Everything derived from "what the source looked like when the trace/error
  // was produced": once the text differs, highlights and messages are stale.
  function refreshDecor() {
    const stale = S.runSource !== null && src.value !== S.runSource;
    const diagLive = S.diag && src.value === S.diagSource;
    renderMirror([
      !stale && S.stepRange ? { ...S.stepRange, cls: "hl" } : null,
      diagLive && S.diag.range ? { ...S.diag.range, cls: "err" } : null,
    ]);
    if (diagLive) {
      diagEl.textContent = S.diag.text;
      diagEl.className = "diag";
      diagEl.hidden = false;
    } else if (stale) {
      diagEl.textContent = "The program changed since the last run: the trace below still refers to the old source. Press Run to refresh it.";
      diagEl.className = "diag warn";
      diagEl.hidden = false;
    } else {
      diagEl.hidden = true;
    }
  }

  function setDiag(text, loc, source) {
    S.diag = { text, range: loc ? { start: loc.start, end: loc.end } : null };
    S.diagSource = source;
    refreshDecor();
    if (loc) revealLine(loc.line);
  }

  function locDiag(err, source, prefix) {
    setDiag(`${prefix || ""}${err.message} (line ${err.line}, column ${err.col})`, err, source);
  }

  function replaceText(text) {
    // execCommand keeps the browser's undo history, so Format can be undone.
    src.focus();
    src.select();
    let ok = false;
    try { ok = document.execCommand("insertText", false, text); } catch (_) { /* fall through */ }
    if (!ok || src.value !== text) {
      src.value = text;
      src.dispatchEvent(new Event("input"));
    }
    src.setSelectionRange(0, 0);
    src.scrollTop = 0;
  }

  let saveTimer = null;
  src.addEventListener("input", () => {
    renderGutter();
    refreshDecor();
    clearTimeout(saveTimer);
    saveTimer = setTimeout(() => {
      try { localStorage.setItem(STORAGE_KEY, src.value); } catch (_) { /* ignore */ }
    }, 400);
  });
  src.addEventListener("scroll", () => {
    mirror.scrollTop = src.scrollTop;
    mirror.scrollLeft = src.scrollLeft;
    gutter.scrollTop = src.scrollTop;
  });

  // --------------------------------------------------------------- run/format
  const maxLogs = () => Math.max(0, Math.floor(Number(maxLogsEl.value) || 0));

  function clearTrace() {
    S.runSource = null; S.total = 0; S.viewTotal = 0; S.rows = []; S.page = 0;
    S.selected = -1; S.selPos = -1; S.selRow = null; S.stepRange = null;
    S.disabledKinds = new Set(); S.counts = {}; S.envRows = [];
    S.memStart = 0; S.memBound = 0; S.memTarget = -1; S.pendingDelta = 0;
    S.diag = null; S.diagSource = null;
    $("q").value = ""; $("memq").value = "";
    $("chips").replaceChildren();
    $("logs").replaceChildren();
    $("envBody").replaceChildren();
    $("memBody").replaceChildren();
    $("memResults").hidden = true;
    $("memRange").textContent = "";
    $("inspTitle").textContent = "";
    showEmpty("Run a program to see its trace.");
    renderPos(); renderPager(); refreshDecor();
  }

  async function run() {
    if (S.state !== "idle") return;
    const source = src.value;
    S.sid++;
    const sid = S.sid;
    clearTrace();
    startedAt = performance.now();
    setState("running", "Running… 0.0 s");
    ticker = setInterval(() => { statusEl.textContent = `Running… ${elapsed()} s`; }, 100);
    let res;
    try {
      res = await rpc("run", source, maxLogs());
    } catch (e) {
      stopTicker();
      if (sid === S.sid) { setState("idle", "Ready"); report(e); }
      return;
    }
    stopTicker();
    if (sid !== S.sid) return;
    S.runSource = source;
    S.total = res.total;
    S.viewTotal = res.total;
    S.counts = res.kinds;
    setState("idle", `${fmtN(res.total)} log entries in ${elapsed()} s` +
      (res.maxDepth ? `, max call depth ${res.maxDepth}` : "") +
      (res.error ? " (stopped by an error)" : ""));
    if (res.error) locDiag(res.error, source);
    buildChips();
    if (res.total === 0) {
      showEmpty("The run produced no log entries.");
      refreshDecor();
    } else {
      nav("filter", () => applyFilter(true)); // loads the last page, selects the last entry
    }
  }

  async function format() {
    if (S.state !== "idle") return;
    const source = src.value;
    let r;
    try { r = await rpc("format", source); } catch (e) { report(e); return; }
    if (src.value !== source) return; // edited while formatting
    if (r.ok) {
      S.diag = null;
      if (r.output !== source) replaceText(r.output);
      statusEl.textContent = r.output === source ? "Already formatted" : "Formatted";
      refreshDecor();
    } else {
      locDiag(r.error, source, "Cannot format: ");
    }
  }

  function cancel() {
    if (S.state !== "running") return;
    stopTicker();
    S.sid++;
    rejectAll("cancelled");
    worker.terminate();
    worker = null;
    clearTrace();
    startWorker("Cancelled. Restarting interpreter…");
    setDiag("Run cancelled.", null, src.value);
  }

  // -------------------------------------------------------------------- trace
  function showEmpty(text) {
    $("logsEmpty").textContent = text;
    $("logsEmpty").hidden = false;
  }

  function buildChips() {
    const box = $("chips");
    box.replaceChildren();
    for (const k of KINDS) {
      if (!S.counts[k]) continue;
      const b = h("button", { class: `chip k-${k}`, "aria-pressed": "true", "data-kind": k },
        `${KIND_LABEL[k]} ${fmtN(S.counts[k])}`);
      b.addEventListener("click", () => {
        if (S.disabledKinds.has(k)) S.disabledKinds.delete(k); else S.disabledKinds.add(k);
        b.setAttribute("aria-pressed", String(!S.disabledKinds.has(k)));
        nav("filter", () => applyFilter(false));
      });
      box.append(b);
    }
  }

  // Sends the filter to the worker, then re-selects (or selects the last entry
  // on the first call) so that the table shows the page around the selection.
  async function applyFilter(initial) {
    const sid = S.sid;
    if (!S.total) return;
    const kinds = KINDS.filter((k) => !S.disabledKinds.has(k));
    const r = await rpc("filter", kinds, $("q").value);
    if (sid !== S.sid) return;
    S.viewTotal = r.count;
    await selectNow(S.selected >= 0 && !initial ? S.selected : S.total - 1, { reload: true });
  }

  async function loadPage(page) {
    const sid = S.sid;
    const maxPage = Math.max(0, Math.ceil(S.viewTotal / S.pageSize) - 1);
    page = clamp(page, 0, maxPage);
    const r = await rpc("logs", page * S.pageSize, S.pageSize);
    if (sid !== S.sid) return;
    S.page = page; S.rows = r.rows; S.viewTotal = r.total;
    renderRows();
    renderPager();
  }

  function renderRows() {
    const frag = document.createDocumentFragment();
    for (const r of S.rows) {
      const tr = h("tr", { class: r.i === S.selected ? "sel" : null, "data-i": r.i });
      tr.append(
        h("td", { class: "num" }, String(r.i)),
        h("td", { style: `padding-left:${0.6 + r.depth * 0.9}rem` },
          h("span", { class: `dot k-${r.kind}` }), r.name),
        h("td", null, r.descr),
        h("td", { class: "muted" }, `${r.line}:${r.col}`));
      frag.append(tr);
    }
    $("logs").replaceChildren(frag);
    if (S.rows.length) {
      $("logsEmpty").hidden = true;
    } else {
      showEmpty(S.total ? "No entries match the filter." : "Run a program to see its trace.");
    }
    markSelected();
  }

  // Like scrollIntoView, but only scrolls the given box (never the page).
  function keepVisible(box, el) {
    const b = box.getBoundingClientRect(), r = el.getBoundingClientRect();
    const th = box.querySelector("thead");
    const top = b.top + (th ? th.offsetHeight : 0);
    if (r.top < top) box.scrollTop -= top - r.top;
    else if (r.bottom > b.bottom) box.scrollTop += r.bottom - b.bottom;
  }

  function markSelected() {
    let found = null;
    for (const tr of $("logs").children) {
      const on = Number(tr.dataset.i) === S.selected;
      tr.classList.toggle("sel", on);
      if (on) found = tr;
    }
    if (found) keepVisible($("logsScroll"), found);
  }

  function renderPager() {
    const pages = Math.max(1, Math.ceil(S.viewTotal / S.pageSize));
    $("pgCount").textContent = fmtN(pages);
    $("pgNum").value = S.page + 1;
    $("pgNum").max = pages;
    $("pgFirst").disabled = $("pgPrev").disabled = S.page <= 0;
    $("pgNext").disabled = $("pgLast").disabled = S.page >= pages - 1;
  }

  function renderPos() {
    const r = S.selRow;
    const has = S.total > 0 && r;
    $("pos").textContent = !S.total ? "no trace"
      : !has ? `${fmtN(S.total)} entries`
      : `#${fmtN(S.selected)} of ${fmtN(S.total - 1)}` + (S.selPos < 0 ? " (hidden by filter)" : "");
    $("first").disabled = $("prev").disabled = !has || (S.selPos >= 0 && S.selPos === 0);
    $("next").disabled = $("last").disabled = !has || (S.selPos >= 0 && S.selPos >= S.viewTotal - 1);
    $("over").disabled = !has || S.selected >= S.total - 1;
    $("out").disabled = !has || r.outEnd < 0;
    $("inspTitle").textContent = has ? `#${S.selected} ${r.name}` : "";
  }

  async function selectNow(index, opts) {
    const sid = S.sid;
    if (!S.total) return;
    index = clamp(Math.floor(index), 0, S.total - 1);
    const r = await rpc("row", index);
    if (sid !== S.sid) return;
    S.selected = index; S.selRow = r.row; S.selPos = r.pos;
    const reload = opts && opts.reload;
    if (r.pos >= 0) {
      const page = Math.floor(r.pos / S.pageSize);
      if (reload || page !== S.page || (!S.rows.length && S.viewTotal)) await loadPage(page);
    } else if (reload) {
      const ins = -r.pos - 1;
      await loadPage(S.viewTotal ? Math.floor(Math.min(ins, S.viewTotal - 1) / S.pageSize) : 0);
    }
    if (sid !== S.sid) return;
    markSelected();
    renderPos();
    S.stepRange = { start: r.row.start, end: r.row.end };
    refreshDecor();
    if (S.runSource === src.value) revealLine(r.row.line);
    await refreshInspector();
  }

  async function selectByPos(p) {
    const sid = S.sid;
    if (!S.viewTotal) return;
    p = clamp(p, 0, S.viewTotal - 1);
    const page = Math.floor(p / S.pageSize);
    if (page !== S.page || !S.rows.length) await loadPage(page);
    if (sid !== S.sid) return;
    const row = S.rows[p - S.page * S.pageSize];
    if (row) await selectNow(row.i);
  }

  // Step in the current (possibly filtered) view; deltas accumulate while busy.
  function stepView(delta) {
    if (!S.total) return;
    S.pendingDelta += delta;
    nav("step", async () => {
      const d = S.pendingDelta;
      S.pendingDelta = 0;
      if (!d || !S.viewTotal) return;
      // selPos < 0: the selection is filtered out; -selPos-1 is where it would be.
      const ins = -S.selPos - 1;
      const p = S.selPos >= 0 ? S.selPos + d : (d > 0 ? ins + d - 1 : ins + d);
      await selectByPos(p);
    });
  }

  const goTo = (i) => nav("select", () => selectNow(i));
  const stepOver = () => { if (S.selRow) goTo(S.selRow.callEnd >= 0 ? S.selRow.callEnd : S.selected + 1); };
  const stepOut = () => { if (S.selRow && S.selRow.outEnd >= 0) goTo(S.selRow.outEnd); };
  const toPage = (p) => nav("page", () => loadPage(p));

  async function exportCsv() {
    const sid = S.sid, total = S.viewTotal;
    if (!total) return;
    const q = (s) => /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
    const parts = ["index,event,description,line,col,end_line,end_col,depth\n"];
    for (let off = 0; off < total; off += 5000) {
      statusEl.textContent = `Exporting… ${fmtN(Math.min(off, total))} / ${fmtN(total)}`;
      const r = await rpc("logs", off, 5000);
      if (sid !== S.sid) return;
      for (const x of r.rows) {
        parts.push([x.i, q(x.name), q(x.descr), x.line, x.col, x.endLine, x.endCol, x.depth].join(",") + "\n");
      }
    }
    const a = h("a", { href: URL.createObjectURL(new Blob(parts, { type: "text/csv" })), download: "mao-trace.csv" });
    document.body.append(a);
    a.click();
    a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 10000);
    statusEl.textContent = `Exported ${fmtN(total)} entries`;
  }

  // --------------------------------------------------------------- inspector
  function setTab(name) {
    S.tab = name;
    for (const t of ["env", "mem"]) {
      $("tab-" + t).setAttribute("aria-selected", String(t === name));
      $("panel-" + t).hidden = t !== name;
    }
    nav("inspector", refreshInspector);
  }

  async function refreshInspector() {
    if (S.selected < 0) return;
    if (S.tab === "env") await loadEnv(); else await loadMem();
  }

  async function loadEnv() {
    const sid = S.sid, i = S.selected;
    const [cur, prev] = await Promise.all([rpc("env", i), i > 0 ? rpc("env", i - 1) : null]);
    if (sid !== S.sid) return;
    const sig = (r) => `${r.value}|${r.detail}|${r.addr}`;
    const before = new Map((prev ? prev.rows : []).map((r) => [r.name, sig(r)]));
    S.envRows = cur.rows.map((r) => ({ ...r, changed: before.get(r.name) !== sig(r) }));
    renderEnv();
  }

  function addrLink(addr, text) {
    return h("a", { class: "addr", "data-addr": addr, title: "Show in memory" }, text);
  }

  function renderEnv() {
    const q = $("envq").value.trim().toLowerCase();
    const rows = S.envRows.filter((r) =>
      !q || `${r.name} ${r.type} ${r.value} ${r.detail}`.toLowerCase().includes(q));
    const frag = document.createDocumentFragment();
    for (const r of rows) {
      const tr = h("tr", { class: r.changed ? "chg" : null });
      const value = h("td", null);
      if (r.kind === "array") {
        value.append(addrLink(Number(r.value), r.value), " ", h("span", { class: "detail" }, r.detail));
      } else {
        value.append(r.value);
      }
      tr.append(h("td", null, r.name), h("td", { class: "muted" }, r.type), value,
        h("td", null, r.addr >= 0 ? addrLink(r.addr, hex(r.addr)) : "—"));
      frag.append(tr);
    }
    $("envBody").replaceChildren(frag);
    $("envEmpty").hidden = rows.length > 0;
    $("envEmpty").textContent = S.envRows.length ? "No match." : "The environment is empty.";
  }

  async function loadMem(retry) {
    const sid = S.sid, i = S.selected;
    const p = await rpc("memory", i, S.memStart, MEM_PAGE);
    if (sid !== S.sid) return;
    S.memBound = p.bound;
    if (!p.cells.length && p.bound > 0 && S.memStart >= p.bound && !retry) {
      S.memStart = Math.floor((p.bound - 1) / MEM_PAGE) * MEM_PAGE; // memory was smaller back then
      return loadMem(true);
    }
    const frag = document.createDocumentFragment();
    for (const c of p.cells) {
      const cls = [c.changed ? "chg" : "", c.addr === S.memTarget ? "target" : ""].join(" ").trim();
      frag.append(h("tr", { class: cls || null },
        h("td", null, hex(c.addr), h("span", { class: "muted" }, ` (${c.addr})`)),
        h("td", null, c.value), h("td", { class: "muted" }, c.type), h("td", null, c.label)));
    }
    $("memBody").replaceChildren(frag);
    $("memEmpty").hidden = p.cells.length > 0;
    const last = p.cells.length ? p.cells[p.cells.length - 1].addr : p.start;
    $("memRange").textContent = p.cells.length ? `${hex(p.start)}–${hex(last)} of ${fmtN(p.bound)}` : "";
    $("memPrev").disabled = p.start <= 0;
    $("memNext").disabled = p.start + MEM_PAGE >= p.bound;
    const target = $("memBody").querySelector("tr.target");
    if (target) keepVisible(target.closest(".scroll"), target);
    if ($("memq").value.trim()) await memSearch();
    else $("memResults").hidden = true;
  }

  // Re-run on every step, so the search acts as a watch on a value.
  async function memSearch() {
    const sid = S.sid, q = $("memq").value.trim(), box = $("memResults");
    if (!q || S.selected < 0) { box.hidden = true; return; }
    const r = await rpc("memSearch", S.selected, q, MEM_MATCHES);
    if (sid !== S.sid) return;
    box.hidden = false;
    box.replaceChildren();
    if (!r.ok) { box.append(r.error); return; }
    if (!r.cells.length) { box.append("No cell holds this value."); return; }
    box.append(`${r.truncated ? `first ${MEM_MATCHES}` : r.cells.length} match${r.cells.length === 1 && !r.truncated ? "" : "es"}: `);
    for (const c of r.cells) {
      box.append(addrLink(c.addr, hex(c.addr) + (c.label ? ` ${c.label}` : "")));
    }
  }

  function showMem(addr) {
    S.memTarget = addr;
    S.memStart = Math.floor(addr / MEM_PAGE) * MEM_PAGE;
    if (S.tab !== "mem") setTab("mem"); else nav("inspector", refreshInspector);
  }

  function memPage(delta) {
    S.memStart = Math.max(0, S.memStart + delta * MEM_PAGE);
    nav("inspector", refreshInspector);
  }

  function goToAddress() {
    const t = $("addr").value.trim();
    const a = Number(t);
    if (t === "" || !Number.isInteger(a) || a < 0) { $("addr").setCustomValidity("Enter an address like 0x1f or 31"); $("addr").reportValidity(); return; }
    $("addr").setCustomValidity("");
    showMem(a);
  }

  // ------------------------------------------------------------------ wiring
  runBtn.addEventListener("click", run);
  cancelBtn.addEventListener("click", cancel);
  formatBtn.addEventListener("click", format);

  $("first").addEventListener("click", () => nav("step", () => selectByPos(0)));
  $("last").addEventListener("click", () => nav("step", () => selectByPos(S.viewTotal - 1)));
  $("prev").addEventListener("click", () => stepView(-1));
  $("next").addEventListener("click", () => stepView(1));
  $("over").addEventListener("click", stepOver);
  $("out").addEventListener("click", stepOut);
  $("export").addEventListener("click", () => nav("export", exportCsv));
  $("goto").addEventListener("keydown", (e) => {
    if (e.key === "Enter" && $("goto").value !== "") goTo(Number($("goto").value));
  });

  let filterTimer = null;
  $("q").addEventListener("input", () => {
    clearTimeout(filterTimer);
    filterTimer = setTimeout(() => nav("filter", () => applyFilter(false)), 250);
  });

  $("logs").addEventListener("click", (e) => {
    const tr = e.target.closest("tr");
    if (tr) goTo(Number(tr.dataset.i));
  });
  $("pgFirst").addEventListener("click", () => toPage(0));
  $("pgPrev").addEventListener("click", () => toPage(S.page - 1));
  $("pgNext").addEventListener("click", () => toPage(S.page + 1));
  $("pgLast").addEventListener("click", () => toPage(Infinity));
  $("pgNum").addEventListener("change", () => toPage(Number($("pgNum").value) - 1));
  $("pgSize").addEventListener("change", () => {
    S.pageSize = Number($("pgSize").value);
    if (S.total) nav("select", () => selectNow(S.selected, { reload: true }));
  });

  // Keyboard navigation while the trace has focus (click a row first).
  $("trace").addEventListener("keydown", (e) => {
    if (e.ctrlKey || e.metaKey || e.altKey || e.target.closest("input, select, textarea")) return;
    if (!S.total) return;
    const step = { ArrowDown: 1, j: 1, ArrowUp: -1, k: -1 }[e.key];
    if (step) stepView(step);
    else if (e.key === "Home") nav("step", () => selectByPos(0));
    else if (e.key === "End") nav("step", () => selectByPos(S.viewTotal - 1));
    else if (e.key === "PageDown") toPage(S.page + 1);
    else if (e.key === "PageUp") toPage(S.page - 1);
    else if (e.key === "o") stepOver();
    else if (e.key === "u") stepOut();
    else return;
    e.preventDefault();
  });

  document.addEventListener("keydown", (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === "Enter") { e.preventDefault(); run(); }
    else if (e.altKey && e.shiftKey && e.code === "KeyF") { e.preventDefault(); format(); }
  });

  $("tab-env").addEventListener("click", () => setTab("env"));
  $("tab-mem").addEventListener("click", () => setTab("mem"));
  $("envq").addEventListener("input", renderEnv);
  $("panel-env").addEventListener("click", (e) => {
    const a = e.target.closest("a.addr");
    if (a) showMem(Number(a.dataset.addr));
  });
  $("memResults").addEventListener("click", (e) => {
    const a = e.target.closest("a.addr");
    if (a) showMem(Number(a.dataset.addr));
  });
  $("memGo").addEventListener("click", goToAddress);
  $("addr").addEventListener("keydown", (e) => { if (e.key === "Enter") goToAddress(); });
  $("memPrev").addEventListener("click", () => memPage(-1));
  $("memNext").addEventListener("click", () => memPage(1));
  $("memFind").addEventListener("click", () => nav("inspector", memSearch));
  $("memq").addEventListener("keydown", (e) => { if (e.key === "Enter") nav("inspector", memSearch); });

  // -------------------------------------------------------------------- init
  try {
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved) src.value = saved;
  } catch (_) { /* storage unavailable */ }
  renderGutter();
  clearTrace();

  // The wasm is compiled with the WasmGC + exception-handling proposals.
  const hasWasmGC = WebAssembly.validate(
    new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0, 1, 5, 1, 95, 1, 120, 0]));
  if (hasWasmGC) {
    startWorker();
  } else {
    setState("failed", "This browser lacks WebAssembly GC support. Use a recent Chrome, Firefox, Safari or Edge.");
  }
})();
