"use strict";
// Main-thread UI. All interpreter work happens in worker.js, and the whole
// trace stays on the OCaml side: this file only ever holds the window of data
// it is displaying (one page of logs, one page of memory, one environment).
//
//   - rpc(op, ...args)  request/response over postMessage
//   - nav(type, job)    serial queue for navigation work; a newer job of the
//                       same type replaces a pending one, so holding a key
//                       down never builds a backlog in the worker
//   - "Annulla" terminates the worker (the only way to stop a running wasm
//     computation) and starts a fresh one.
//
// Source locations are links to the editor:
//   - the location of a log entry highlights its whole start-end range;
//   - the location in an error message moves the cursor to its start/point.
(() => {
  const $ = (id) => document.getElementById(id);
  const src = $("src"), marks = $("marks"), syntax = $("syntax"), gutter = $("gutter");
  const runBtn = $("run"), cancelBtn = $("cancel"), formatBtn = $("format");
  const statusEl = $("status"), diagEl = $("diag"), maxLogsEl = $("maxLogs");

  const KINDS = ["decl", "assign", "store", "branch", "def", "call", "invoke"];
  const KIND_LABEL = {
    decl: "dichiarazioni", assign: "assegnamenti", store: "scritture su array",
    branch: "if / while", def: "definizioni", call: "chiamate di funzione",
    invoke: "chiamate di procedura",
  };
  const MEM_PAGE = 128;
  const MEM_MATCHES = 200;
  const STORAGE_KEY = "mao.source";
  const INDENT = "    ";
  const SAMPLES_DIR = "samples/";

  const clamp = (x, lo, hi) => Math.max(lo, Math.min(hi, x));
  const fmtN = (n) => n.toLocaleString("it-IT");
  const hex = (a) => "0x" + a.toString(16);

  function h(tag, attrs, ...kids) {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs || {})) {
      if (v !== false && v != null) e.setAttribute(k, v);
    }
    for (const c of kids) e.append(c);
    return e;
  }

  // Prism grammar for Mao (mirrors maoLexer.mll). Order matters: the first
  // matching rule wins, so keywords come before the "function call" rule.
  if (window.Prism) {
    Prism.languages.mao = {
      comment: { pattern: /\/\/[^\n\r]*|\/\*[\s\S]*?(?:\*\/|$)/, greedy: true },
      string: { pattern: /"[^"\n]*"/, greedy: true },
      char: /'[A-Za-z0-9]'/,
      keyword: /\b(?:skip|if|else|while|new|return|length)\b/,
      "class-name": /\b(?:int|bool|double|char)\b/,
      boolean: /\b(?:true|false)\b/,
      operator: /:=|&&|\|\||==|!=|<=|>=|\bxor\b|[-+*\/%!<>=?:]/,
      number: /\b\d+\.\d*|\b\d+\b/,
      function: /\b[A-Za-z_][A-Za-z0-9_]*(?=\s*\()/,
      punctuation: /[()[\]{},;.]/,
    };
  }

  // The formatter works on the AST, which does not keep comments: it would
  // silently delete them. Detect them (skipping string and char literals).
  function hasComments(text) {
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      if (c === '"') {
        const j = text.indexOf('"', i + 1);
        if (j < 0) return false;
        i = j;
      } else if (c === "'" && text[i + 2] === "'") {
        i += 2;
      } else if (c === "/" && (text[i + 1] === "/" || text[i + 1] === "*")) {
        return true;
      }
    }
    return false;
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
    stepRange: null,         // source range of the selected entry
    focusRange: null,        // range pinned by clicking a location link
    diag: null, diagSource: null, diagId: 0,
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
    setDiag((e && e.message) || String(e), null, src.value, "Errore interno: ");
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

  const elapsed = () => ((performance.now() - startedAt) / 1000).toFixed(1).replace(".", ",");
  function stopTicker() { clearInterval(ticker); ticker = null; }

  function startWorker(message) {
    setState("loading", message || "Caricamento dell'interprete…");
    const w = new Worker("worker.js");
    worker = w;
    w.onmessage = (e) => {
      if (w !== worker) return;
      const m = e.data;
      if (m.type === "ready") {
        setState("idle", "Pronto (Ctrl/Cmd+Invio per eseguire)");
      } else if (m.type === "fatal") {
        fail(m.message);
      } else if (m.id !== undefined) {
        const p = pending.get(m.id);
        if (!p) return;
        pending.delete(m.id);
        if (m.ok) p.resolve(m.result); else p.reject(new Error(m.error));
      }
    };
    w.onerror = (e) => { if (w === worker) fail(e.message || "Il worker si è interrotto."); };
  }

  function fail(message) {
    stopTicker();
    rejectAll("cancelled");
    if (worker) worker.terminate();
    worker = null;
    setState("failed", "L'interprete non è disponibile.");
    setDiag(message, null, src.value);
  }

  // ------------------------------------------------------------------ editor
  const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const LH = 21;

  // Syntax layer: the program highlighted by Prism (plain text if unavailable).
  function renderSyntax() {
    const text = src.value + "\n";
    syntax.innerHTML = window.Prism && Prism.languages.mao
      ? Prism.highlight(text, Prism.languages.mao, "mao")
      : esc(text);
    syntax.scrollTop = src.scrollTop;
    syntax.scrollLeft = src.scrollLeft;
  }

  // Marks layer: transparent text, only the background of the ranges shows.
  function renderMarks(ranges) {
    const full = src.value + "\n", len = full.length;
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
    marks.innerHTML = html;
    marks.scrollTop = src.scrollTop;
    marks.scrollLeft = src.scrollLeft;
  }

  function renderGutter() {
    let lines = 1;
    for (let i = src.value.indexOf("\n"); i !== -1; i = src.value.indexOf("\n", i + 1)) lines++;
    let t = "1";
    for (let i = 2; i <= lines; i++) t += "\n" + i;
    gutter.textContent = t;
    gutter.scrollTop = src.scrollTop;
  }

  function updateCursorPos() {
    const p = src.selectionStart, before = src.value.slice(0, p);
    const line = before.split("\n").length;
    $("cursorPos").textContent = `Riga ${line}, col. ${p - (before.lastIndexOf("\n") + 1) + 1}`;
  }

  function revealLine(line) {
    const top = (line - 1) * LH;
    if (top < src.scrollTop || top > src.scrollTop + src.clientHeight - 3 * LH) {
      src.scrollTop = Math.max(0, top - src.clientHeight / 3);
    }
  }

  const isStale = () => S.runSource !== null && src.value !== S.runSource;

  // Everything derived from "what the source looked like when the trace/error
  // was produced": once the text differs, highlights, links and messages are
  // stale and hidden.
  let diagKey = null;
  function refreshDecor() {
    const stale = isStale();
    const diagLive = S.diag && src.value === S.diagSource;
    renderMarks([
      !stale && S.stepRange ? { ...S.stepRange, cls: "hl" } : null,
      !stale && S.focusRange ? { ...S.focusRange, cls: "hl focus flash" } : null,
      diagLive && S.diag.loc ? { start: S.diag.loc.start, end: S.diag.loc.end, cls: "err" } : null,
    ]);
    document.body.classList.toggle("is-stale", stale);

    const key = diagLive ? "d" + S.diagId : stale ? "s" : "";
    if (key === diagKey) return;
    diagKey = key;
    diagEl.replaceChildren();
    if (diagLive) {
      diagEl.className = "diag";
      diagEl.append(S.diag.prefix + S.diag.message);
      const l = S.diag.loc;
      if (l) {
        const link = h("button", { type: "button", class: "linklike", title: "Sposta il cursore qui" },
          `riga ${l.line}, colonna ${l.col}`);
        link.addEventListener("click", () => moveCursor(l));
        diagEl.append(" (", link, ")");
      }
      diagEl.hidden = false;
    } else if (stale) {
      diagEl.className = "diag warn";
      diagEl.append("Il programma è cambiato dall'ultima esecuzione: la traccia qui sotto si riferisce ancora al vecchio sorgente. Premi Esegui per aggiornarla.");
      diagEl.hidden = false;
    } else {
      diagEl.hidden = true;
    }
  }

  function setDiag(message, loc, source, prefix) {
    S.diag = { prefix: prefix || "", message, loc: loc || null };
    S.diagSource = source;
    S.diagId++;
    refreshDecor();
    if (loc) revealLine(loc.line);
  }

  function locDiag(err, source, prefix) { setDiag(err.message, err, source, prefix); }

  // Error links: put the caret at the start (or point) of the location.
  function moveCursor(loc) {
    src.focus();
    src.setSelectionRange(loc.start, loc.start);
    revealLine(loc.line);
    updateCursorPos();
  }

  // Replace [start, end) with `text` and select [selStart, selEnd]. Goes
  // through execCommand when possible so that undo (Ctrl+Z) keeps working.
  function editText(start, end, text, selStart, selEnd) {
    src.focus();
    src.setSelectionRange(start, end);
    let ok = false;
    try { ok = document.execCommand("insertText", false, text); } catch (_) { /* fall through */ }
    if (!ok) {
      src.setRangeText(text, start, end, "end");
      src.dispatchEvent(new Event("input"));
    }
    const caret = start + text.length;
    src.setSelectionRange(selStart === undefined ? caret : selStart, selEnd === undefined ? caret : selEnd);
    updateCursorPos();
  }

  function replaceText(text) {
    editText(0, src.value.length, text, 0, 0);
    src.scrollTop = 0;
  }

  // Tab / Shift+Tab: indent / outdent the selected lines (or insert spaces up
  // to the next tab stop when there is no selection).
  function indentSelection(outdent) {
    const text = src.value;
    let start = src.selectionStart, end = src.selectionEnd;
    if (start === end && !outdent) {
      const col = start - (text.lastIndexOf("\n", start - 1) + 1);
      editText(start, end, " ".repeat(INDENT.length - (col % INDENT.length)));
      return;
    }
    const lineStart = text.lastIndexOf("\n", start - 1) + 1;
    const last = end > start && text[end - 1] === "\n" ? end - 1 : end;
    let lineEnd = text.indexOf("\n", last);
    if (lineEnd === -1) lineEnd = text.length;
    const lines = text.slice(lineStart, lineEnd).split("\n");
    let firstDelta = 0, total = 0;
    const out = lines.map((ln, i) => {
      let d;
      if (outdent) {
        const m = /^(?: {1,4}|\t)/.exec(ln);
        d = m ? -m[0].length : 0;
        ln = ln.slice(-d);
      } else {
        d = ln.length ? INDENT.length : 0;
        ln = (d ? INDENT : "") + ln;
      }
      if (i === 0) firstDelta = d;
      total += d;
      return ln;
    });
    editText(lineStart, lineEnd, out.join("\n"),
      Math.max(lineStart, start + firstDelta), Math.max(lineStart, end + total));
  }

  // Enter keeps the indentation of the current line (one level deeper after "{").
  function newlineWithIndent() {
    const text = src.value, start = src.selectionStart, end = src.selectionEnd;
    const line = text.slice(text.lastIndexOf("\n", start - 1) + 1, start);
    const base = /^[ \t]*/.exec(line)[0];
    const deeper = /\{\s*$/.test(line);
    let ins = "\n" + base + (deeper ? INDENT : "");
    const caret = start + ins.length;
    if (deeper && text[end] === "}") ins += "\n" + base;
    editText(start, end, ins, caret, caret);
  }

  let escapeArmed = false, saveTimer = null;
  src.addEventListener("keydown", (e) => {
    const plain = !e.ctrlKey && !e.metaKey && !e.altKey;
    if (e.key === "Escape") { escapeArmed = true; return; }
    if (e.key === "Tab" && plain) {
      if (escapeArmed) { escapeArmed = false; return; } // Esc, Tab: leave the editor
      e.preventDefault();
      indentSelection(e.shiftKey);
      return;
    }
    if (e.key === "Enter" && plain && !e.shiftKey && !e.isComposing) {
      e.preventDefault();
      newlineWithIndent();
    }
    if (!["Shift", "Control", "Alt", "Meta"].includes(e.key)) escapeArmed = false;
  });
  src.addEventListener("blur", () => { escapeArmed = false; });

  src.addEventListener("input", () => {
    S.focusRange = null;
    renderSyntax();
    renderGutter();
    refreshDecor();
    updateCursorPos();
    clearTimeout(saveTimer);
    saveTimer = setTimeout(() => {
      try { localStorage.setItem(STORAGE_KEY, src.value); } catch (_) { /* ignore */ }
    }, 400);
  });
  src.addEventListener("scroll", () => {
    for (const l of [marks, syntax, gutter]) { l.scrollTop = src.scrollTop; l.scrollLeft = src.scrollLeft; }
  });
  for (const ev of ["keyup", "click", "focus"]) src.addEventListener(ev, updateCursorPos);
  document.addEventListener("selectionchange", () => { if (document.activeElement === src) updateCursorPos(); });

  // --------------------------------------------------------------- run/format
  const maxLogs = () => Math.max(0, Math.floor(Number(maxLogsEl.value) || 0));

  function clearTrace() {
    S.runSource = null; S.total = 0; S.viewTotal = 0; S.rows = []; S.page = 0;
    S.selected = -1; S.selPos = -1; S.selRow = null; S.stepRange = null; S.focusRange = null;
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
    $("envNote").textContent = "";
    $("memNote").textContent = "";
    showEmpty("Esegui un programma per vedere la traccia.");
    renderPos(); renderPager(); refreshDecor();
  }

  async function run() {
    if (S.state !== "idle") return;
    const source = src.value;
    S.sid++;
    const sid = S.sid;
    clearTrace();
    startedAt = performance.now();
    setState("running", "Esecuzione… 0,0 s");
    ticker = setInterval(() => { statusEl.textContent = `Esecuzione… ${elapsed()} s`; }, 100);
    let res;
    try {
      res = await rpc("run", source, maxLogs());
    } catch (e) {
      stopTicker();
      if (sid === S.sid) { setState("idle", "Pronto"); report(e); }
      return;
    }
    stopTicker();
    if (sid !== S.sid) return;
    S.runSource = source;
    S.total = res.total;
    S.viewTotal = res.total;
    S.counts = res.kinds;
    setState("idle", `${fmtN(res.total)} log in ${elapsed()} s` +
      (res.maxDepth ? `, profondità massima delle chiamate ${res.maxDepth}` : "") +
      (res.error ? " (interrotta da un errore)" : ""));
    if (res.error) locDiag(res.error, source);
    buildChips();
    if (res.total === 0) {
      showEmpty("L'esecuzione non ha prodotto log.");
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
      if (r.output !== source && hasComments(source) &&
          !window.confirm("Il formattatore non conserva i commenti: verranno rimossi (puoi annullare con Ctrl+Z). Continuare?")) {
        statusEl.textContent = "Formattazione annullata";
        return;
      }
      S.diag = null;
      if (r.output !== source) replaceText(r.output);
      statusEl.textContent = r.output === source ? "Già formattato" : "Formattato";
      refreshDecor();
    } else {
      locDiag(r.error, source, "Impossibile formattare: ");
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
    startWorker("Annullata. Riavvio dell'interprete…");
    setDiag("Esecuzione annullata.", null, src.value);
  }

  // ---------------------------------------------------------------- examples
  const menu = $("examplesMenu"), exBtn = $("examples");
  let examples = null;

  function closeMenu() {
    menu.hidden = true;
    exBtn.setAttribute("aria-expanded", "false");
  }

  async function openMenu() {
    menu.hidden = false;
    exBtn.setAttribute("aria-expanded", "true");
    if (examples === null) {
      menu.replaceChildren(h("li", { class: "note" }, "Caricamento…"));
      try {
        const r = await fetch(SAMPLES_DIR + "index.json");
        if (!r.ok) throw new Error(r.status);
        examples = await r.json();
      } catch (_) {
        menu.replaceChildren(h("li", { class: "note" }, "Impossibile caricare gli esempi."));
        return;
      }
    }
    menu.replaceChildren();
    if (!examples.length) menu.append(h("li", { class: "note" }, "Nessun esempio disponibile."));
    for (const name of examples) {
      const b = h("button", { type: "button", role: "menuitem" }, name.replace(/\.mao$/, ""));
      b.addEventListener("click", () => { closeMenu(); loadExample(name); });
      menu.append(h("li", { role: "none" }, b));
    }
    const first = menu.querySelector("button");
    if (first) first.focus();
  }

  async function loadExample(name) {
    try {
      const r = await fetch(SAMPLES_DIR + encodeURIComponent(name));
      if (!r.ok) throw new Error(r.status);
      const text = await r.text();
      S.diag = null;
      replaceText(text.replace(/\r\n/g, "\n"));
      refreshDecor();
      statusEl.textContent = `Esempio caricato: ${name.replace(/\.mao$/, "")}`;
    } catch (_) {
      setDiag(`Impossibile caricare l'esempio «${name}».`, null, src.value);
    }
  }

  exBtn.addEventListener("click", () => { if (menu.hidden) openMenu(); else closeMenu(); });
  document.addEventListener("click", (e) => { if (!e.target.closest(".menu")) closeMenu(); });
  menu.addEventListener("keydown", (e) => {
    if (e.key === "Escape") { closeMenu(); exBtn.focus(); }
    else if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      const items = [...menu.querySelectorAll("button")];
      const i = items.indexOf(document.activeElement);
      const next = items[clamp(i + (e.key === "ArrowDown" ? 1 : -1), 0, items.length - 1)];
      if (next) { e.preventDefault(); next.focus(); }
    }
  });

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

  function locText(r) {
    return `${r.line}:${r.col}–${r.endLine}:${r.endCol}`;
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
        h("td", null, h("a", { class: "loc", role: "button", tabindex: "-1", title: "Evidenzia nel codice" }, locText(r))));
      frag.append(tr);
    }
    $("logs").replaceChildren(frag);
    if (S.rows.length) {
      $("logsEmpty").hidden = true;
    } else {
      showEmpty(S.total ? "Nessun log corrisponde al filtro." : "Esegui un programma per vedere la traccia.");
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

  // Which log the inspector refers to, stated explicitly above both tabs.
  function renderStateRef() {
    const box = $("stateRef"), r = S.selRow;
    box.replaceChildren();
    if (!S.total || !r) { box.append("Nessun log selezionato."); return; }
    box.append("Stato dopo il log ", h("b", null, `#${fmtN(S.selected)}`),
      ` di ${fmtN(S.total - 1)} — ${r.name}`, r.descr ? `: ${r.descr}` : "");
    if (S.selected === S.total - 1) box.append(" (ultimo log: stato finale)");
  }

  function changedNote() {
    return S.selected > 0
      ? `Evidenziato: modificato rispetto al log #${fmtN(S.selected - 1)}.`
      : "Primo log: tutto è nuovo rispetto allo stato iniziale (vuoto).";
  }

  function renderPos() {
    const r = S.selRow;
    const has = S.total > 0 && r;
    $("pos").textContent = !S.total ? "nessuna traccia"
      : !has ? `${fmtN(S.total)} log`
      : `#${fmtN(S.selected)} di ${fmtN(S.total - 1)}` + (S.selPos < 0 ? " (nascosto dal filtro)" : "");
    $("first").disabled = $("prev").disabled = !has || (S.selPos >= 0 && S.selPos === 0);
    $("next").disabled = $("last").disabled = !has || (S.selPos >= 0 && S.selPos >= S.viewTotal - 1);
    renderStateRef();
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
    // Clicking a location link pins (and flashes) the range of the entry.
    S.focusRange = opts && opts.flash ? { start: r.row.start, end: r.row.end } : null;
    refreshDecor();
    if (!isStale()) revealLine(r.row.line);
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

  const goTo = (i, opts) => nav("select", () => selectNow(i, opts));
  const toPage = (p) => nav("page", () => loadPage(p));

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
    $("envNote").textContent = changedNote();
  }

  function addrLink(addr, text) {
    return h("a", { class: "addr", "data-addr": addr, title: "Mostra in memoria" }, text);
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
    $("envEmpty").textContent = S.envRows.length ? "Nessun risultato." : "L'ambiente è vuoto.";
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
    $("memRange").textContent = p.cells.length ? `${hex(p.start)}–${hex(last)} di ${fmtN(p.bound)}` : "";
    $("memPrev").disabled = p.start <= 0;
    $("memNext").disabled = p.start + MEM_PAGE >= p.bound;
    $("memNote").textContent = `${changedNote()} Le celle di memoria non vengono mai liberate.`;
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
    if (!r.cells.length) { box.append("Nessuna cella contiene questo valore."); return; }
    box.append(r.truncated ? `prime ${MEM_MATCHES} corrispondenze: `
      : `${r.cells.length} ${r.cells.length === 1 ? "corrispondenza" : "corrispondenze"}: `);
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
    if (t === "" || !Number.isInteger(a) || a < 0) {
      $("addr").setCustomValidity("Inserisci un indirizzo come 0x1f oppure 31");
      $("addr").reportValidity();
      return;
    }
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
  $("goto").addEventListener("keydown", (e) => {
    if (e.key === "Enter" && $("goto").value !== "") goTo(Number($("goto").value));
  });

  let filterTimer = null;
  $("q").addEventListener("input", () => {
    clearTimeout(filterTimer);
    filterTimer = setTimeout(() => nav("filter", () => applyFilter(false)), 250);
  });

  // A row click selects the entry; a click on its location link also pins and
  // flashes the entry's source range in the editor.
  $("logs").addEventListener("click", (e) => {
    const tr = e.target.closest("tr");
    if (!tr) return;
    goTo(Number(tr.dataset.i), { flash: !!e.target.closest("a.loc") && !isStale() });
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
  renderSyntax();
  renderGutter();
  updateCursorPos();
  clearTrace();

  // The wasm is compiled with the WasmGC + exception-handling proposals.
  const hasWasmGC = WebAssembly.validate(
    new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0, 1, 5, 1, 95, 1, 120, 0]));
  if (hasWasmGC) {
    startWorker();
  } else {
    setState("failed", "Questo browser non supporta WebAssembly GC. Usa una versione recente di Chrome, Firefox, Safari o Edge.");
  }
})();
