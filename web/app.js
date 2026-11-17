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
//   - the trace, the environment and the memory are paginated tables: each
//     page is requested to the worker when shown (see makePager).
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
  const STORAGE_KEY = "mao.source";
  const INDENT = "    ";
  const SAMPLES_DIR = "samples/";
  const INFO_URL = "info.html";

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
    disabledKinds: new Set(),
    counts: {},
    rows: [],                // the displayed page of the (filtered) trace
    selected: -1,            // index of the selected log
    selPos: -1,              // its position in the filtered trace
    pendingDelta: 0,
    tab: "env",
    memTarget: -1,           // memory cell to highlight
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
        setState("idle", "Pronto");
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
  // was produced": once the text differs, highlights, links and error messages
  // are stale and hidden.
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

    const key = diagLive ? "d" + S.diagId : "";
    if (key === diagKey) return;
    diagKey = key;
    diagEl.replaceChildren();
    diagEl.hidden = !diagLive;
    if (!diagLive) return;
    diagEl.append(S.diag.prefix + S.diag.message);
    const l = S.diag.loc;
    if (l) {
      const link = h("button", { type: "button", class: "linklike", title: "Sposta il cursore qui" },
        `riga ${l.line}, colonna ${l.col}`);
      link.addEventListener("click", () => moveCursor(l));
      diagEl.append(" (", link, ")");
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
    S.runSource = null; S.total = 0; S.rows = [];
    S.selected = -1; S.selPos = -1; S.stepRange = null; S.focusRange = null;
    S.disabledKinds = new Set(); S.counts = {};
    S.memTarget = -1; S.pendingDelta = 0;
    S.diag = null; S.diagSource = null;
    $("q").value = ""; $("envq").value = ""; $("memq").value = "";
    $("chips").replaceChildren();
    $("logs").replaceChildren();
    $("envBody").replaceChildren();
    $("memBody").replaceChildren();
    $("envEmpty").hidden = true;
    $("memEmpty").hidden = true;
    for (const p of [logsPager, envPager, memPager]) p.update(0, 0);
    showEmpty("Esegui un programma per vedere la traccia.");
    renderPos(); refreshDecor();
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
      nav("filter", () => applyFilter(true)); // selects the last entry: the final state
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

  // ------------------------------------------------------------------- pager
  // Pagination bar of a table: « ‹ pagina [n] di N › » and rows per page.
  // `onPage(offset)` must load the page starting at `offset` and then call
  // `update(offset, total)` with what the worker returned.
  function makePager(id, sizes, size, { onPage, onSize }) {
    const button = (text, title) =>
      h("button", { type: "button", class: "secondary small", title }, text);
    const first = button("«", "Prima pagina"), prev = button("‹", "Pagina precedente");
    const next = button("›", "Pagina successiva"), last = button("»", "Ultima pagina");
    const num = h("input", { class: "narrow", type: "number", min: "1", value: "1", "aria-label": "Numero di pagina" });
    const count = h("span", null, "1");
    const select = h("select", { "aria-label": "Righe per pagina" },
      ...sizes.map((n) => h("option", { selected: n === size ? "" : null }, String(n))));
    $(id).append(first, prev, h("span", null, "pagina ", num, " di ", count), next, last,
      h("span", { class: "grow" }), select, h("span", { class: "muted" }, "righe"));

    const p = {
      offset: 0, total: 0, size,
      pages: () => Math.max(1, Math.ceil(p.total / p.size)),
      page: () => Math.floor(p.offset / p.size),
      update(offset, total) {
        p.offset = offset; p.total = total;
        count.textContent = fmtN(p.pages());
        num.value = p.page() + 1;
        num.max = p.pages();
        first.disabled = prev.disabled = p.page() <= 0;
        next.disabled = last.disabled = p.page() >= p.pages() - 1;
      },
      go(page) { onPage(clamp(page, 0, p.pages() - 1) * p.size); },
    };
    first.addEventListener("click", () => p.go(0));
    prev.addEventListener("click", () => p.go(p.page() - 1));
    next.addEventListener("click", () => p.go(p.page() + 1));
    last.addEventListener("click", () => p.go(Infinity));
    num.addEventListener("change", () => p.go(Number(num.value) - 1));
    select.addEventListener("change", () => {
      p.size = Number(select.value);
      p.offset = p.offset - (p.offset % p.size);
      onSize();
    });
    return p;
  }

  const logsPager = makePager("logsPager", [50, 100, 250, 500], 100, {
    onPage: (offset) => nav("page", () => loadLogs(offset)),
    onSize: () => nav("select", () => S.selPos >= 0 ? selectPos(S.selPos, { reload: true }) : loadLogs(0)),
  });
  const envPager = makePager("envPager", [10, 25, 50, 100], 25, {
    onPage: (offset) => nav("inspector", () => loadEnv(offset)),
    onSize: () => nav("inspector", () => loadEnv(envPager.offset)),
  });
  const memPager = makePager("memPager", [16, 32, 64, 128], 32, {
    onPage: (offset) => nav("inspector", () => loadMem(offset)),
    onSize: () => nav("inspector", () => loadMem(memPager.offset)),
  });

  // Like scrollIntoView, but only scrolls the given box (never the page).
  function keepVisible(box, el) {
    const b = box.getBoundingClientRect(), r = el.getBoundingClientRect();
    const th = box.querySelector("thead");
    const top = b.top + (th ? th.offsetHeight : 0);
    if (r.top < top) box.scrollTop -= top - r.top;
    else if (r.bottom > b.bottom) box.scrollTop += r.bottom - b.bottom;
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
      const b = h("button", { class: `chip k-${k}`, "aria-pressed": "true" },
        `${KIND_LABEL[k]} ${fmtN(S.counts[k])}`);
      b.addEventListener("click", () => {
        if (S.disabledKinds.has(k)) S.disabledKinds.delete(k); else S.disabledKinds.add(k);
        b.setAttribute("aria-pressed", String(!S.disabledKinds.has(k)));
        nav("filter", () => applyFilter(false));
      });
      box.append(b);
    }
  }

  // Sends the filter to the worker, then selects the last entry (first run)
  // or the visible entry closest to the current selection.
  async function applyFilter(initial) {
    const sid = S.sid;
    if (!S.total) return;
    const kinds = KINDS.filter((k) => !S.disabledKinds.has(k));
    const { total } = await rpc("filter", kinds, $("q").value);
    if (sid !== S.sid) return;
    logsPager.total = total;
    const pos = initial || S.selected < 0 ? total - 1 : await rpc("position", S.selected);
    if (sid !== S.sid) return;
    await selectPos(pos, { reload: true });
  }

  async function loadLogs(offset) {
    const sid = S.sid;
    const r = await rpc("logs", offset, logsPager.size);
    if (sid !== S.sid) return false;
    S.rows = r.items;
    logsPager.update(r.offset, r.total);
    renderRows();
    return true;
  }

  const locText = (r) => `${r.line}:${r.col}–${r.endLine}:${r.endCol}`;

  function renderRows() {
    const frag = document.createDocumentFragment();
    S.rows.forEach((r, k) => {
      frag.append(h("tr", { "data-pos": logsPager.offset + k },
        h("td", { class: "num" }, String(r.i)),
        h("td", { style: `padding-left:${0.75 + r.depth * 1.1}rem` },
          h("span", { class: `badge k-${r.kind}` }, r.name)),
        h("td", null, r.descr),
        h("td", null, h("a", { class: "loc", role: "button", tabindex: "-1", title: "Evidenzia nel codice" }, locText(r)))));
    });
    $("logs").replaceChildren(frag);
    if (S.rows.length) $("logsEmpty").hidden = true;
    else showEmpty(S.total ? "Nessun log corrisponde alla ricerca." : "Esegui un programma per vedere la traccia.");
    markSelected();
  }

  function markSelected() {
    let found = null;
    for (const tr of $("logs").children) {
      const on = S.selPos >= 0 && Number(tr.dataset.pos) === S.selPos;
      tr.classList.toggle("sel", on);
      if (on) found = tr;
    }
    if (found) keepVisible($("logsScroll"), found);
  }

  function renderPos() {
    const has = S.total > 0 && S.selPos >= 0;
    $("pos").textContent = !S.total ? "" : S.selected >= 0 ? `#${fmtN(S.selected)} di ${fmtN(S.total - 1)}` : "";
    $("first").disabled = $("prev").disabled = !has || S.selPos === 0;
    $("next").disabled = $("last").disabled = !has || S.selPos >= logsPager.total - 1;
  }

  // Selects the entry at position `pos` of the filtered trace, loading its
  // page when needed. With an empty view the selection is kept unchanged.
  async function selectPos(pos, opts = {}) {
    const sid = S.sid, size = logsPager.size;
    if (!logsPager.total) {
      S.selPos = -1;
      if (opts.reload) await loadLogs(0);
      renderPos();
      return;
    }
    pos = clamp(pos, 0, logsPager.total - 1);
    const offset = pos - (pos % size);
    if (opts.reload || offset !== logsPager.offset || !S.rows.length) {
      if (!(await loadLogs(offset))) return;
    }
    const row = S.rows[pos - logsPager.offset];
    if (sid !== S.sid || !row) return;
    S.selected = row.i; S.selPos = pos;
    markSelected();
    renderPos();
    S.stepRange = { start: row.start, end: row.end };
    // Clicking a location link pins (and flashes) the range of the entry.
    S.focusRange = opts.flash ? { start: row.start, end: row.end } : null;
    refreshDecor();
    if (!isStale()) revealLine(row.line);
    await refreshInspector();
  }

  // Step in the filtered trace; deltas accumulate while the worker is busy.
  function stepView(delta) {
    if (!S.total) return;
    S.pendingDelta += delta;
    nav("step", async () => {
      const d = S.pendingDelta;
      S.pendingDelta = 0;
      if (d && S.selPos >= 0) await selectPos(S.selPos + d);
    });
  }

  const select = (pos, opts) => nav("select", () => selectPos(pos, opts));
  const toPage = (page) => logsPager.go(page);

  // --------------------------------------------------------------- inspector
  function setTab(name) {
    S.tab = name;
    for (const t of ["env", "mem"]) {
      $("tab-" + t).setAttribute("aria-selected", String(t === name));
      $("panel-" + t).hidden = t !== name;
      $(t + "q").hidden = t !== name;
    }
    nav("inspector", refreshInspector);
  }

  async function refreshInspector() {
    if (S.selected < 0) return;
    if (S.tab === "env") await loadEnv(envPager.offset); else await loadMem(memPager.offset);
  }

  function addrLink(addr, text) {
    return h("a", { class: "addr", "data-addr": addr, title: "Mostra in memoria" }, text);
  }

  function setEmpty(id, shown, text) {
    $(id).hidden = shown > 0;
    $(id).textContent = text;
  }

  async function loadEnv(offset) {
    const sid = S.sid;
    if (S.selected < 0) return;
    const q = $("envq").value;
    const r = await rpc("env", S.selected, q, offset, envPager.size);
    if (sid !== S.sid) return;
    envPager.update(r.offset, r.total);
    const frag = document.createDocumentFragment();
    for (const v of r.items) {
      const value = h("td", null);
      if (v.array !== null) value.append(addrLink(v.array, v.value), h("span", { class: "detail" }, v.preview));
      else value.append(v.value);
      frag.append(h("tr", { class: v.changed ? "chg" : null },
        h("td", null, v.name),
        h("td", null, v.addr !== null ? addrLink(v.addr, hex(v.addr)) : "—"),
        value, h("td", { class: "type" }, v.type)));
    }
    $("envBody").replaceChildren(frag);
    setEmpty("envEmpty", r.items.length, q.trim() ? "Nessun risultato." : "L'ambiente è vuoto.");
  }

  async function loadMem(offset) {
    const sid = S.sid;
    if (S.selected < 0) return;
    const q = $("memq").value;
    const r = await rpc("memory", S.selected, q, offset, memPager.size);
    if (sid !== S.sid) return;
    memPager.update(r.offset, r.total);
    const frag = document.createDocumentFragment();
    for (const c of r.items) {
      const cls = c.addr === S.memTarget ? "target" : c.changed ? "chg" : null;
      frag.append(h("tr", { class: cls },
        h("td", null, hex(c.addr)),
        h("td", null, c.value), h("td", { class: "type" }, c.type),
        h("td", { class: "owner" }, c.owner)));
    }
    $("memBody").replaceChildren(frag);
    setEmpty("memEmpty", r.items.length, q.trim() ? "Nessun risultato." : "La memoria è vuota.");
    const target = $("memBody").querySelector("tr.target");
    if (target) keepVisible(target.closest(".scroll"), target);
  }

  // Shows (and highlights) the memory cell at `addr`.
  function showMem(addr) {
    S.memTarget = addr;
    $("memq").value = "";
    memPager.offset = addr - (addr % memPager.size);
    if (S.tab !== "mem") setTab("mem"); else nav("inspector", refreshInspector);
  }

  // --------------------------------------------------------------- info panel
  const info = $("info");
  let infoLoaded = false;

  async function openInfo() {
    info.showModal();
    if (infoLoaded) return;
    try {
      const r = await fetch(INFO_URL);
      if (!r.ok) throw new Error(r.status);
      $("infoBody").innerHTML = await r.text();
      infoLoaded = true;
    } catch (_) {
      $("infoBody").textContent = "Impossibile caricare le informazioni.";
    }
  }

  // ------------------------------------------------------------------ wiring
  runBtn.addEventListener("click", run);
  cancelBtn.addEventListener("click", cancel);
  formatBtn.addEventListener("click", format);

  $("infoBtn").addEventListener("click", openInfo);
  $("infoClose").addEventListener("click", () => info.close());
  info.addEventListener("click", (e) => { if (e.target === info) info.close(); }); // backdrop

  $("first").addEventListener("click", () => select(0));
  $("last").addEventListener("click", () => select(logsPager.total - 1));
  $("prev").addEventListener("click", () => stepView(-1));
  $("next").addEventListener("click", () => stepView(1));

  // Search boxes: wait for a pause in typing before asking the worker.
  function debounced(f) {
    let timer = null;
    return () => { clearTimeout(timer); timer = setTimeout(f, 250); };
  }
  $("q").addEventListener("input", debounced(() => nav("filter", () => applyFilter(false))));
  $("envq").addEventListener("input", debounced(() => nav("inspector", () => loadEnv(0))));
  $("memq").addEventListener("input", debounced(() => {
    S.memTarget = -1;
    nav("inspector", () => loadMem(0));
  }));

  // A row click selects the entry; a click on its location link also pins and
  // flashes the entry's source range in the editor.
  $("logs").addEventListener("click", (e) => {
    const tr = e.target.closest("tr");
    if (!tr) return;
    select(Number(tr.dataset.pos), { flash: !!e.target.closest("a.loc") && !isStale() });
  });

  // Keyboard navigation while the trace has focus (click a row first).
  $("trace").addEventListener("keydown", (e) => {
    if (e.ctrlKey || e.metaKey || e.altKey || e.target.closest("input, select, textarea")) return;
    if (!S.total) return;
    const step = { ArrowDown: 1, j: 1, ArrowUp: -1, k: -1 }[e.key];
    if (step) stepView(step);
    else if (e.key === "Home") select(0);
    else if (e.key === "End") select(logsPager.total - 1);
    else if (e.key === "PageDown") toPage(logsPager.page() + 1);
    else if (e.key === "PageUp") toPage(logsPager.page() - 1);
    else return;
    e.preventDefault();
  });

  document.addEventListener("keydown", (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === "Enter") { e.preventDefault(); run(); }
    else if (e.altKey && e.shiftKey && e.code === "KeyF") { e.preventDefault(); format(); }
  });

  $("tab-env").addEventListener("click", () => setTab("env"));
  $("tab-mem").addEventListener("click", () => setTab("mem"));
  $("panel-env").addEventListener("click", (e) => {
    const a = e.target.closest("a.addr");
    if (a) showMem(Number(a.dataset.addr));
  });

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
