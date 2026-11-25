"use strict";
(() => {
  const $ = (id) => document.getElementById(id);
  const src = $("src"), marks = $("marks"), syntax = $("syntax"), gutter = $("gutter");
  const runBtn = $("run"), cancelBtn = $("cancel"), formatBtn = $("format");
  const statusEl = $("status"), diagEl = $("diag");
  const maxLogsEl = $("maxLogs"), maxDepthEl = $("maxDepth");
  const langBtns = { false: $("langBase"), true: $("langExtra") };

  const KINDS = ["decl", "assign", "store", "branch", "def", "call", "invoke"];
  const KIND_LABEL = {
    decl: "dichiarazioni", assign: "assegnamenti", store: "scritture su array",
    branch: "if / while", def: "funzioni e procedure", call: "chiamate di funzione",
    invoke: "invocazioni di procedura",
  };
  const STORAGE_KEY = "mao.source";
  const EXTRA_KEY = "mao.extra";
  const INDENT = "    ";
  const SAMPLES_DIR = "samples/";
  const INFO_URL = "info.html";

  const clamp = (x, lo, hi) => Math.max(lo, Math.min(hi, x));
  const fmtN = (n) => n.toLocaleString("it-IT");
  const hex = (a) => "0x" + a.toString(16);
  const NETWORK_HINT = "controlla la connessione e riprova.";

  function h(tag, attrs, ...kids) {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs || {})) {
      if (v !== false && v != null) e.setAttribute(k, v);
    }
    for (const c of kids) e.append(c);
    return e;
  }

  const spinner = () => h("span", { class: "spinner", "aria-hidden": "true" });

  function retryButton(run, attrs) {
    const b = h("button", { type: "button", class: "linklike", ...attrs }, "Riprova");
    b.addEventListener("click", (e) => { e.stopPropagation(); run(); });
    return b;
  }

  // kind is "busy" (with a spinner), "error" or undefined
  function setStatus(text, kind, retry) {
    statusEl.className = kind || "";
    statusEl.replaceChildren(text);
    if (retry) statusEl.append(" ", retryButton(retry));
  }

  if (window.Prism) {
    Prism.languages.mao = {
      comment: { pattern: /\/\/[^\n\r]*|\/\*[\s\S]*?(?:\*\/|$)/, greedy: true },
      char: { pattern: /'(?:[^'\\\n]|\\[nt0\\'"])'/, greedy: true },
      string: { pattern: /"(?:[^"\\\n]|\\[nt0\\'"])*"/, greedy: true },
      keyword: [
        /\b(?:skip|if|else|while|new|return)\b/,
        { pattern: /(\.\s*)length\b/, lookbehind: true },
      ],
      "class-name": /\b(?:int|bool|double|char)\b/,
      boolean: /\b(?:true|false)\b/,
      operator: /\+\+\.?|--\.?|[-+*\/]=|:=|&&|\|\||==|!=|<=|>=|\bxor\b|[-+*\/%!<>=?:]/,
      number: /\b\d+\.\d*|\.\d+|\b\d+\b/,
      function: /\b[A-Za-z][A-Za-z0-9_]*(?=\s*\()/,
      punctuation: /[()[\]{},;.]/,
    };
  }

  function hasComments(text) {
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      if (c === '"') {
        let j = i + 1;
        while (j < text.length && text[j] !== '"') j += text[j] === "\\" ? 2 : 1;
        if (j >= text.length) return false;
        i = j;
      } else if (c === "'") {
        const len = text[i + 1] === "\\" ? 4 : 3;
        if (text[i + len - 1] === "'") i += len - 1;
      } else if (c === "/" && (text[i + 1] === "/" || text[i + 1] === "*")) {
        return true;
      }
    }
    return false;
  }

  const S = {
    state: "loading",
    extra: false,
    sid: 0,
    runSource: null,
    total: 0,
    disabledKinds: new Set(),
    counts: {},
    rows: [],
    selected: -1,
    selPos: -1,
    pendingDelta: 0,
    tab: "env",
    memTarget: -1,
    stepRange: null,
    focusRange: null,
    diag: null, diagSource: null, diagId: 0,
  };

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
    const prefix = e && e.overflow ? "Errore di esecuzione: " : "Errore interno: ";
    setDiag((e && e.message) || String(e), null, src.value, prefix);
  }

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

  function setState(next, message) {
    S.state = next;
    runBtn.disabled = next !== "idle";
    formatBtn.disabled = next !== "idle";
    cancelBtn.disabled = next !== "running";
    maxLogsEl.disabled = next === "running";
    maxDepthEl.disabled = next === "running";
    for (const b of Object.values(langBtns)) b.disabled = next === "running";
    setStatus(message, { loading: "busy", running: "busy", failed: "error" }[next]);
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
        fail(m.message, m.detail);
      } else if (m.id !== undefined) {
        const p = pending.get(m.id);
        if (!p) return;
        pending.delete(m.id);
        if (m.ok) p.resolve(m.result);
        else p.reject(Object.assign(new Error(m.error), { overflow: !!m.overflow }));
      }
    };
    w.onerror = (e) => {
      e.preventDefault();
      if (w === worker) fail("Impossibile avviare l'interprete: " + NETWORK_HINT, e.message);
    };
  }

  // the technical detail, often in English, only goes to the console
  function fail(message, detail) {
    if (detail) console.error(detail);
    stopTicker();
    rejectAll("cancelled");
    if (worker) worker.terminate();
    worker = null;
    setState("failed", message);
    setStatus(message, "error", () => startWorker());
  }

  const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const LH = 21;

  function renderSyntax() {
    const text = src.value + "\n";
    syntax.innerHTML = window.Prism && Prism.languages.mao
      ? Prism.highlight(text, Prism.languages.mao, "mao")
      : esc(text);
    syntax.scrollTop = src.scrollTop;
    syntax.scrollLeft = src.scrollLeft;
  }

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

  function locDiag(err, source, prefix) {
    setDiag(err.message, err.line ? err : null, source, prefix);
  }

  function moveCursor(loc) {
    src.focus();
    src.setSelectionRange(loc.start, loc.start);
    revealLine(loc.line);
    updateCursorPos();
  }

  function editText(start, end, text, selStart, selEnd) {
    src.focus();
    src.setSelectionRange(start, end);
    let ok = false;
    try { ok = document.execCommand("insertText", false, text); } catch (_) {  }
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
      if (escapeArmed) { escapeArmed = false; return; }
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
      try { localStorage.setItem(STORAGE_KEY, src.value); } catch (_) {  }
    }, 400);
  });
  src.addEventListener("scroll", () => {
    for (const l of [marks, syntax, gutter]) { l.scrollTop = src.scrollTop; l.scrollLeft = src.scrollLeft; }
  });
  for (const ev of ["keyup", "click", "focus"]) src.addEventListener(ev, updateCursorPos);
  document.addEventListener("selectionchange", () => { if (document.activeElement === src) updateCursorPos(); });

  // OCaml ints are 31-bit in WASM, so larger limits would overflow
  const MAX_LIMIT = 1e9;

  // an empty or invalid limit falls back to the default, only 0 means no limit
  function readLimit(el) {
    const n = Math.floor(Number(el.value));
    const value = el.value.trim() !== "" && n >= 0 ? Math.min(n, MAX_LIMIT) : Number(el.defaultValue);
    el.value = String(value);
    return value;
  }
  for (const el of [maxLogsEl, maxDepthEl]) el.addEventListener("change", () => readLimit(el));

  function clearInspector(text) {
    for (const [body, empty, pager] of [["envBody", "envEmpty", envPager], ["memBody", "memEmpty", memPager]]) {
      $(body).replaceChildren();
      pager.update(0, 0);
      $(empty).textContent = text || "";
      $(empty).hidden = !text;
    }
  }

  function clearTrace() {
    S.runSource = null; S.total = 0; S.rows = [];
    S.selected = -1; S.selPos = -1; S.stepRange = null; S.focusRange = null;
    S.disabledKinds = new Set(); S.counts = {};
    S.memTarget = -1; S.pendingDelta = 0;
    S.diag = null; S.diagSource = null;
    $("q").value = ""; $("envq").value = ""; $("memq").value = "";
    $("chips").replaceChildren();
    $("logs").replaceChildren();
    clearInspector();
    logsPager.update(0, 0);
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
    ticker = setInterval(() => setStatus(`Esecuzione… ${elapsed()} s`, "busy"), 100);
    let res;
    try {
      res = await rpc("run", source, readLimit(maxLogsEl), readLimit(maxDepthEl), S.extra);
    } catch (e) {
      stopTicker();
      if (sid === S.sid) { setState("idle", "Pronto"); report(e); }
      return;
    }
    stopTicker();
    if (sid !== S.sid) return;
    if (!res.executed) {
      setState("idle", "Programma non eseguito");
      showEmpty("Il programma non è stato eseguito: correggi l'errore e riprova.");
      locDiag(res.error, source);
      return;
    }
    S.runSource = source;
    S.total = res.total;
    S.counts = res.kinds;
    setState("idle", `${fmtN(res.total)} ${res.total === 1 ? "passo" : "passi"} in ${elapsed()} s` +
      (res.maxDepth ? `, con profondità massima delle chiamate uguale a ${res.maxDepth}` : "") +
      (res.error ? " (interrotta da un errore)" : ""));
    if (res.error) locDiag(res.error, source);
    buildChips();
    if (res.total === 0) {
      showEmpty("L'esecuzione non ha prodotto nessuna traccia.");
      refreshDecor();
    } else {
      nav("filter", () => applyFilter(true));
    }
  }

  async function format() {
    if (S.state !== "idle") return;
    const source = src.value;
    let r;
    try { r = await rpc("format", source, S.extra); } catch (e) { report(e); return; }
    if (src.value !== source) return;
    if (r.ok) {
      if (r.output !== source && hasComments(source) &&
          !window.confirm("Il formattatore non conserva i commenti: verranno rimossi (puoi annullare con Ctrl+Z). Continuare?")) {
        setStatus("Formattazione annullata");
        return;
      }
      S.diag = null;
      if (r.output !== source) replaceText(r.output);
      setStatus(r.output === source ? "Già formattato" : "Formattato");
      refreshDecor();
    } else {
      setStatus("Formattazione non riuscita");
      const message = r.error.message.charAt(0).toLowerCase() + r.error.message.slice(1);
      locDiag({ ...r.error, message }, source, "Impossibile formattare: ");
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

  const menu = $("examplesMenu"), exBtn = $("examples");
  let examples = null, exampleText = null;

  function closeMenu() {
    menu.hidden = true;
    exBtn.setAttribute("aria-expanded", "false");
  }

  async function fetchOk(url) {
    const r = await fetch(url);
    if (!r.ok) throw new Error(`${url}: HTTP ${r.status}`);
    return r;
  }

  let examplesLoading = false;

  async function openMenu() {
    menu.hidden = false;
    exBtn.setAttribute("aria-expanded", "true");
    if (examples === null) {
      if (examplesLoading) return;
      examplesLoading = true;
      menu.replaceChildren(h("li", { class: "note" }, spinner(), "Caricamento degli esempi…"));
      try {
        examples = await (await fetchOk(SAMPLES_DIR + "index.json")).json();
      } catch (e) {
        console.error(e);
        const retry = retryButton(openMenu, { role: "menuitem" });
        menu.replaceChildren(
          h("li", { class: "note error" }, "Impossibile caricare gli esempi: " + NETWORK_HINT),
          h("li", { role: "none" }, retry));
        if (!menu.hidden) retry.focus();
        return;
      } finally {
        examplesLoading = false;
      }
      if (menu.hidden) return;
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

  let exampleRequest = 0;

  async function loadExample(name) {
    const label = name.replace(/\.mao$/, "");
    if (src.value.trim() && src.value !== exampleText &&
        !window.confirm(`Il programma nell'editor verrà sostituito dall'esempio «${label}» (puoi annullare con Ctrl+Z). Continuare?`)) {
      setStatus("Caricamento dell'esempio annullato");
      return;
    }
    const request = ++exampleRequest, before = src.value;
    setStatus(`Caricamento dell'esempio «${label}»…`, "busy");
    let text;
    try {
      text = (await (await fetchOk(SAMPLES_DIR + encodeURIComponent(name))).text()).replace(/\r\n/g, "\n");
    } catch (e) {
      console.error(e);
      if (request === exampleRequest) {
        setStatus(`Impossibile caricare l'esempio «${label}»: ${NETWORK_HINT}`, "error", () => loadExample(name));
      }
      return;
    }
    if (request !== exampleRequest) return;
    // the student may have kept typing while the example was downloading
    if (src.value !== before) {
      setStatus("Caricamento dell'esempio annullato: il programma è stato modificato");
      return;
    }
    S.diag = null;
    replaceText(text);
    exampleText = src.value;
    refreshDecor();
    setStatus(`Esempio caricato: ${label}`);
  }

  const limitsBtn = $("limitsBtn"), limitsPanel = $("limitsPanel");

  function toggleLimits(open) {
    limitsPanel.hidden = !open;
    limitsBtn.setAttribute("aria-expanded", String(open));
    if (open) maxLogsEl.focus();
  }

  exBtn.addEventListener("click", () => { if (menu.hidden) openMenu(); else closeMenu(); });
  limitsBtn.addEventListener("click", () => toggleLimits(limitsPanel.hidden));
  limitsPanel.addEventListener("keydown", (e) => {
    if (e.key === "Escape" || e.key === "Enter") { toggleLimits(false); limitsBtn.focus(); }
  });
  document.addEventListener("click", (e) => {
    if (!e.target.closest("#examples, #examplesMenu")) closeMenu();
    if (!e.target.closest("#limitsBtn, #limitsPanel")) toggleLimits(false);
  });
  menu.addEventListener("keydown", (e) => {
    if (e.key === "Escape") { closeMenu(); exBtn.focus(); }
    else if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      const items = [...menu.querySelectorAll("button")];
      const i = items.indexOf(document.activeElement);
      const next = items[clamp(i + (e.key === "ArrowDown" ? 1 : -1), 0, items.length - 1)];
      if (next) { e.preventDefault(); next.focus(); }
    }
  });

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

  const logsPager = makePager("logsPager", [25, 50, 100, 250], 50, {
    onPage: (offset) => nav("page", () => loadLogs(offset)),
    onSize: () => nav("select", () => S.selPos >= 0 ? selectPos(S.selPos, { reload: true }) : loadLogs(0)),
  });
  const envPager = makePager("envPager", [25, 50, 100, 250], 50, {
    onPage: (offset) => nav("inspector", () => loadEnv(offset)),
    onSize: () => nav("inspector", () => loadEnv(envPager.offset)),
  });
  const memPager = makePager("memPager", [25, 50, 100, 250], 50, {
    onPage: (offset) => nav("inspector", () => loadMem(offset)),
    onSize: () => nav("inspector", () => loadMem(memPager.offset)),
  });

  function keepVisible(box, el) {
    const b = box.getBoundingClientRect(), r = el.getBoundingClientRect();
    const th = box.querySelector("thead");
    const top = b.top + (th ? th.offsetHeight : 0);
    if (r.top < top) box.scrollTop -= top - r.top;
    else if (r.bottom > b.bottom) box.scrollTop += r.bottom - b.bottom;
  }

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
        h("td", { class: "num" }, String(r.i + 1)),
        h("td", { style: `padding-left:${0.75 + r.depth * 1.1}rem` },
          h("span", { class: `badge k-${r.kind}` }, r.name)),
        h("td", null, r.descr),
        h("td", null, h("a", { class: "loc", role: "button", tabindex: "-1", title: "Evidenzia nel codice" }, locText(r)))));
    });
    $("logs").replaceChildren(frag);
    if (S.rows.length) $("logsEmpty").hidden = true;
    else showEmpty(S.total ? "Nessun passo corrisponde alla ricerca." : "Esegui un programma per vedere la traccia.");
    markSelected();
  }

  function markSelected() {
    let found = null;
    for (const tr of $("logs").children) {
      const on = S.selPos >= 0 && Number(tr.dataset.pos) === S.selPos;
      tr.classList.toggle("sel", on);
      if (on) tr.setAttribute("aria-current", "step"); else tr.removeAttribute("aria-current");
      if (on) found = tr;
    }
    if (found) keepVisible($("logsScroll"), found);
  }

  function renderPos() {
    const has = S.total > 0 && S.selPos >= 0;
    $("pos").textContent = has ? `passo ${fmtN(S.selected + 1)} di ${fmtN(S.total)}` : "—";
    $("stateAt").textContent = has ? `dopo il passo ${fmtN(S.selected + 1)}` : "";
    $("first").disabled = $("prev").disabled = !has || S.selPos === 0;
    $("next").disabled = $("last").disabled = !has || S.selPos >= logsPager.total - 1;
  }

  async function selectPos(pos, opts = {}) {
    const sid = S.sid, size = logsPager.size;
    if (!logsPager.total) {
      S.selPos = -1; S.stepRange = null; S.focusRange = null;
      if (opts.reload) await loadLogs(0);
      renderPos();
      clearInspector(S.total ? "Nessun passo selezionato." : "");
      refreshDecor();
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
    S.focusRange = opts.flash ? { start: row.start, end: row.end } : null;
    refreshDecor();
    if (!isStale()) revealLine(row.line);
    await refreshInspector();
  }

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
    if (S.selPos < 0) return;
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
    if (S.selPos < 0) return;
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
    if (S.selPos < 0) return;
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

  function showMem(addr) {
    S.memTarget = addr;
    $("memq").value = "";
    memPager.offset = addr - (addr % memPager.size);
    if (S.tab !== "mem") setTab("mem"); else nav("inspector", refreshInspector);
  }

  const info = $("info"), infoBody = $("infoBody");
  let infoState = "empty";

  async function loadInfo() {
    infoState = "loading";
    infoBody.replaceChildren(h("p", { class: "note" }, spinner(), "Caricamento della guida…"));
    try {
      infoBody.innerHTML = await (await fetchOk(INFO_URL)).text();
      infoState = "loaded";
    } catch (e) {
      console.error(e);
      infoState = "empty";
      const retry = retryButton(loadInfo);
      infoBody.replaceChildren(h("p", { class: "note error" },
        "Impossibile caricare la guida: " + NETWORK_HINT + " ", retry));
      if (info.open) retry.focus();
    }
  }

  function openInfo() {
    info.showModal();
    if (infoState === "empty") loadInfo();
  }

  // extra is the extended language, with the abbreviations ++, += and similar
  function setLang(extra, announce) {
    S.extra = extra;
    for (const [k, b] of Object.entries(langBtns)) b.setAttribute("aria-pressed", String(k === String(extra)));
    try { localStorage.setItem(EXTRA_KEY, String(extra)); } catch (_) {  }
    if (announce && S.state === "idle") setStatus(extra ? "Versione estesa del linguaggio" : "Versione base del linguaggio");
  }
  for (const [k, b] of Object.entries(langBtns)) {
    b.addEventListener("click", () => { if (String(S.extra) !== k) setLang(k === "true", true); });
  }

  runBtn.addEventListener("click", run);
  cancelBtn.addEventListener("click", cancel);
  formatBtn.addEventListener("click", format);

  $("infoBtn").addEventListener("click", openInfo);
  $("infoClose").addEventListener("click", () => info.close());
  info.addEventListener("click", (e) => { if (e.target === info) info.close(); });

  $("first").addEventListener("click", () => select(0));
  $("last").addEventListener("click", () => select(logsPager.total - 1));
  $("prev").addEventListener("click", () => stepView(-1));
  $("next").addEventListener("click", () => stepView(1));

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

  $("logs").addEventListener("click", (e) => {
    const tr = e.target.closest("tr");
    if (!tr) return;
    select(Number(tr.dataset.pos), { flash: !!e.target.closest("a.loc") && !isStale() });
  });

  $("trace").addEventListener("keydown", (e) => {
    if (e.ctrlKey || e.metaKey || e.altKey || e.target.closest("input, select, textarea")) return;
    if (!S.total) return;
    const step = { ArrowDown: 1, j: 1, ArrowUp: -1, k: -1 }[e.key];
    if (step) stepView(step);
    else if (e.key === "Home") select(0);
    else if (e.key === "End") select(logsPager.total - 1);
    else if (e.key === "PageDown") stepView(logsPager.size);
    else if (e.key === "PageUp") stepView(-logsPager.size);
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

  try {
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved) src.value = saved;
    setLang(localStorage.getItem(EXTRA_KEY) === "true", false);
  } catch (_) {  }
  renderSyntax();
  renderGutter();
  updateCursorPos();
  clearTrace();

  const hasWasmGC = WebAssembly.validate(
    new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0, 1, 5, 1, 95, 1, 120, 0]));
  if (hasWasmGC) {
    startWorker();
  } else {
    setState("failed", "Questo browser non supporta WebAssembly GC. Usa una versione recente di Chrome, Firefox, Safari o Edge.");
  }
})();
