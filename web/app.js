"use strict";
// Main-thread UI. All interpreter work happens in worker.js.
// "Cancel" terminates the worker (the only way to stop a running wasm
// computation) and starts a fresh one.
(() => {
  const $ = (id) => document.getElementById(id);
  const runBtn = $("run"), cancelBtn = $("cancel");
  const statusEl = $("status"), out = $("out"), src = $("src");

  // The wasm is compiled with the WasmGC + exception-handling proposals.
  const hasWasmGC = WebAssembly.validate(
    new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0, 1, 5, 1, 95, 1, 120, 0]));

  // loading | idle | running | failed
  let state = "loading";
  let worker = null;
  let runId = 0;
  let startedAt = 0;
  let ticker = null;

  function setState(next, message) {
    state = next;
    runBtn.disabled = next !== "idle";
    cancelBtn.disabled = next !== "running";
    statusEl.textContent = message;
  }

  function showOutput(text, isError) {
    out.textContent = text;
    out.classList.toggle("error", isError);
  }

  const elapsed = () => ((performance.now() - startedAt) / 1000).toFixed(1);

  function stopTicker() {
    clearInterval(ticker);
    ticker = null;
  }

  function startWorker(message) {
    setState("loading", message || "Loading interpreter…");
    const w = new Worker("worker.js");
    worker = w;

    w.onmessage = (e) => {
      if (w !== worker) return; // message from a worker we already discarded
      const m = e.data;
      if (m.type === "ready") {
        setState("idle", "Ready (Ctrl/Cmd+Enter to run)");
      } else if (m.type === "result") {
        if (m.id !== runId) return;
        stopTicker();
        showOutput(m.output, !m.ok);
        setState("idle", `Finished in ${elapsed()} s`);
      } else if (m.type === "fatal") {
        fail(m.message);
      }
    };
    // Fires if worker.js itself cannot be loaded or throws.
    w.onerror = (e) => {
      if (w === worker) fail(e.message || "The worker crashed.");
    };
  }

  function fail(message) {
    stopTicker();
    if (worker) worker.terminate();
    worker = null;
    showOutput(message, true);
    setState("failed", "The interpreter is unavailable. See the message on the right.");
  }

  function run() {
    if (state !== "idle") return;
    runId++;
    showOutput("", false);
    startedAt = performance.now();
    setState("running", "Running… 0.0 s");
    ticker = setInterval(() => {
      statusEl.textContent = `Running… ${elapsed()} s`;
    }, 100);
    worker.postMessage({ type: "run", id: runId, source: src.value });
  }

  function cancel() {
    if (state !== "running") return;
    stopTicker();
    runId++;                 // ignore anything the old run might still report
    worker.terminate();
    worker = null;
    showOutput("Run cancelled.", false);
    startWorker("Cancelled. Restarting interpreter…");
  }

  runBtn.addEventListener("click", run);
  cancelBtn.addEventListener("click", cancel);
  src.addEventListener("keydown", (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === "Enter") run();
  });

  if (hasWasmGC) {
    startWorker();
  } else {
    setState("failed",
      "This browser lacks WebAssembly GC support. Use a recent Chrome, Firefox, Safari or Edge.");
  }
})();
