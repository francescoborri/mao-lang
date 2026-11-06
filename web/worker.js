"use strict";
// Web Worker: loads the OCaml->WASM interpreter and runs programs off the
// main thread, so the page stays responsive and a run can be cancelled by
// terminating this worker (see app.js).
//
// Protocol
//   worker -> page   { type: "ready" }
//                    { type: "result", id, ok, output }
//                    { type: "fatal", message }
//   page -> worker   { type: "run", id, source }

let failed = false;

function fatal(message) {
  if (failed) return;
  failed = true;
  self.postMessage({ type: "fatal", message });
}

self.addEventListener("unhandledrejection", (e) =>
  fatal("Failed to initialise the interpreter: " + e.reason));

// Defines the global `runProgram` (registered by web/main.ml). The loader
// fetches main.bc.wasm.assets/ relative to the script's location, so this
// file must live next to it.
try {
  importScripts("main.bc.wasm.js");
} catch (err) {
  fatal("Could not load main.bc.wasm.js: " + err);
}

self.onmessage = (e) => {
  const m = e.data;
  if (m.type !== "run") return;
  try {
    const r = self.runProgram(m.source);
    self.postMessage({ type: "result", id: m.id, ok: r.ok, output: r.output });
  } catch (err) {
    self.postMessage({ type: "result", id: m.id, ok: false,
                       output: "Internal error: " + err });
  }
};

// The wasm module is instantiated asynchronously: tell the page once the
// OCaml side has registered `runProgram`.
(function waitForRuntime(tries) {
  if (failed) return;
  if (typeof self.runProgram === "function") {
    self.postMessage({ type: "ready" });
  } else if (tries > 300) {
    fatal("Timed out waiting for the interpreter to start.");
  } else {
    setTimeout(() => waitForRuntime(tries + 1), 50);
  }
})(0);
