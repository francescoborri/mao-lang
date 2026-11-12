"use strict";
// Web Worker: loads the OCaml->WASM interpreter and answers requests from the
// page. The whole trace lives on the OCaml side; the page only ever asks for
// small windows of it (see web/main.ml). Running in a worker keeps the page
// responsive and lets "Cancel" terminate a runaway computation (see app.js).
//
// Protocol
//   worker -> page   { type: "ready" }
//                    { type: "fatal", message }
//                    { id, ok: true,  result }
//                    { id, ok: false, error }
//   page -> worker   { id, op, args }      op is a function of the global `Mao`
//                                          (run, format, filter, logs, row,
//                                          env, memory, memSearch)

let failed = false;

function fatal(message) {
  if (failed) return;
  failed = true;
  self.postMessage({ type: "fatal", message });
}

self.addEventListener("unhandledrejection", (e) =>
  fatal("Failed to initialise the interpreter: " + e.reason));

// Defines the global `Mao` (registered by web/main.ml). The loader fetches
// main.bc.wasm.assets/ relative to the script's location, so this file must
// live next to it.
try {
  importScripts("main.bc.wasm.js");
} catch (err) {
  fatal("Could not load main.bc.wasm.js: " + err);
}

self.onmessage = (e) => {
  const { id, op, args } = e.data;
  try {
    const fn = self.Mao && self.Mao[op];
    if (typeof fn !== "function") throw new Error("unknown operation: " + op);
    const result = fn(...args);
    // OCaml-side failures are reported as { fatal } instead of exceptions.
    if (result && typeof result.fatal === "string") {
      self.postMessage({ id, ok: false, error: result.fatal });
    } else {
      self.postMessage({ id, ok: true, result });
    }
  } catch (err) {
    self.postMessage({ id, ok: false, error: "Internal error: " + err });
  }
};

// The wasm module is instantiated asynchronously: tell the page once the
// OCaml side has registered `Mao`.
(function waitForRuntime(tries) {
  if (failed) return;
  if (self.Mao && typeof self.Mao.run === "function") {
    self.postMessage({ type: "ready" });
  } else if (tries > 300) {
    fatal("Timed out waiting for the interpreter to start.");
  } else {
    setTimeout(() => waitForRuntime(tries + 1), 50);
  }
})(0);
