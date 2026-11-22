"use strict";

let failed = false;

// the message is for the student, the detail (often in English) for the console
function fatal(message, detail) {
  if (failed) return;
  failed = true;
  self.postMessage({ type: "fatal", message, detail: detail === undefined ? undefined : String(detail) });
}

const LOAD_FAILED = "Impossibile caricare l'interprete: controlla la connessione e riprova.";

self.addEventListener("unhandledrejection", (e) => fatal(LOAD_FAILED, e.reason));

try {
  importScripts("main.bc.wasm.js");
} catch (err) {
  fatal(LOAD_FAILED, err);
}

const isStackOverflow = (err) =>
  /call stack|too much recursion/i.test(String(err));

const STACK_OVERFLOW_MESSAGE =
  "ricorsione troppo profonda: le chiamate annidate hanno esaurito la memoria " +
  "del browser prima di raggiungere il limite di ricorsione. " +
  "Prova ad abbassare il limite di ricorsione per vedere la traccia.";

self.onmessage = (e) => {
  const { id, op, args } = e.data;
  try {
    const fn = self.Mao && self.Mao[op];
    if (typeof fn !== "function") throw new Error("operazione sconosciuta: " + op);
    const result = fn(...args);
    if (result && typeof result.fatal === "string") {
      self.postMessage({ id, ok: false, error: result.fatal });
    } else {
      self.postMessage({ id, ok: true, result });
    }
  } catch (err) {
    if (isStackOverflow(err)) {
      self.postMessage({ id, ok: false, error: STACK_OVERFLOW_MESSAGE, overflow: true });
    } else {
      self.postMessage({ id, ok: false, error: String(err) });
    }
  }
};

(function waitForRuntime(tries) {
  if (failed) return;
  if (self.Mao && typeof self.Mao.run === "function") {
    self.postMessage({ type: "ready" });
  } else if (tries > 300) {
    fatal("L'interprete non si è avviato in tempo: riprova.");
  } else {
    setTimeout(() => waitForRuntime(tries + 1), 50);
  }
})(0);
