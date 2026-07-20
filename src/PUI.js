export const traceEnabled = () => {
  if (typeof window !== "undefined" && window.__bambikTrace === true) return true;
  try {
    return typeof localStorage !== "undefined" && localStorage.getItem("bambik-trace") === "true";
  } catch (_) {
    return false;
  }
};

export const traceImpl = (tag) => (value) => () =>
  console.debug("%c[bambik]%c " + tag, "color:#6200ee;font-weight:bold", "color:inherit", value);

// Starvation warnings fire in browsers only (never in Node test runs) and
// can be silenced with `window.__bambikNoWarn = true`.
export const diagnosticsEnabled = () =>
  typeof window !== "undefined" && window.__bambikNoWarn !== true;

export const warnImpl = (msg) => () =>
  console.warn("%c[bambik]%c " + msg, "color:#b26a00;font-weight:bold", "color:inherit");
