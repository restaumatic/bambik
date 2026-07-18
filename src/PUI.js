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
