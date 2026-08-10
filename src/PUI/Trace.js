// The only foreign code the diagnostics need: the log sink. `console` exists
// in every JavaScript host, and both switches are parameters set from
// PureScript (see `setTracing`/`setDiagnostics`) — nothing here reads a
// global, so no host is assumed.

export const traceImpl = (tag) => (value) => () =>
  console.debug("%c[bambik]%c " + tag, "color:#6200ee;font-weight:bold", "color:inherit", value);

export const warnImpl = (msg) => () =>
  console.warn("%c[bambik]%c " + msg, "color:#b26a00;font-weight:bold", "color:inherit");
