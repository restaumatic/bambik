// Shared chrome for every demo page: the source listing, the header size
// readouts, and the column that groups the running demo with its tracing note.
// Pages declare only what differs — their .purs filename, via
// <body data-source="Counter.purs"> — and load this with a relative src.

const fmt = (n) => n < 1024 ? n + "B"
  : n < 1048576 ? (n / 1024).toFixed(1).replace(/\.0$/, "") + "kB"
  : (n / 1048576).toFixed(1).replace(/\.0$/, "") + "MB"

// The demo mounts into <body> at runtime and has no marker class of its own (it
// is whatever the widget builds), so it cannot be wrapped in static markup:
// everything body gains that is not page chrome is moved into one column box,
// with the tracing note last — which is what makes that note read as belonging
// to the running demo rather than to the source listing beside it.
const groupDemoWithNote = () => {
  const note = document.getElementById("demo-note")
  if (!note) return
  const column = document.createElement("div")
  column.id = "demo-column"
  note.replaceWith(column)
  const chrome = new Set([
    document.getElementById("page-header"),
    document.getElementById("source-panel"),
    column,
  ])
  const collect = () => {
    const mounted = [...document.body.childNodes].filter(n =>
      !chrome.has(n) && n.nodeName !== "SCRIPT" &&
      (n.nodeType === 1 || (n.nodeType === 3 && n.textContent.trim())))
    if (!mounted.length) return false
    column.append(...mounted, note)
    return true
  }
  // Collect once the demo has mounted, then stop: it mutates its own DOM
  // afterwards, and a live observer would keep re-parenting its children.
  if (!collect()) {
    new MutationObserver((_, obs) => { if (collect()) obs.disconnect() })
      .observe(document.body, { childList: true })
  }
}

// MDC2/MDC3 switcher: design-system siblings live beside each other by path
// convention (…/counter-mdc2/ ↔ …/counter-mdc3/), so the sibling URL is
// derived from the location and the switcher appears only where the sibling
// actually exists (probed with a HEAD request) — no per-page markup;
// single-variant demos carry no suffix and get no switcher.
const offerDesignSystemSwitch = () => {
  const header = document.getElementById("page-header")
  if (!header) return
  const mdc3 = location.pathname.endsWith("-mdc3/")
  if (!mdc3 && !location.pathname.endsWith("-mdc2/")) return
  const sibling = mdc3
    ? location.pathname.replace(/-mdc3\/$/, "-mdc2/")
    : location.pathname.replace(/-mdc2\/$/, "-mdc3/")
  fetch(sibling + "index.html", { method: "HEAD", cache: "no-cache" }).then(r => {
    if (!r.ok) return
    const toggle = document.createElement("span")
    const link = (href, label) => '<a href="' + href + '">' + label + '</a>'
    toggle.innerHTML = '<span class="sep">·</span> ' +
      (mdc3 ? link(sibling, "MDC2") + " | <strong>MDC3</strong>"
           : "<strong>MDC2</strong> | " + link(sibling, "MDC3"))
    header.append(toggle)
  }).catch(() => {})
}

// data-source may name several files (space-separated) when a demo is split
// into an app module plus a packaged components module: the first fills the
// existing listing, each further file gets its own heading + listing beneath
// it, and the header readout sums their sizes.
const showSource = (files) => {
  const names = files.trim().split(/\s+/)
  return Promise.all([
    Promise.all(names.map(f => fetch(f, { cache: "no-cache" }).then(r => r.text()))),
    fetch("bundle.js", { method: "HEAD", cache: "no-cache" }).then(r => r.headers.get("content-length")),
  ]).then(([sources, bundleBytes]) => {
    const el = document.getElementById("src")
    el.textContent = sources[0]
    if (window.hljs) hljs.highlightElement(el)
    if (names.length > 1) {
      const panel = el.closest("#source-panel") || document.getElementById("source-panel")
      const anchor = panel.querySelector("p.note")
      const heading = (name) => {
        const h = document.createElement("h3")
        h.innerHTML = '<a href="' + name + '">' + name + '</a>'
        return h
      }
      el.parentElement.before(heading(names[0]))
      names.slice(1).forEach((name, i) => {
        const pre = document.createElement("pre")
        const code = document.createElement("code")
        code.className = "language-haskell"
        code.textContent = sources[i + 1]
        pre.append(code)
        if (window.hljs) hljs.highlightElement(code)
        anchor ? anchor.before(heading(name), pre) : panel.append(heading(name), pre)
      })
    }
    const total = sources.reduce((n, s) => n + new TextEncoder().encode(s).length, 0)
    document.getElementById("src-size").innerHTML =
      '<a href="' + names[0] + '">source</a> (' + fmt(total) + ')'
    if (bundleBytes) {
      document.getElementById("bundle-sep").hidden = false
      document.getElementById("bundle-size").innerHTML =
        '<a href="bundle.js">bundle</a> (' + fmt(+bundleBytes) + ')'
    }
  })
}

groupDemoWithNote()
offerDesignSystemSwitch()
showSource(document.body.dataset.source)
