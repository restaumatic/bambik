// Shared chrome for every demo page: the source listings, the header size
// readouts, and the column that groups the running demo with its tracing note.
// Pages declare only what differs — their .purs filenames, via
// <body data-source="CounterMDC2.purs ../counter/CounterLogic.purs"> — and
// load this with a relative src.

const fmt = (n) => n < 1024 ? n + "B"
  : n < 1048576 ? (n / 1024).toFixed(1).replace(/\.0$/, "") + "kB"
  : (n / 1048576).toFixed(1).replace(/\.0$/, "") + "MB"

// The demo mounts into <body> at runtime and has no marker class of its own (it
// is whatever the UI component builds), so it cannot be wrapped in static markup:
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
  // Comments move too: a pane's placeholders are comment nodes, and a pane
  // mounted at the top level must stay between its own placeholders or it
  // can never be detached again.
  const mounted = () => [...document.body.childNodes].filter(n =>
    !chrome.has(n) && n.nodeName !== "SCRIPT" &&
    (n.nodeType === 1 || n.nodeType === 8 || (n.nodeType === 3 && n.textContent.trim())))
  // Moved a task after mounting, once custom elements have rendered: a
  // Shoelace range moved before its first render throws from its
  // disconnectedCallback, unobserving an input it has not built yet.
  const collect = () => setTimeout(() => {
    const surface = document.body.hasAttribute("data-surface") && demoSurface()
    if (surface) { surface.inner.append(...mounted()); column.append(surface.outer, note) }
    else column.append(...mounted(), note)
  })
  // Collect once the demo has mounted, then stop: it mutates its own DOM
  // afterwards, and a live observer would keep re-parenting its children.
  if (mounted().length) collect()
  else {
    new MutationObserver((_, obs) => {
      if (mounted().length) { obs.disconnect(); collect() }
    }).observe(document.body, { childList: true })
  }
}

// The surface a demo is shown on is the page's framing, not the app's: an app
// wrapped whole in one card would teach a card nobody's design has. A page
// opts in with <body data-surface>, and gets its vocabulary's own card —
// built from the classes its stylesheet already carries (MDC2, Bootstrap), its
// custom element (Shoelace), or the stylesheet the vocabulary's card injects,
// under the same id so a card inside the demo never injects it twice (MDC3,
// Fluent). MD3 and Fluent elements carry no margins, so the surface stacks its
// children with a gap, as their cards do.
const surfaceStyles = {
  mdc3: ["md3-card", `
.md3-card { background: var(--md-sys-color-surface-container-low, #f7f2fa); color: var(--md-sys-color-on-surface, #1d1b20); border-radius: 12px; box-shadow: 0 1px 2px rgba(0,0,0,.3), 0 1px 3px 1px rgba(0,0,0,.15); padding: 16px; margin: 15px 0; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
.md3-card > p { margin: 0; }
`],
  fluent: ["fluent-card", `
.fluent-card { background: var(--colorNeutralBackground1, #fff); color: var(--colorNeutralForeground1, #242424); font-family: var(--fontFamilyBase, 'Segoe UI', sans-serif); border-radius: var(--borderRadiusXLarge, 8px); box-shadow: var(--shadow4, 0 2px 4px rgba(0,0,0,.14)); padding: 20px; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
`],
}

const demoSurface = () => {
  const suffix = (location.pathname.match(/-(mdc2|mdc3|shoelace|fluent|bootstrap)\/(index\.html)?$/) || [])[1]
  const div = (className, style) => {
    const d = document.createElement("div")
    if (className) d.className = className
    if (style) d.style.cssText = style
    return d
  }
  const single = (d) => ({ outer: d, inner: d })
  switch (suffix) {
    case "mdc2":
      return single(div("mdc-card", "padding: 10px; margin: 15px 0; text-align: justify;"))
    case "mdc3":
    case "fluent": {
      const [id, css] = surfaceStyles[suffix]
      if (!document.getElementById(id)) {
        const style = document.createElement("style")
        style.id = id
        style.textContent = css
        document.head.append(style)
      }
      return single(div(id))
    }
    case "shoelace": {
      const outer = document.createElement("sl-card")
      const inner = div("", "display: flex; flex-direction: column; align-items: flex-start; gap: var(--sl-spacing-medium);")
      outer.append(inner)
      return { outer, inner }
    }
    case "bootstrap": {
      const outer = div("card")
      const inner = div("card-body d-flex flex-column align-items-start gap-3")
      outer.append(inner)
      return { outer, inner }
    }
    default:
      return null
  }
}

// Design-system switcher: vocabulary siblings live beside each other by path
// convention (…/counter-mdc2/ ↔ …/counter-mdc3/ ↔ …/counter-shoelace/ ↔ …),
// so every sibling URL is derived from the location and the switcher lists
// exactly the siblings that actually exist (each probed with a HEAD request)
// — no per-page markup; single-variant demos carry no suffix and get no
// switcher.
const offerDesignSystemSwitch = () => {
  const header = document.getElementById("page-header")
  if (!header) return
  const labels = { mdc2: "MDC2", mdc3: "MDC3", shoelace: "Shoelace", fluent: "Fluent", bootstrap: "Bootstrap", html: "HTML" }
  const suffixes = Object.keys(labels)
  const current = suffixes.find(s => location.pathname.endsWith("-" + s + "/"))
  if (!current) return
  const base = location.pathname.slice(0, -(current.length + 2)) + "-"
  Promise.all(suffixes.map(s =>
    s === current ? Promise.resolve(true)
      : fetch(base + s + "/index.html", { method: "HEAD", cache: "no-cache" })
          .then(r => r.ok).catch(() => false)
  )).then(exists => {
    const present = suffixes.filter((_, i) => exists[i])
    if (present.length < 2) return
    const toggle = document.createElement("span")
    toggle.innerHTML = '<span class="sep">·</span> ' + present.map(s =>
      s === current ? "<strong>" + labels[s] + "</strong>"
        : '<a href="' + base + s + '/">' + labels[s] + "</a>").join(" | ")
    header.append(toggle)
  })
}

// data-source names the demo's source files, space-separated: the view module
// first, then its logic module (for design-system twins a relative path into
// the shared unsuffixed sibling directory), then any packaged components
// module. The first fills the existing listing, each further file gets its own
// heading + listing beneath it (headings show the basename, links keep the
// path), and the header readout sums their sizes.
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
        h.innerHTML = '<a href="' + name + '">' + name.split("/").pop() + '</a>'
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
