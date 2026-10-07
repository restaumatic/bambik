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

// Repeated rows fold. A view model module types every function at the whole
// row its line is fed, so a long model row recurs in signature after
// signature. In every listing after the view's, each repeat after the
// row's first appearance is shown as a chip of the row's labels; a click expands it in place, a click
// on the expanded row folds it again, and "unfold all" restores the listing
// as written. Only type rows count: `{ … }`/`[ … ]` groups holding `::`, at
// least 60 characters once whitespace is collapsed.
const escapeHtml = (t) => t.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c])
const highlight = (t) => window.hljs
  ? hljs.highlight(t, { language: "haskell", ignoreIllegals: true }).value
  : escapeHtml(t)

const typeGroups = (text) => {
  const groups = [], stack = []
  for (let i = 0; i < text.length; i++) {
    const c = text[i]
    if (c === '"') { for (i++; i < text.length && text[i] !== '"'; i++) if (text[i] === "\\") i++; continue }
    if (c === "-" && text[i + 1] === "-") { while (i < text.length && text[i] !== "\n") i++; continue }
    if ("{[(".includes(c)) stack.push(i)
    else if ("}])".includes(c) && stack.length) {
      const start = stack.pop()
      if (text[start] !== "(") groups.push({ start, end: i + 1 })
    }
  }
  return groups.sort((a, b) => a.start - b.start)
}

const rowLabels = (row) => {
  const labels = []
  let depth = 0
  for (let i = 0; i < row.length; i++) {
    const c = row[i]
    if (c === '"') {
      const j = row.indexOf('"', i + 1)
      if (depth === 1 && /^\s*::/.test(row.slice(j + 1))) labels.push(row.slice(i, j + 1))
      i = j; continue
    }
    if ("{[(".includes(c)) depth++
    else if ("}])".includes(c)) depth--
    else if (depth === 1 && /[a-z_]/.test(c) && !/[\w'.]/.test(row[i - 1] || "")) {
      const m = row.slice(i).match(/^([a-z_][\w']*)\s*::/)
      if (m) labels.push(m[1])
      while (i + 1 < row.length && /[\w']/.test(row[i + 1])) i++
    }
  }
  const open = row[0], close = row[row.length - 1]
  const inner = labels.join(", ")
  return open + " " + (inner.length > 64 ? inner.slice(0, 63) + "…" : inner) + " " + close
}

const renderListing = (code, text, foldRepeats) => {
  const flat = (t) => t.replace(/\s+/g, " ")
  const seen = new Set(), folds = []
  let foldedUntil = -1
  for (const g of foldRepeats ? typeGroups(text) : []) {
    if (g.start < foldedUntil) continue
    const row = text.slice(g.start, g.end), key = flat(row)
    if (key.length < 60 || !key.includes("::")) continue
    if (seen.has(key)) { folds.push(g); foldedUntil = g.end }
    else seen.add(key)
  }
  if (!folds.length) {
    code.innerHTML = highlight(text)
    code.classList.add("hljs")
    return 0
  }
  let html = "", at = 0
  folds.forEach((g, i) => {
    html += highlight(text.slice(at, g.start))
    html += '<span class="row-fold" data-fold="' + i + '" role="button" tabindex="0" title="Repeated row: click to expand">' +
      escapeHtml(rowLabels(text.slice(g.start, g.end))) + "</span>"
    at = g.end
  })
  html += highlight(text.slice(at))
  code.innerHTML = html
  code.classList.add("hljs")
  const full = folds.map((g) => highlight(text.slice(g.start, g.end)))
  const toggle = (el) => {
    if (el.classList.contains("row-fold")) {
      el.classList.replace("row-fold", "row-unfolded")
      el.title = "Click to fold"
      el.dataset.label = el.innerHTML
      el.innerHTML = full[+el.dataset.fold]
    } else {
      el.classList.replace("row-unfolded", "row-fold")
      el.title = "Repeated row: click to expand"
      el.innerHTML = el.dataset.label
    }
  }
  code.addEventListener("click", (e) => {
    const el = e.target.closest(".row-fold, .row-unfolded")
    if (el && !String(window.getSelection())) toggle(el)
  })
  code.addEventListener("keydown", (e) => {
    const el = e.target.closest(".row-fold, .row-unfolded")
    if (el && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); toggle(el) }
  })
  const note = document.createElement("p")
  note.className = "note folds"
  note.innerHTML = folds.length + " repeated " + (folds.length === 1 ? "row" : "rows") +
    ' folded, each shown by its labels: click one to expand it, or <a href="#">unfold all</a>.'
  note.querySelector("a").addEventListener("click", (e) => {
    e.preventDefault()
    code.querySelectorAll(".row-fold").forEach(toggle)
    note.remove()
  })
  code.parentElement.before(note)
  return folds.length
}

const foldStyle = () => {
  if (document.getElementById("row-fold-style")) return
  const style = document.createElement("style")
  style.id = "row-fold-style"
  style.textContent = `
.row-fold { background: #e8eef7; color: #3b4a5e; border-radius: 4px; padding: 0 3px; cursor: pointer; }
.row-fold:hover, .row-fold:focus { background: #d4e0f2; outline: none; }
.row-unfolded { background: #f3f6fb; cursor: pointer; }
.words a { font-family: monospace; font-style: normal; }
`
  document.head.append(style)
}

// The words a demo adds. README's reading order is counter,
// temperature-converter, flight-booker, todo-list, checkout, order-form:
// on those pages the note lists the vocabulary the view imports that no
// earlier demo in the order imported, in the same design system; on every
// other page, what it imports beyond the whole order. Each word links to
// its place in the skill's vocabulary.md index.
const readingOrder = [["7guis", "counter"], ["7guis", "temperature-converter"], ["7guis", "flight-booker"],
  ["nguis", "todo-list"], ["nguis", "checkout"], ["nguis", "order-form"]]
const viewFileSuffix = { mdc2: "MDC2", mdc3: "MDC3", shoelace: "Shoelace", fluent: "Fluent", bootstrap: "Bootstrap", html: "HTML" }
const vocabularyIndex = "https://github.com/restaumatic/bambik/blob/main/.claude/skills/developing-bambik-apps/vocabulary.md"

const wordsOf = (src) => {
  const words = []
  for (const m of src.matchAll(/^import PUI(?:\.[\w.]+)? \((.*)\)[ \t]*$/gm))
    words.push(...m[1].split(",").map((w) => w.trim().replace(/^\((.*)\)$/, "$1")).filter(Boolean))
  for (const m of src.matchAll(/^import (?:Data\.Profunctor\.Row\.\w+|QualifiedDo\.\w+) as (\w+)/gm))
    words.push(m[1] + ".do")
  return words
}

const showNewWords = (viewSource) => {
  const m = location.pathname.match(/\/(7guis|nguis)\/([a-z0-9-]+)-(mdc2|mdc3|shoelace|fluent|bootstrap|html)\/(index\.html)?$/)
  const panel = document.getElementById("source-panel")
  if (!m || !panel) return
  const [, , family, suffix] = m
  const at = readingOrder.findIndex(([, f]) => f === family)
  const earlier = (at >= 0 ? readingOrder.slice(0, at) : readingOrder)
    .filter(([suite]) => suite === "7guis" || suffix === "mdc2" || suffix === "mdc3")
  const pascal = (f) => f.split("-").map((w) => w[0].toUpperCase() + w.slice(1)).join("")
  Promise.all(earlier.map(([suite, f]) =>
    fetch("../../" + suite + "/" + f + "-" + suffix + "/" + pascal(f) + viewFileSuffix[suffix] + ".purs", { cache: "no-cache" })
      .then((r) => r.ok ? r.text() : "").catch(() => "")
  )).then((sources) => {
    const known = new Set(sources.flatMap(wordsOf))
    const fresh = [...new Set(wordsOf(viewSource))].filter((w) => !known.has(w))
    const lead = at === 0 ? "Words in this demo"
      : at > 0 ? "New in this demo, over the earlier demos of the reading order"
      : "Beyond the reading order's demos"
    if (!fresh.length && at < 0) return
    const note = document.createElement("p")
    note.className = "note words"
    note.innerHTML = lead + ": " + (fresh.length
      ? fresh.map((w) => '<a href="' + vocabularyIndex + "#:~:text=" + encodeURIComponent(w) + '">' + escapeHtml(w) + "</a>").join(", ")
      : "none") + "."
    panel.prepend(note)
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
  foldStyle()
  return Promise.all([
    Promise.all(names.map(f => fetch(f, { cache: "no-cache" }).then(r => r.text()))),
    fetch("bundle.js", { method: "HEAD", cache: "no-cache" }).then(r => r.headers.get("content-length")),
  ]).then(([sources, bundleBytes]) => {
    const el = document.getElementById("src")
    const panel = el.closest("#source-panel") || document.getElementById("source-panel")
    const heading = (name) => {
      const h = document.createElement("h3")
      h.innerHTML = '<a href="' + name + '">' + name.split("/").pop() + '</a>'
      return h
    }
    if (names.length > 1) el.parentElement.before(heading(names[0]))
    renderListing(el, sources[0], false)
    if (names.length > 1) {
      const anchor = panel.querySelector("p.note:not(.words):not(.folds)")
      names.slice(1).forEach((name, i) => {
        const pre = document.createElement("pre")
        const code = document.createElement("code")
        code.className = "language-haskell"
        pre.append(code)
        anchor ? anchor.before(heading(name), pre) : panel.append(heading(name), pre)
        renderListing(code, sources[i + 1], true)
      })
    }
    showNewWords(sources[0])
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
