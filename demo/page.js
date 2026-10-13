// Shared chrome for every demo page: the design system a page runs, the
// source listings, the header size readouts, and the column that groups the
// running demo with its tracing note. Pages declare only what differs and
// load this with a relative src: an app's page carries one
// <template data-variant="mdc3" data-source="MDC3.purs Counter.purs">
// per design system, holding that system's stylesheets and style, and this
// loads the chosen one's bundle-<variant>.js (a plain-HTML app carries one
// `html` template; a page with no template loads its own bundle.js — the
// law benches).

const labels = { mdc2: "MDC2", mdc3: "MDC3", shoelace: "Shoelace", fluent: "Fluent", bootstrap: "Bootstrap", html: "HTML" }

// The implementations a page carries, in the order it lists them; the query
// names the one shown (`inbox/?mdc3`), the first when it names none.
const variantTemplates = [...document.querySelectorAll("template[data-variant]")]
const chosenTemplate = variantTemplates.find(t => t.dataset.variant === location.search.slice(1)) || variantTemplates[0]

const currentVariant = chosenTemplate && chosenTemplate.dataset.variant

const bundleFile = chosenTemplate ? "bundle-" + currentVariant + ".js" : "bundle.js"

// Install the chosen implementation: its stylesheets into the head, its
// sources onto the body, its own notes kept and the others' dropped. Its
// bundle loads once the stylesheets have: a catalogue measures itself when it
// initializes (an MDC2 outlined field sizes its notch to its label), so it
// must not mount on an unstyled page.
const installVariant = () => {
  if (!chosenTemplate) return
  const v = currentVariant
  document.body.dataset.source = chosenTemplate.dataset.source
  for (const n of document.body.querySelectorAll("[data-variant]"))
    if (!n.dataset.variant.split(" ").includes(v)) n.remove()
  document.title += " (" + labels[v] + ")"
  const title = document.getElementById("page-title")
  if (title) title.textContent += " (" + labels[v] + ")"
  const content = chosenTemplate.content.cloneNode(true)
  const loaded = [...content.querySelectorAll('link[rel="stylesheet"]')].map(link =>
    new Promise(done => { link.onload = link.onerror = done }))
  document.head.append(content)
  Promise.all(loaded).then(() => {
    const script = document.createElement("script")
    script.type = "module"
    script.src = bundleFile
    document.head.append(script)
  })
}

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
  const suffix = currentVariant
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

// Design-system switcher: every template of the page, each a link to the
// page with that one chosen.
const offerDesignSystemSwitch = () => {
  const header = document.getElementById("page-header")
  if (!header || variantTemplates.length < 2) return
  const toggle = document.createElement("span")
  toggle.innerHTML = '<span class="sep">·</span> ' + variantTemplates.map(t => t.dataset.variant).map(s =>
    s === currentVariant ? "<strong>" + labels[s] + "</strong>"
      : '<a href="?' + s + '">' + labels[s] + "</a>").join(" | ")
  header.append(toggle)
}

// Every listing shows its file verbatim, highlighted and nothing else.
const escapeHtml = (t) => t.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c])

const renderListing = (code, text) => {
  code.textContent = text
  if (window.hljs) hljs.highlightElement(code)
}

const wordsStyle = () => {
  if (document.getElementById("words-style")) return
  const style = document.createElement("style")
  style.id = "words-style"
  style.textContent = ".words a { font-family: monospace; font-style: normal; }"
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
const vocabularyIndex = "https://github.com/restaumatic/bambik/blob/main/.claude/skills/developing-bambik-apps/vocabulary.md"

const wordsOf = (src) => {
  const words = []
  for (const m of src.matchAll(/^import PUI(?:\.[\w.]+)? \((.*)\)[ \t]*$/gm))
    words.push(...m[1].split(",").map((w) => w.trim().replace(/^\((.*)\)$/, "$1")).filter(Boolean))
  for (const m of src.matchAll(/^import (?:Data\.Profunctor\.Row\.\w+|QualifiedDo\.\w+) as (\w+)/gm))
    words.push(m[1] + ".do")
  return words
}

// a view's file is named by its design system alone (`counter/MDC2.purs`)
const viewPath = (suite, family, suffix) => "../../" + suite + "/" + family + "/" + labels[suffix] + ".purs"


const showNewWords = (viewSource) => {
  const m = location.pathname.match(/\/(7guis|nguis)\/([a-z0-9-]+)\/(index\.html)?$/)
  const panel = document.getElementById("source-panel")
  if (!m || !panel || !currentVariant) return
  const family = m[2]
  const suffix = currentVariant
  const at = readingOrder.findIndex(([, f]) => f === family)
  const earlier = (at >= 0 ? readingOrder.slice(0, at) : readingOrder)
    .filter(([suite]) => suite === "7guis" || suffix === "mdc2" || suffix === "mdc3")
  Promise.all(earlier.map(([suite, f]) =>
    fetch(viewPath(suite, f, suffix), { cache: "no-cache" })
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
// first, then its view model module, then any further modules (crud's
// stand-in server, order-dashboard's packaged controls). The first fills the existing listing, each further file gets its own
// heading + listing beneath it (headings show the basename, links keep the
// path), and the header readout sums their sizes.
const showSource = (files) => {
  const names = files.trim().split(/\s+/)
  wordsStyle()
  return Promise.all([
    Promise.all(names.map(f => fetch(f, { cache: "no-cache" }).then(r => r.text()))),
    fetch(bundleFile, { method: "HEAD", cache: "no-cache" }).then(r => r.headers.get("content-length")),
  ]).then(([sources, bundleBytes]) => {
    const el = document.getElementById("src")
    const panel = el.closest("#source-panel") || document.getElementById("source-panel")
    const heading = (name) => {
      const h = document.createElement("h3")
      h.innerHTML = '<a href="' + name + '">' + name.split("/").pop() + '</a>'
      return h
    }
    if (names.length > 1) el.parentElement.before(heading(names[0]))
    renderListing(el, sources[0])
    if (names.length > 1) {
      const anchor = panel.querySelector("p.note:not(.words)")
      names.slice(1).forEach((name, i) => {
        const pre = document.createElement("pre")
        const code = document.createElement("code")
        code.className = "language-haskell"
        pre.append(code)
        anchor ? anchor.before(heading(name), pre) : panel.append(heading(name), pre)
        renderListing(code, sources[i + 1])
      })
    }
    showNewWords(sources[0])
    const total = sources.reduce((n, s) => n + new TextEncoder().encode(s).length, 0)
    document.getElementById("src-size").innerHTML =
      '<a href="' + names[0] + '">source</a> (' + fmt(total) + ')'
    if (bundleBytes) {
      document.getElementById("bundle-sep").hidden = false
      document.getElementById("bundle-size").innerHTML =
        '<a href="' + bundleFile + '">bundle</a> (' + fmt(+bundleBytes) + ')'
    }
  })
}

installVariant()
groupDemoWithNote()
offerDesignSystemSwitch()
showSource(document.body.dataset.source)
