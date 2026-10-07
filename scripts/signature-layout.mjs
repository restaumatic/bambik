// The layout of a business function's signature (writing.md *Layout*): one
// line when it fits the width; otherwise the name alone, then `::` and each
// `->` leading a line, and a record or variant too long for its line broken
// one field per line under its opening bracket. Only whitespace changes, so
// the result is still the compiler's hint verbatim, whitespace aside.
export const width = 100

const tokenize = s => s.match(/"(?:[^"\\]|\\.)*"|::|->|[{}[\](),|]|[^\s{}[\](),|"]+/g) ?? []
const closer = { '{': '}', '[': ']', '(': ')' }
const stops = new Set([',', ')', '}', ']', '->', '|'])

const parse = text => {
  const ts = tokenize(text)
  let i = 0
  const peek = () => ts[i]
  const next = () => ts[i++]
  const type = () => {
    const parts = [app()]
    while (peek() === '->') { next(); parts.push(app()) }
    return parts.length > 1 ? { k: 'arrow', parts } : parts[0]
  }
  const app = () => {
    const items = []
    while (peek() !== undefined && !stops.has(peek())) items.push(atom())
    if (items.length === 0) throw new Error(`unexpected ${peek()}`)
    return items.length === 1 ? items[0] : { k: 'app', items }
  }
  const atom = () => {
    const t = next()
    if (!(t in closer)) return { k: 'word', t }
    const close = closer[t]
    const entries = []
    let tail = null
    if (peek() === close) { next(); return { k: 'group', open: t, close, entries, tail } }
    for (;;) {
      if (ts[i + 1] === '::' && peek() !== '(') { const label = next(); next(); entries.push({ label, type: type() }) }
      else entries.push({ type: type() })
      const sep = next()
      if (sep === ',') continue
      if (sep === '|') { tail = type(); if (next() !== close) throw new Error('unclosed tail'); break }
      if (sep !== close) throw new Error(`expected ${close}, got ${sep}`)
      break
    }
    return { k: 'group', open: t, close, entries, tail }
  }
  const t = type()
  if (i !== ts.length) throw new Error(`trailing ${ts.slice(i).join(' ')}`)
  return t
}

const entry = (e, f) => (e.label ? `${e.label} :: ` : '') + f(e.type, e.label ? e.label.length + 4 : 0)

const inline = n => {
  switch (n.k) {
    case 'word': return n.t
    case 'app': return n.items.map(inline).join(' ')
    case 'arrow': return n.parts.map(inline).join(' -> ')
    case 'group':
      if (n.entries.length === 0 && !n.tail) return n.open + n.close
      return `${n.open} ${n.entries.map(e => entry(e, inline)).join(', ')}${n.tail ? ` | ${inline(n.tail)}` : ''} ${n.close}`
  }
}

const pad = c => ' '.repeat(c)

const render = (n, c) => {
  const s = inline(n)
  if (c + s.length <= width) return s
  if (n.k === 'group' && n.entries.length) {
    const rows = n.entries.map((e, j) => (j === 0 ? `${n.open} ` : `${pad(c)}, `) + entry(e, (t, off) => render(t, c + 2 + off)))
    if (n.tail) rows.push(`${pad(c)}| ${render(n.tail, c + 2)}`)
    return `${rows.join('\n')}\n${pad(c)}${n.close}`
  }
  if (n.k === 'app') {
    const prefix = n.items.slice(0, -1).map(inline).join(' ') + ' '
    return prefix + render(n.items[n.items.length - 1], c + prefix.length)
  }
  return s
}

const oneLine = t => t.replace(/\s+/g, ' ').replace(/ ,/g, ',').trim()

export const layoutSignature = (name, hint) => {
  const flat = oneLine(hint)
  let t
  try { t = parse(flat) } catch { return `${name} :: ${flat}` }
  if (inline(t) !== flat) return `${name} :: ${flat}`
  if (`${name} :: ${flat}`.length <= width) return `${name} :: ${flat}`
  const parts = t.k === 'arrow' ? t.parts : [t]
  return [name, ...parts.map((p, j) => `  ${j === 0 ? '::' : '->'} ${render(p, 5)}`)].join('\n')
}
