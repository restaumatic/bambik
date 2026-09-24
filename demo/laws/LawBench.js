export const section = name => () => {
  const s = document.createElement('section')
  s.dataset.bench = name
  const h = document.createElement('h3')
  h.textContent = name
  s.appendChild(h)
  document.body.appendChild(s)
  return s
}

const snapshot = v => v === undefined ? null : JSON.parse(JSON.stringify(v))

// The entry is registered before the component's channel is subscribed, so
// an emission made at subscription is logged under `registration`.
export const register = name => shape => samples => feeds => () => {
  const entry = { shape, log: [], phase: 'registration', samples: samples.map(snapshot) }
  entry.feed = k => {
    const at = entry.log.length
    entry.phase = `feed ${k}`
    try { feeds[k]() } finally { entry.phase = 'between' }
    return entry.log.slice(at).map(e => e.value)
  }
  entry.since = at => entry.log.slice(at)
  ;(window.__laws ??= {})[name] = entry
  return v => () => { entry.log.push({ phase: entry.phase, value: snapshot(v) }) }
}
