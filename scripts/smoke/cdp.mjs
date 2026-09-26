// Minimal Chrome DevTools Protocol session for the smoke harness: one
// WebSocket per page target, PLACEHOLDER// assertion runs as an expression inside the page.
export const openSession = async (cdpBase, url) => {
  // open blank, enable, then navigate, so nothing the page logs or throws
  // while it mounts is missed
  const target = await fetch(`${cdpBase}/json/new?about:blank`, { method: 'PUT' })
    .then(r => r.json())
  const ws = new WebSocket(target.webSocketDebuggerUrl)
  await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej })
  let id = 0
  const pending = new Map()
  // uncaught exceptions and console warnings/errors, for tests that assert a
  // page ran clean
  const events = []
  ws.onmessage = e => {
    const msg = JSON.parse(e.data)
    if (msg.id && pending.has(msg.id)) { pending.get(msg.id)(msg); pending.delete(msg.id) }
    if (msg.method === 'Runtime.exceptionThrown') {
      const d = msg.params.exceptionDetails
      events.push({ kind: 'exception', text: d.exception?.description || d.text })
    }
    if (msg.method === 'Runtime.consoleAPICalled' && ['warning', 'error'].includes(msg.params.type)) {
      events.push({ kind: msg.params.type, text: msg.params.args.map(a => a.value ?? a.description ?? '').join(' ') })
    }
  }
  const send = (method, params = {}) => new Promise(res => {
    const i = ++id
    pending.set(i, res)
    ws.send(JSON.stringify({ id: i, method, params }))
  })
  await send('Runtime.enable')
  await send('Page.navigate', { url })
  const ev = async expr => {
    const r = await send('Runtime.evaluate', { expression: expr, awaitPromise: true, returnByValue: true })
    const ex = r.result?.exceptionDetails
    if (ex) throw new Error('page exception: ' + (ex.exception?.description || ex.text))
    return r.result?.result?.value
  }
  const close = async () => {
    await fetch(`${cdpBase}/json/close/${target.id}`).catch(() => {})
    ws.close()
  }
  return { ev, close, send, events }
}
