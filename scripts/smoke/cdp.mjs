// Minimal Chrome DevTools Protocol session for the smoke harness: one
// WebSocket per page target, `Runtime.evaluate` as the only verb — every
// assertion runs as an expression inside the page.
export const openSession = async (cdpBase, url) => {
  const target = await fetch(`${cdpBase}/json/new?${encodeURIComponent(url)}`, { method: 'PUT' })
    .then(r => r.json())
  const ws = new WebSocket(target.webSocketDebuggerUrl)
  await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej })
  let id = 0
  const pending = new Map()
  ws.onmessage = e => {
    const msg = JSON.parse(e.data)
    if (msg.id && pending.has(msg.id)) { pending.get(msg.id)(msg); pending.delete(msg.id) }
  }
  const send = (method, params = {}) => new Promise(res => {
    const i = ++id
    pending.set(i, res)
    ws.send(JSON.stringify({ id: i, method, params }))
  })
  await send('Runtime.enable')
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
  return { ev, close }
}
