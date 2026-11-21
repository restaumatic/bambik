// A benchmark page: a mount opened in headless Chrome over CDP, driven with
// real input, and read three ways — `Performance.getMetrics` deltas (what
// the browser spent in script, style and layout), a CPU profile attributed
// to layers, and the page's own text.
import { createServer } from 'node:http'
import { existsSync, mkdtempSync, readFileSync, rmSync, statSync } from 'node:fs'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { openSession } from '../smoke/cdp.mjs'
import { launchChrome } from '../smoke/chrome.mjs'
import { layerOf, moduleMap, out } from './build.mjs'

export const sleep = ms => new Promise(r => setTimeout(r, ms))

// What the page gets before any of its own script runs: the locators the
// scenarios click by, and a count of the timeouts that fired per delay, with
// when the first and the last did — a clock line of period p is the
// timeouts of delay p.
const prelude = `
window.__bench = {
  fired: {},
  firstAt: {},
  lastAt: {},
  visible: el => { const r = el.getBoundingClientRect(); return r.width > 0 && r.height > 0 },
  name: el => (el.getAttribute('aria-label') || el.innerText || '').trim(),
  // the innermost visible element named label, inside the smallest element whose text contains scope
  find(label, scope) {
    const all = [...document.querySelectorAll('*')].filter(el => this.visible(el) && this.name(el) === label)
    const inner = all.filter(el => !all.some(o => o !== el && el.contains(o)))
    const size = el => { if (!scope) return 0; for (let a = el; a; a = a.parentElement) if ((a.innerText || '').includes(scope)) return (a.innerText || '').length; return Infinity }
    return inner.filter(el => size(el) < Infinity).sort((a, b) => size(a) - size(b))[0] ?? null
  },
  field: name => [...document.querySelectorAll('[name="' + name + '"]')].find(el => window.__bench.visible(el)) ?? null,
  centre(el) { el.scrollIntoView({ block: 'center', inline: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] },
}
{
  const set = window.setTimeout
  window.setTimeout = function (f, ms, ...rest) {
    return set.call(window, function () {
      const B = window.__bench, now = performance.now()
      B.fired[ms] = (B.fired[ms] || 0) + 1
      B.firstAt[ms] ??= now
      B.lastAt[ms] = now
      return typeof f === 'function' ? f.apply(this, arguments) : undefined
    }, ms, ...rest)
  }
}
`

const serve = () => new Promise(resolve => {
  const s = createServer((req, res) => {
    const file = path.join(out, decodeURIComponent(new URL(req.url, 'http://x').pathname))
    if (!file.startsWith(out) || !existsSync(file) || statSync(file).isDirectory()) { res.writeHead(404); return res.end() }
    res.writeHead(200, { 'content-type': file.endsWith('.js') ? 'text/javascript' : 'text/html' })
    res.end(readFileSync(file))
  })
  s.listen(0, '127.0.0.1', () => resolve(s))
})

export const startBrowser = async () => {
  const server = await serve()
  const profile = mkdtempSync(path.join(tmpdir(), 'bambik-bench-'))
  const { proc, cdpBase } = await launchChrome(profile)
  const version = await fetch(`${cdpBase}/json/version`).then(r => r.json()).then(v => v.Browser)
  const stop = async () => {
    proc.kill()
    server.close()
    await sleep(300)
    rmSync(profile, { recursive: true, force: true, maxRetries: 10, retryDelay: 200 })
  }

  // open a mount: { width, height, throttle, profile } — `profile: true` starts
  // a CPU profile before the page's first script
  const open = async (mount, { width = 1100, height = 900, throttle = 1, profile: profiled = false } = {}) => {
    const session = await openSession(cdpBase, 'about:blank')
    const { send, ev, events } = session
    await send('Emulation.setDeviceMetricsOverride', { width, height, deviceScaleFactor: 1, mobile: width < 600 })
    await send('Performance.enable')
    await send('Page.enable')
    await send('Page.addScriptToEvaluateOnNewDocument', { source: prelude })
    if (throttle > 1) await send('Emulation.setCPUThrottlingRate', { rate: throttle })
    const modules = moduleMap(mount)
    const startProfile = async () => {
      await send('Profiler.enable')
      await send('Profiler.setSamplingInterval', { interval: 200 })
      await send('Profiler.start')
    }
    // self time per layer, a library frame counted to the nearest frame above it that is not one
    const stopProfile = async () => {
      const { profile } = (await send('Profiler.stop')).result
      const byId = new Map(profile.nodes.map(n => [n.id, n]))
      const parent = new Map()
      for (const n of profile.nodes) for (const c of n.children ?? []) parent.set(c, n.id)
      const own = n => {
        const { functionName, url, lineNumber } = n.callFrame
        if (functionName === '(garbage collector)') return 'garbage collector'
        if (functionName === '(idle)') return 'idle'
        if (!url) return functionName === '(program)' || functionName === '(root)' ? 'browser' : null
        if (!url.includes(`/${mount}.js`)) return 'harness'
        let lo = 0, hi = modules.length - 1, at = null
        while (lo <= hi) { const mid = (lo + hi) >> 1; if (modules[mid].line <= lineNumber) { at = modules[mid]; lo = mid + 1 } else hi = mid - 1 }
        return layerOf(at?.path)
      }
      const owner = new Map()
      const ownerOf = id => {
        if (owner.has(id)) return owner.get(id)
        const n = byId.get(id)
        let layer = own(n)
        if (layer === null || layer === 'libraries') layer = parent.has(id) ? ownerOf(parent.get(id)) : 'browser'
        owner.set(id, layer)
        return layer
      }
      const layers = {}
      const functions = new Map()
      profile.samples.forEach((id, i) => {
        const ms = (profile.timeDeltas[i] ?? 0) / 1000
        const layer = ownerOf(id)
        layers[layer] = (layers[layer] ?? 0) + ms
        const { functionName, lineNumber } = byId.get(id).callFrame
        const key = `${functionName || '(anonymous)'}:${lineNumber}`
        functions.set(key, (functions.get(key) ?? 0) + ms)
      })
      delete layers.idle
      const top = [...functions].filter(([k]) => !k.startsWith('(idle)')).sort((a, b) => b[1] - a[1]).slice(0, 12).map(([k, ms]) => {
        const line = +k.slice(k.lastIndexOf(':') + 1)
        let at = null
        for (const m of modules) { if (m.line <= line) at = m; else break }
        return { fn: k.slice(0, k.lastIndexOf(':')), module: at?.path.replace(/^(\.bench\/)?output\//, '').replace(/\/(index|foreign)\.js$/, '') ?? '', ms: +ms.toFixed(2) }
      })
      return { layers: Object.fromEntries(Object.entries(layers).map(([k, v]) => [k, +v.toFixed(2)])), top }
    }
    if (profiled) await startProfile()
    const port = server.address().port
    await send('Page.navigate', { url: `http://127.0.0.1:${port}/${mount}.html` })

    const metrics = async () => {
      const list = (await send('Performance.getMetrics')).result.metrics
      const m = Object.fromEntries(list.map(({ name, value }) => [name, value]))
      return { script: m.ScriptDuration * 1000, style: m.RecalcStyleDuration * 1000, layout: m.LayoutDuration * 1000, task: m.TaskDuration * 1000, nodes: m.Nodes, listeners: m.JSEventListeners, heap: m.JSHeapUsedSize }
    }
    // what `act` cost: script, style and layout, the layout forced before the second reading
    const measure = async act => {
      const a = await metrics()
      await act()
      await ev('document.body.offsetHeight')
      const b = await metrics()
      const d = { script: b.script - a.script, style: b.style - a.style, layout: b.layout - a.layout }
      return { ...d, total: d.script + d.style + d.layout, nodes: b.nodes - a.nodes }
    }
    const mouse = (type, [x, y]) => send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
    const clickAt = async xy => { await mouse('mouseMoved', xy); await mouse('mousePressed', xy); await mouse('mouseReleased', xy) }
    // click the element a page expression yields; false when it yields none
    const clickOn = async expr => {
      const xy = await ev(`(() => { const el = ${expr}; return el ? window.__bench.centre(el) : null })()`)
      if (!xy) return false
      await clickAt(xy)
      return true
    }
    const click = (label, scope) => clickOn(`window.__bench.find(${JSON.stringify(label)}, ${JSON.stringify(scope ?? null)})`)
    const key = async (k, code, vk) => {
      await send('Input.dispatchKeyEvent', { type: 'keyDown', key: k, code, windowsVirtualKeyCode: vk, text: k.length === 1 ? k : undefined })
      await send('Input.dispatchKeyEvent', { type: 'keyUp', key: k, code, windowsVirtualKeyCode: vk })
    }
    const waitFor = async (expr, ms = 15000) => {
      for (let t = 0; t < ms; t += 50) { if (await ev(expr)) return true; await sleep(50) }
      return false
    }
    const gc = () => send('HeapProfiler.collectGarbage')
    return { ...session, mount, ev, send, events, metrics, measure, clickAt, clickOn, click, key, waitFor, gc, startProfile, stopProfile, sleep }
  }
  return { open, stop, version }
}
