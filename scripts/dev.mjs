// Watch-mode dev server for every demo at once. The repo's demo/ tree is the
// server root, in the same folder layout the deploy scp's to the remote host —
// /7guis/counter/ here is /bambik/demo/7guis/counter/ there, so every relative
// link and asset path resolves identically in both places.
//
// An mtime-polling watcher over src/ and demo/ drives two paths by file kind:
// a .purs/.js edit runs spago, and esbuild's own watch over output/ rebundles
// the affected demos and reloads once the new bundle lands; a page's .html has
// nothing to compile and is served straight off disk, so it reloads
// immediately. Reloads reach the browser over the /esbuild SSE endpoint
// (injected through the JS banner, the same protocol esbuild's own serve uses,
// so the demo pages need no dev-only markup).
//
// Polling, not inotify: fs.watch costs an inotify instance per directory and
// Node hits its per-process ceiling well before the ~40 source dirs here, while
// `recursive: true` silently delivers *no* events on ext4 — a watcher that
// looks healthy and never rebuilds. A stat sweep has no budget to exhaust.
//
//   node scripts/dev.mjs                  (all demos)
//   node scripts/dev.mjs counter cells    (only these, by name or set)
import { context } from 'esbuild'
import { spawn } from 'node:child_process'
import { createServer } from 'node:http'
import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs'
import path from 'node:path'
import { all, entryFor } from './demos.mjs'

const PORT = 1234

const filters = process.argv.slice(2)
const demos = filters.length
  ? all.filter(d => filters.some(f => f === d.name || f === d.set))
  : all
if (!demos.length) {
  console.error(`no demo matches ${filters.join(' ')}\n` +
    `demos: ${all.map(d => d.name).join(', ')}\nsets: 1, 7guis, nguis`)
  process.exit(1)
}

const env = { ...process.env, PATH: `${path.resolve('node_modules/.bin')}:${process.env.PATH}` }

// ── browser reload broadcast ──────────────────────────────────────────────
const clients = new Set()
const reload = () => {
  for (const res of clients) res.write('event: change\ndata: {}\n\n')
}

// ── spago build on source change ──────────────────────────────────────────
let building = false
let queued = false
function build() {
  if (building) { queued = true; return }
  building = true
  const t0 = Date.now()
  spawn('spago', ['build'], { env, stdio: ['ignore', 'inherit', 'inherit'] })
    .on('exit', code => {
      console.log(`[spago] ${code === 0 ? 'ok' : 'FAILED'} (${((Date.now() - t0) / 1000).toFixed(1)}s)`)
      building = false
      if (queued) { queued = false; build() }
    })
}

const POLL_MS = 400

// Compiled sources go through spago; a page's own .html is served straight off
// disk, so it needs no build — only a reload.
const isCompiled = f => /\.(purs|js)$/.test(f) && !f.includes('bundle')
const isServed = f => f.endsWith('.html')
const isWatched = f => isCompiled(f) || isServed(f)

const watched = dir => readdirSync(dir, { withFileTypes: true }).flatMap(e => {
  const p = path.join(dir, e.name)
  return e.isDirectory() ? watched(p) : isWatched(e.name) ? [p] : []
})

const stamp = () => {
  const seen = new Map()
  for (const f of ['src', 'demo'].flatMap(watched)) {
    try { seen.set(f, statSync(f).mtimeMs) } catch { /* raced with a delete */ }
  }
  return seen
}

let prev = stamp()
console.log(`polling ${prev.size} files in src/ and demo/ every ${POLL_MS}ms`)
setInterval(() => {
  const next = stamp()
  const touched = []
  for (const [f, m] of next) if (prev.get(f) !== m) touched.push(f)
  for (const f of prev.keys()) if (!next.has(f)) touched.push(f)
  prev = next
  if (!touched.length) return
  // Both kinds can change in one tick, so handle them independently: a
  // .purs/.js edit rebuilds, and esbuild's own watch reloads once the new
  // output lands; an .html edit has nothing to compile, so reload right away.
  const html = touched.filter(isServed)
  if (html.length) { console.log(`[html] ${html.join(' ')}`); reload() }
  if (touched.some(isCompiled)) build()
}, POLL_MS)

build()

// ── one esbuild watch context per demo ────────────────────────────────────
const reloadPlugin = name => ({
  name: 'reload',
  setup(b) {
    let first = true
    b.onEnd(result => {
      if (result.errors.length) return
      if (first) { first = false; return }
      console.log(`[esbuild] ${name}`)
      reload()
    })
  },
})

await Promise.all(demos.map(async d => {
  const ctx = await context({
    stdin: { contents: entryFor(d), resolveDir: process.cwd() },
    bundle: true,
    format: 'esm',
    outfile: `${d.dir}/bundle.js`,
    banner: { js: `new EventSource('/esbuild').addEventListener('change', () => location.reload());` },
    plugins: [reloadPlugin(d.name)],
    logLevel: 'warning',
  })
  await ctx.watch()
}))

// ── static server rooted at demo/ ─────────────────────────────────────────
const contentTypes = {
  '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css',
  '.json': 'application/json', '.svg': 'image/svg+xml', '.purs': 'text/plain',
  '.png': 'image/png', '.jpg': 'image/jpeg', '.ico': 'image/x-icon',
  '.woff': 'font/woff', '.woff2': 'font/woff2',
}

const isDir = p => { try { return statSync(p).isDirectory() } catch { return false } }

createServer((req, res) => {
  const url = new URL(req.url, 'http://x')
  const pathname = decodeURIComponent(url.pathname)

  if (pathname === '/esbuild') {
    res.writeHead(200, {
      'content-type': 'text/event-stream',
      'cache-control': 'no-cache',
      connection: 'keep-alive',
    })
    clients.add(res)
    req.on('close', () => clients.delete(res))
    return
  }

  const root = path.resolve('demo')
  let file = path.resolve(root, `.${pathname}`)
  if (file !== root && !file.startsWith(root + path.sep)) {
    res.writeHead(403, { 'content-type': 'text/plain' })
    res.end('forbidden')
    return
  }
  if (isDir(file)) file = path.join(file, 'index.html')
  if (!existsSync(file)) {
    res.writeHead(404, { 'content-type': 'text/plain' })
    res.end('not found')
    return
  }
  res.writeHead(200, {
    'content-type': contentTypes[path.extname(file)] || 'application/octet-stream',
    'cache-control': 'no-store',
  })
  res.end(readFileSync(file))
}).listen(PORT, '127.0.0.1', () => {
  console.log(`dev server: http://127.0.0.1:${PORT}/  (${demos.length} demo(s), auto-reload on)`)
  for (const d of demos) console.log(`  http://127.0.0.1:${PORT}/${d.dir.slice('demo/'.length)}/`)
})
