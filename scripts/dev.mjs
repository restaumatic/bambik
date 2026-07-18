// Watch-mode dev server for any demo: a lightweight watcher over src/ and
// the demo sources runs spago on change (spago -w itself exhausts inotify
// watching all of .spago), esbuild rebundles on output/ change, and the
// page auto-reloads via esbuild's /esbuild SSE endpoint (injected through
// the JS banner).
//
//   node scripts/dev.mjs counter          (7guis demos, by directory name)
//   node scripts/dev.mjs 1|2|mdc|helloworld
import { context } from 'esbuild'
import { spawn } from 'node:child_process'
import { readdirSync, statSync } from 'node:fs'
import path from 'node:path'

const sevenGuis = {
  'counter': ['Counter', 'counter'],
  'temperature-converter': ['TemperatureConverter', 'temperatureConverter'],
  'flight-booker': ['FlightBooker', 'flightBooker'],
  'timer': ['Timer', 'timer'],
  'crud': ['Crud', 'crud'],
  'circle-drawer': ['CircleDrawer', 'circleDrawer'],
  'cells': ['Cells', 'cells'],
}

const name = process.argv[2]
if (!name) {
  console.error(`usage: node scripts/dev.mjs <demo>\ndemos: ${[...Object.keys(sevenGuis), '1', '2', 'mdc', 'helloworld'].join(', ')}`)
  process.exit(1)
}

const isSevenGuis = name in sevenGuis
const dir = isSevenGuis ? `demo/7guis/${name}` : `demo/${name}`
const spagoPath = isSevenGuis ? 'demo/7guis/*/*.purs' : `${dir}/**/*.purs`
const watchDirs = ['src', isSevenGuis ? 'demo/7guis' : dir]
const [mod, fn] = isSevenGuis ? sevenGuis[name] : ['Main', 'main']

const env = { ...process.env, PATH: `${path.resolve('node_modules/.bin')}:${process.env.PATH}` }

let building = false
let queued = false
function build() {
  if (building) { queued = true; return }
  building = true
  const t0 = Date.now()
  spawn('spago', ['build', '--path', spagoPath], { env, stdio: ['ignore', 'inherit', 'inherit'] })
    .on('exit', code => {
      console.log(`[spago] ${code === 0 ? 'ok' : 'FAILED'} (${((Date.now() - t0) / 1000).toFixed(1)}s)`)
      building = false
      if (queued) { queued = false; build() }
    })
}

// mtime polling instead of inotify: the watched set is small (~100 files)
// and inotify budgets are routinely exhausted by editors on dev machines
const sources = d => readdirSync(d, { withFileTypes: true }).flatMap(e => {
  const p = path.join(d, e.name)
  if (e.isDirectory()) return sources(p)
  return /\.(purs|js)$/.test(e.name) && !e.name.includes('bundle') ? [p] : []
})
const stamp = () => watchDirs.flatMap(sources)
  .map(p => `${p}:${statSync(p, { throwIfNoEntry: false })?.mtimeMs ?? 0}`).join('\n')
let last = stamp()
setInterval(() => {
  const now = stamp()
  if (now !== last) { last = now; build() }
}, 300)

build()

const ctx = await context({
  stdin: {
    contents: `import { ${fn} } from './output/${mod}/index.js'; ${fn}();`,
    resolveDir: process.cwd(),
  },
  bundle: true,
  format: 'esm',
  outfile: `${dir}/bundle.js`,
  banner: { js: `new EventSource('/esbuild').addEventListener('change', () => location.reload());` },
  logLevel: 'info',
})
await ctx.watch()
const { port } = await ctx.serve({ servedir: dir, port: 1234, host: '127.0.0.1' })
console.log(`dev server: http://127.0.0.1:${port}/ (${dir}, auto-reload on)`)
