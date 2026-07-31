// Watch-mode dev server for any demo: a lightweight watcher over src/ and
// the demo sources runs spago on change (spago -w itself exhausts inotify
// watching all of .spago), esbuild rebundles on output/ change, and the
// page auto-reloads via esbuild's /esbuild SSE endpoint (injected through
// the JS banner).
//
//   node scripts/dev.mjs counter          (7guis demos, by directory name)
//   node scripts/dev.mjs 1                 (the demo/1 order form)
import { context } from 'esbuild'
import { spawn } from 'node:child_process'
import { readdirSync, watch } from 'node:fs'
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
  console.error(`usage: node scripts/dev.mjs <demo>\ndemos: ${[...Object.keys(sevenGuis), '1'].join(', ')}`)
  process.exit(1)
}

const isSevenGuis = name in sevenGuis
const dir = isSevenGuis ? `demo/7guis/${name}` : `demo/${name}`
const watchDirs = ['src', isSevenGuis ? 'demo/7guis' : dir]
const [mod, fn] = isSevenGuis ? sevenGuis[name] : ['OrderForm', 'orderForm']

const env = { ...process.env, PATH: `${path.resolve('node_modules/.bin')}:${process.env.PATH}` }

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

const isSource = f => /\.(purs|js)$/.test(f) && !f.includes('bundle')
const subdirs = d => [d, ...readdirSync(d, { withFileTypes: true })
  .filter(e => e.isDirectory())
  .flatMap(e => subdirs(path.join(d, e.name)))]

let timer
const changed = () => { clearTimeout(timer); timer = setTimeout(build, 50) }
try {
  const dirs = watchDirs.flatMap(subdirs)
  for (const d of dirs) watch(d, (_, file) => file && isSource(file) && changed())
  console.log(`watching via inotify (${dirs.length} dirs)`)
} catch (e) {
  if (e.code === 'ENOSPC') {
    console.error(`inotify budget exhausted (ENOSPC) — raise it and retry:
  sudo sysctl fs.inotify.max_user_instances=1024 fs.inotify.max_user_watches=1048576`)
    process.exit(1)
  }
  throw e
}

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
