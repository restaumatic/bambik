// Holey twins of every demo (guardrails L18): the view runs before its logic
// exists. Each demo's view model modules (`*ViewModel`) are replaced by
// stubs whose every export
// is a bare, untyped `hole`; every other demo module is copied unchanged, over
// the real library. The result is built under holey.dhall and bundled into
// .holey/demo/, mirroring demo/, for scripts/smoke/tests/holes.mjs to mount.
//
//   node scripts/holes.mjs              (all demos)
//   node scripts/holes.mjs counter-mdc2 (single demos by name, or a set)
import { build } from 'esbuild'
import { spawnSync } from 'node:child_process'
import { cpSync, mkdirSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs'
import path from 'node:path'
import { all } from './demos.mjs'

const out = '.holey'
const filters = process.argv.slice(2)
const demos = all.filter(d => d.set !== 'laws')
  .filter(d => !filters.length || filters.some(f => f === d.name || f === d.set))
if (!demos.length) {
  console.error(`no demo matches ${filters.join(' ')}`)
  process.exit(1)
}

const walk = dir => readdirSync(dir).flatMap(f => {
  const p = path.join(dir, f)
  return statSync(p).isDirectory() ? walk(p) : [p]
})
const moduleOf = file => readFileSync(file, 'utf8').match(/^module\s+([\w.]+)/m)?.[1]

const local = new Map(walk('demo').filter(f => f.endsWith('.purs') && !f.startsWith(`demo${path.sep}laws`))
  .map(f => [moduleOf(f), f]))

const importsOf = src => [...src.matchAll(/^import\s+([\w.]+)/gm)].map(m => m[1])
const isLogic = mod => /ViewModel$/.test(mod)

const stub = src => {
  const [, mod, exports] = src.match(/^module\s+([\w.]+)\s*\(([\s\S]*?)\)\s*where/m)
  const names = exports.split(',').map(s => s.trim()).filter(Boolean)
  return [`module Holey.${mod} (${names.join(', ')}) where`, '', 'import PUI.Web (hole)', '',
    ...names.map(n => `${n} = hole`), ''].join('\n')
}

const twin = src => src
  .replace(/^module\s+([\w.]+)/m, 'module Holey.$1')
  .replace(/^import\s+([\w.]+)/gm, (line, mod) =>
    local.has(mod) ? `import Holey.${mod}` : line)

rmSync(out, { recursive: true, force: true })
mkdirSync(`${out}/src`, { recursive: true })
cpSync('demo', `${out}/demo`, { recursive: true, filter: f => !f.endsWith('bundle.js') && !f.includes(`demo${path.sep}laws`) })

const written = new Set()
const emit = mod => {
  if (written.has(mod)) return
  written.add(mod)
  const src = readFileSync(local.get(mod), 'utf8')
  writeFileSync(`${out}/src/${mod}.purs`, isLogic(mod) ? stub(src) : twin(src))
  if (!isLogic(mod)) importsOf(src).filter(m => local.has(m)).forEach(emit)
}
demos.forEach(d => emit(d.mod))

const spago = spawnSync('spago', ['-x', 'holey.dhall', 'build'], { stdio: 'inherit' })
if (spago.status !== 0) process.exit(spago.status ?? 1)

for (const d of demos) {
  await build({
    stdin: { contents: `import { ${d.fn} } from './output/Holey.${d.mod}/index.js'; ${d.fn}();`, resolveDir: process.cwd() },
    bundle: true,
    format: 'esm',
    outfile: `${out}/${d.dir}/bundle.js`,
    logLevel: 'error',
  })
}
console.log(`holey twins bundled: ${demos.length}`)
