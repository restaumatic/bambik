// The benchmark's mounts: a station of Diner alone on a day of a given
// load, the six stations together under their tab bar, or the application
// as it starts — each a PureScript module written into .bench/src, compiled
// against a copy of output/ and bundled unminified, so a CPU profile can be
// read back module by module.
import { spawnSync } from 'node:child_process'
import { cpSync, existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { build } from 'esbuild'
import { linksFor } from '../demos.mjs'

export const out = '.bench'

// a station of the application: its tab, its part and the entry the part exports
export const stations = {
  Stock: { tab: 'Magazyn', fn: 'stock' },
  Hall: { tab: 'Sala', fn: 'hall' },
  Kitchen: { tab: 'Kuchnia', fn: 'kitchen' },
  Pass: { tab: 'Wydawka', fn: 'pass' },
  Report: { tab: 'Raport', fn: 'report' },
  Statistics: { tab: 'Statystyki', fn: 'statistics' },
}

// the standard load times `scale`
const load = scale =>
  `standardLoad { dishes = ${304 * scale}, tables = ${42 * scale}, paid = ${219 * scale}, ready = ${10 * scale}, cooking = ${20 * scale}, taking = ${3 * scale} }`

// A mount is { name, kind } and, by kind:
//   'app'      the application as it starts
//   'station'  { part, scale, perPage: { list, tiles } } — one station on a
//              day of `scale`
//   'service'  { scale, perPage } — the six stations under their tab bar
// A station's lines that read the time follow the wall clock themselves.
const source = m => {
  const head = [`module BenchMain.${m.name} (main) where`, '', 'import Prelude', 'import Effect (Effect)']
  if (m.kind === 'app') return [...head, 'import Diner.View as App', '', 'main :: Effect Unit', 'main = App.diner', ''].join('\n')
  const seed = station => `( busyWith (${load(m.scale)}) { list: ${m.perPage.list}, tiles: ${m.perPage.tiles} } (."${station}" {}) )`
  const imports = [
    'import Data.Profunctor.Row.VariantToVariant as VariantToVariant',
    'import Diner.Fixtures (busyWith)',
    'import Diner.Service (standardLoad)',
    'import PUI (looped, with)',
    'import PUI.Web (inCaseAt)',
    'import PUI.Web.MDC3 (body, tab, tabBar)',
    'import QualifiedDo.Semigroupoid as Semigroupoid',
  ]
  if (m.kind === 'station') {
    const s = stations[m.part]
    return [...head, ...imports, `import Diner.View.${m.part} as Part`, '', 'main :: Effect Unit', 'main =',
      '  body $', '    ( Semigroupoid.do', `      Part.${s.fn}`, `    ) # looped # with ${seed(s.tab)}`, '',
    ].join('\n')
  }
  const parts = Object.entries(stations)
  return [...head, ...imports, ...parts.map(([part]) => `import Diner.View.${part} as ${part}`), '', 'main :: Effect Unit', 'main =',
    '  body $', '    ( Semigroupoid.do', '      tabBar @"Stanowisko" $ VariantToVariant.do',
    ...parts.map(([, s]) => `        tab @"${s.tab}"`),
    ...parts.map(([part, s]) => `      ${part}.${s.fn} # inCaseAt @"${s.tab}" @"Stanowisko"`),
    `    ) # looped # with ${seed('Sala')}`, '',
  ].join('\n')
}

// Compile and bundle the mounts; returns, per mount, the bundle's module map
// (first line of each module, for attributing a profile).
export const buildMounts = async mounts => {
  rmSync(`${out}/src`, { recursive: true, force: true })
  mkdirSync(`${out}/src`, { recursive: true })
  for (const m of mounts) writeFileSync(`${out}/src/${m.name}.purs`, source(m))
  if (!existsSync(`${out}/output`)) cpSync('output', `${out}/output`, { recursive: true })
  const bin = n => `node_modules/.bin/${n}`
  const globs = spawnSync(bin('spago'), ['sources'], { encoding: 'utf8' }).stdout.trim().split('\n')
  const purs = spawnSync(bin('purs'), ['compile', ...globs, `${out}/src/*.purs`, '-o', `${out}/output`], { encoding: 'utf8', maxBuffer: 1 << 28 })
  const errors = (purs.stdout + purs.stderr).split(/^(?=Error \d+ of \d+:|Error found:|Warning \d+ of \d+:|Warning found:)/m).filter(b => /^Error/.test(b))
  if (errors.length) throw new Error(`bench mounts failed to compile:\n${errors.join('\n')}`)
  const head = linksFor('mdc3').join('\n')
  for (const m of mounts) {
    await build({
      stdin: { contents: `import { main } from './${out}/output/BenchMain.${m.name}/index.js'; main();`, resolveDir: process.cwd() },
      bundle: true, format: 'esm', outfile: `${out}/${m.name}.js`, logLevel: 'error',
    })
    writeFileSync(`${out}/${m.name}.html`, `<!doctype html>
<html lang="pl"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
${head}
<script type="module" src="${m.name}.js"></script></head><body></body></html>
`)
  }
}

// The modules of a bundle by the line they start at, from esbuild's `// path`
// comments, and the layer each belongs to.
export const moduleMap = name => {
  const lines = readFileSync(`${out}/${name}.js`, 'utf8').split('\n')
  const starts = []
  lines.forEach((line, i) => {
    const m = line.match(/^\/\/ (\.bench\/output|output|node_modules|src|demo)\/(.+)$/)
    if (m) starts.push({ line: i, path: `${m[1]}/${m[2]}` })
  })
  return starts
}

export const layerOf = path => {
  if (!path) return 'browser'
  const mod = path.match(/output\/([^/]+)\//)?.[1]
  if (mod) {
    if (/^(Diner|BenchMain)(\.|$)/.test(mod) && !/^Diner\.(View|Design)/.test(mod)) return 'view model'
    if (/^Diner\.(View|Design)/.test(mod)) return 'carrier'
    if (/^(PUI|Data\.Profunctor|QualifiedDo|Data\.Structural|Data\.Variant\.Case)(\.|$)/.test(mod)) return 'carrier'
    return 'libraries'
  }
  if (/node_modules\/(@material|lit|@lit|lit-html|lit-element|tslib)/.test(path)) return 'design system'
  if (/^src\//.test(path)) return 'carrier'
  return 'libraries'
}
