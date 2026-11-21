// B5 — application size: synthetic applications of 6 to 48 stations, each
// station a display, an editor, a keyed table and an event line of three
// buttons with their folds, all over one model and under one knot — the
// shape Diner has, at widths Diner does not. Read against the number of
// stations: what the compiler needs for the view and its view model, what
// the bundle weighs, what it costs to draw and to feed with one station
// open.
import { spawnSync } from 'node:child_process'
import { mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { gzipSync } from 'node:zlib'
import { build } from 'esbuild'
import { linksFor } from '../../demos.mjs'
import { out } from '../build.mjs'
import { exponent, round, summary, table } from '../stats.mjs'

const period = 251
const range = k => Array.from({ length: k }, (_, i) => i + 1)

const viewModel = k => [
  `module Size${k} (Item, Size, fresh, ticked, ${range(k).flatMap(i => [`added${i}`, `countLine${i}`, `items${i}`, `removed${i}`, `zeroed${i}`]).join(', ')}) where`, '',
  'import Prelude (map, show, (+), (-), (<>))', '', 'import Data.Array (range)', '',
  'type Item = { id :: Int, label :: String }', '',
  'type Size =', `  { "Stanowisko" :: [ ${range(k).map(i => `"S${i}" :: {}`).join(', ')} ]`, '  , ticks :: Int',
  ...range(k).flatMap(i => [`  , count${i} :: Int`, `  , "Notatka ${i}" :: String`]), '  }', '',
  'fresh :: Size', `fresh = { "Stanowisko": ."S1" {}, ticks: 0${range(k).map(i => `, count${i}: 0, "Notatka ${i}": ""`).join('')} }`, '',
  'ticked :: Size -> Size', 'ticked m = m { ticks = m.ticks + 1 }', '',
  ...range(k).flatMap(i => [
    `countLine${i} :: Size -> String`, `countLine${i} m = "Stanowisko ${i}: " <> show m.count${i}`, '',
    `items${i} :: Size -> Array Item`, `items${i} m = map (\\n -> { id: n, label: "Pozycja " <> show (n + m.count${i}) }) (range 1 5)`, '',
    `added${i} :: Size -> Size`, `added${i} m = m { count${i} = m.count${i} + 1 }`, '',
    `removed${i} :: Size -> Size`, `removed${i} m = m { count${i} = m.count${i} - 1 }`, '',
    `zeroed${i} :: Size -> Size`, `zeroed${i} m = m { count${i} = 0 }`, '',
  ]),
].join('\n')

const view = k => [
  `module Size${k}.View (app) where`, '', 'import Prelude ((#), ($), Unit)', '',
  'import Data.Profunctor.Row.RecordToVariant as RecordToVariant', 'import Data.Profunctor.Row.VariantToRecord as VariantToRecord',
  'import Data.Profunctor.Row.VariantToVariant as VariantToVariant', 'import Effect (Effect)',
  'import PUI (actedOf, fold, looped, PUI, stepped, ticks, with)', 'import PUI.Web (Web, inCaseAt, text, textAt)',
  'import PUI.Web.MDC3 (body, button, columnHeader, dataCell, dataRow, dataTable, filledTextField, headlineSmall, tab, tabBar)',
  'import QualifiedDo.Semigroupoid as Semigroupoid',
  `import Size${k} (Size, fresh, ticked, ${range(k).flatMap(i => [`added${i}`, `countLine${i}`, `items${i}`, `removed${i}`, `zeroed${i}`]).join(', ')})`, '',
  'app :: Effect Unit', 'app =', '  body $ Semigroupoid.do', '    tabBar @"Stanowisko" $ VariantToVariant.do',
  ...range(k).map(i => `      tab @"S${i}"`),
  ...range(k).map(i => `    station${i} # inCaseAt @"S${i}" @"Stanowisko"`),
  '    ( Semigroupoid.do', `      ticks @"Tyknięcie" { ms: ${period}.0 }`, '      fold @"Tyknięcie" ticked ) # stepped', '  # looped # with fresh', '',
  ...range(k).flatMap(i => [
    `station${i} :: PUI Web Size Size`, `station${i} = Semigroupoid.do`,
    `  headlineSmall $ text countLine${i}`, `  filledTextField @"Notatka ${i}"`,
    `  dataTable "S${i}" ( columnHeader "Pozycja" ) ( ( dataRow $ dataCell $ textAt @"label" ) # actedOf @"id" items${i} )`,
    '  ( Semigroupoid.do', '    RecordToVariant.do', `      button @"Dodaj ${i}"`, `      button @"Odejmij ${i}"`, `      button @"Wyzeruj ${i}"`,
    '    VariantToRecord.do', `      fold @"Dodaj ${i}" added${i}`, `      fold @"Odejmij ${i}" removed${i}`, `      fold @"Wyzeruj ${i}" zeroed${i} ) # stepped`, '',
  ]),
].join('\n')

const bin = n => `node_modules/.bin/${n}`
const compile = files => {
  const globs = spawnSync(bin('spago'), ['sources'], { encoding: 'utf8' }).stdout.trim().split('\n')
  const t = Date.now()
  const purs = spawnSync(bin('purs'), ['compile', ...globs, ...files, '-o', `${out}/output`], { encoding: 'utf8', maxBuffer: 1 << 28, timeout: 1200000 })
  const errors = (purs.stdout + purs.stderr).split(/^(?=Error \d+ of \d+:|Error found:|Warning \d+ of \d+:|Warning found:)/m).filter(b => /^Error/.test(b))
  if (errors.length) throw new Error(`a synthetic application failed to compile:\n${errors.join('\n').slice(0, 3000)}`)
  return (Date.now() - t) / 1000
}

export const size = {
  name: 'size',
  browser: true,
  mounts: () => [],
  run: async ({ quick, browser, log }) => {
    const ks = quick ? [6, 12, 24] : [6, 12, 24, 48]
    const dir = `${out}/size`
    rmSync(dir, { recursive: true, force: true })
    mkdirSync(dir, { recursive: true })
    // what the compiler needs with nothing new to compile but one empty module
    writeFileSync(`${dir}/Nothing.purs`, 'module BenchSizeNothing (nothing) where\n\nimport Prelude (Unit, unit)\n\nnothing :: Unit\nnothing = unit\n')
    compile([`${dir}/Nothing.purs`])
    writeFileSync(`${dir}/Nothing.purs`, 'module BenchSizeNothing (nothing) where\n\nimport Prelude (Unit, unit)\n\nnothing :: Unit\nnothing = unit\n\n')
    const idle = compile([`${dir}/Nothing.purs`])
    const by = {}
    for (const k of ks) {
      log(`  ${k} stations`)
      writeFileSync(`${dir}/Size${k}.purs`, viewModel(k))
      writeFileSync(`${dir}/Size${k}View.purs`, view(k))
      // compiled anew every run: what an earlier run left would be reused
      for (const m of [`Size${k}`, `Size${k}.View`]) rmSync(`${out}/output/${m}`, { recursive: true, force: true })
      const seconds = compile([`${dir}/Nothing.purs`, ...ks.filter(j => j <= k).flatMap(j => [`${dir}/Size${j}.purs`, `${dir}/Size${j}View.purs`])])
      const entry = { stdin: { contents: `import { app } from './${out}/output/Size${k}.View/index.js'; app();`, resolveDir: process.cwd() }, bundle: true, format: 'esm', logLevel: 'error' }
      await build({ ...entry, outfile: `${out}/Size${k}.js` })
      await build({ ...entry, minify: true, outfile: `${out}/Size${k}.min.js` })
      const min = readFileSync(`${out}/Size${k}.min.js`)
      writeFileSync(`${out}/Size${k}.html`, `<!doctype html>\n<html lang="pl"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">\n${linksFor('mdc3').join('\n')}\n<script type="module" src="Size${k}.js"></script></head><body></body></html>\n`)
      const cost = m => m.script + m.style + m.layout
      const draws = []
      let nodes = 0
      for (let i = 0; i < 4; i++) {
        const p = await browser.open(`Size${k}`)
        await p.waitFor(`document.querySelector('tbody tr') !== null`, 60000)
        await p.ev('document.body.offsetHeight')
        const m = await p.metrics()
        draws.push(cost(m)); nodes = m.nodes
        await p.close()
      }
      const p = await browser.open(`Size${k}`)
      await p.waitFor(`document.querySelector('tbody tr') !== null`, 60000)
      const next = async () => { const n = await p.ev(`window.__bench.beats(${period})`); await p.waitFor(`window.__bench.beats(${period}) > ${n}`, 20000) }
      const feeds = [], clicks = []
      await next()
      for (let i = 0; i < 8; i++) { await p.sleep(30); feeds.push((await p.measure(next)).total) }
      for (let i = 0; i < 8; i++) clicks.push((await p.measure(() => p.click('Dodaj 1'))).total)
      const problems = p.events.map(e => `${e.kind}: ${e.text}`)
      await p.close()
      by[k] = { compileSeconds: round(seconds - idle, 1), bundleKB: round(min.length / 1024, 1), gzipKB: round(gzipSync(min).length / 1024, 1), drawn: summary(draws), nodes, feed: summary(feeds), click: summary(clicks), problems }
    }
    return { ks, idleCompileSeconds: round(idle, 1), by }
  },
  report: ({ ks, idleCompileSeconds, by }) => {
    const row = (what, get) => { const ys = ks.map(k => get(by[k])); return [what, ...ys, exponent(ks, ys)] }
    const problems = ks.flatMap(k => by[k].problems)
    return ['### Application size: synthetic applications of growing width', '',
      `Each station is a display, an editor, a keyed table of five rows and three buttons with their folds; one model, one knot. Compile time is for the two modules (view and view model), over the ${idleCompileSeconds} s the compiler needs with nothing to do. Growth is the exponent against the number of stations.`, '',
      table(['', ...ks.map(k => `${k} stations`), 'growth'], [
        row('compile (s)', r => r.compileSeconds),
        row('bundle, minified (KB)', r => r.bundleKB),
        row('bundle, gzipped (KB)', r => r.gzipKB),
        row('drawn (ms)', r => r.drawn.p50),
        row('DOM nodes', r => r.nodes),
        row('a tick fed, one station open (ms)', r => r.feed.p50),
        row('a button clicked (ms)', r => r.click.p50),
      ]), '',
      problems.length ? `Console problems: ${[...new Set(problems)].join(' | ')}` : 'No exception and no console warning.', ''].join('\n')
  },
}
