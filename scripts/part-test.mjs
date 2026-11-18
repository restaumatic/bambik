#!/usr/bin/env node
// Part tests: one part of a demo's view run in isolation, from prepared
// initial data, in headless Chrome — so a part can be developed on its own.
//
//   npm run part -- diner            # every part test of the app
//   npm run part -- diner Hall       # the tests of one part
//
// A part is a submodule of the view (`Diner.View.Hall` in View/Hall.purs), a
// stage from the model to the model; its tests sit in the app's test/
// directory at the part's own path, `test/View/Hall.test.mjs`:
//
//   export const tests = [
//     { name: 'an order sent makes its table busy', seed: 'dinnerService',
//       run: async ({ click, text, assert }) => {
//         await click('Dodaj', 'Risotto')
//         await click('Wyślij do kuchni')
//         assert.includes(await text(), 'Stolik zajęty', 'the table is busy')
//       } },
//   ]
//
// `seed` names an initial model: a value of the app's `<App>.Fixtures`
// (diner's Fixtures.purs) or of its view model module — prepared data,
// compiled. Each (part, seed) is mounted as an app of its own, `body $ part
// # looped # with seed`, compiled into .part/ (gitignored) against a copy
// of output/, and every test runs on a fresh page.
//
// What a test is handed:
//   click(label, scope?)        a real mouse click on the visible control whose
//                               accessible name or text is `label`; with a
//                               scope, the one inside the smallest element
//                               whose text contains `scope` (a card, a row)
//   fill(field, text, scope?)   type into the field named `field` (its label)
//   pick(field, option)         open the select named `field`, click `option`
//   disabled(label, scope?)     whether that control is disabled
//   text()                      the page's visible text
//   ev(js)                      the value of a page expression
//   shot(file)                  a screenshot
//   sleep(ms)
//   assert.equal(actual, expected, what)
//   assert.includes(text, part, what) / assert.excludes(text, part, what)
// A test fails on a failed assertion, a control it cannot find, an
// exception, or a console warning or error (the starvation watchdog's
// included). The run exits 1 when any test fails.
import { spawnSync } from 'node:child_process'
import { cpSync, existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs'
import { createServer } from 'node:http'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { pathToFileURL } from 'node:url'
import { build } from 'esbuild'
import { all, labels, linksFor } from './demos.mjs'
import { openSession } from './smoke/cdp.mjs'
import { launchChrome } from './smoke/chrome.mjs'

const fail = msg => { console.error(msg); process.exit(1) }
const [appName, partFilter] = process.argv.slice(2)
if (!appName) fail('usage: npm run part -- <app> [<Part>]   (see scripts/part-test.mjs)')
const demo = all.find(d => d.name === appName) ?? all.find(d => d.family === appName && d.variant)
if (!demo) fail(`no app "${appName}" in scripts/demos.mjs`)

const walk = dir => readdirSync(dir).flatMap(n => {
  const p = path.join(dir, n)
  return statSync(p).isDirectory() ? walk(p) : [p]
})
const moduleOf = src => src.match(/^module ([\w.]+)/m)?.[1]
const exportsOf = src => src.match(/^module [\w.]+\s*\(([\s\S]*?)\)\s*where/)?.[1].split(',').map(s => s.trim()).filter(n => /^[a-z_]/.test(n)) ?? []
const sources = walk(demo.dir).filter(f => f.endsWith('.purs')).map(file => ({ file, src: readFileSync(file, 'utf8'), mod: moduleOf(readFileSync(file, 'utf8')) }))
const viewModel = sources.find(s => s.mod === demo.viewModel)
const fixtures = sources.find(s => s.mod === `${demo.viewModel}.Fixtures`)
const seedModule = seed =>
  fixtures && exportsOf(fixtures.src).includes(seed) ? fixtures.mod
    : exportsOf(viewModel.src).includes(seed) ? viewModel.mod
      : fail(`seed "${seed}" is neither in ${demo.viewModel}.Fixtures nor exported by ${demo.viewModel}`)

// the part tests: test/<path of the part>.test.mjs
const testDir = path.join(demo.dir, 'test')
const suites = []
for (const file of existsSync(testDir) ? walk(testDir).filter(f => f.endsWith('.test.mjs')) : []) {
  const partName = path.basename(file, '.test.mjs')
  if (partFilter && partName !== partFilter) continue
  const partFile = path.join(demo.dir, path.relative(testDir, file).replace(/\.test\.mjs$/, '.purs'))
  const part = sources.find(s => s.file === partFile)
  if (!part || !part.mod?.startsWith(`${demo.mod}.`)) fail(`${file} tests no part of ${demo.mod} (no ${partFile})`)
  const exported = exportsOf(part.src)
  if (exported.length !== 1) fail(`${part.mod} exports ${exported.join(', ') || 'no value'}; a part exports its one stage`)
  const { tests } = await import(pathToFileURL(path.resolve(file)))
  suites.push({ file, partName, part: part.mod, fn: exported[0], tests })
}
if (!suites.length) fail(`no part tests${partFilter ? ` for ${partFilter}` : ''} under ${testDir}`)

// one mount per (part, seed), compiled together
const out = '.part'
rmSync(`${out}/src`, { recursive: true, force: true })
mkdirSync(`${out}/src`, { recursive: true })
const mountName = (s, seed) => `${s.partName}.${seed[0].toUpperCase()}${seed.slice(1)}`
const mounts = new Map()
for (const s of suites) for (const t of s.tests) {
  const name = mountName(s, t.seed)
  if (mounts.has(name)) continue
  const from = seedModule(t.seed)
  mounts.set(name, true)
  writeFileSync(`${out}/src/${name}.purs`, [
    `module PartMain.${name} (main) where`, '',
    'import Prelude', 'import Effect (Effect)', 'import PUI (looped, with)',
    `import PUI.Web.${labels[demo.variant]} (body)`,
    `import ${from} as Seed`, `import ${s.part} as Part`, '',
    'main :: Effect Unit',
    `main = body $ Part.${s.fn} # looped # with Seed.${t.seed}`, '',
  ].join('\n'))
}
if (!existsSync(`${out}/output`)) cpSync('output', `${out}/output`, { recursive: true })
const bin = n => `node_modules/.bin/${n}`
const globs = spawnSync(bin('spago'), ['sources'], { encoding: 'utf8' }).stdout.trim().split('\n')
const purs = spawnSync(bin('purs'), ['compile', ...globs, `${out}/src/*.purs`, '-o', `${out}/output`], { encoding: 'utf8', maxBuffer: 1 << 28 })
const errors = (purs.stdout + purs.stderr).split(/^(?=Error \d+ of \d+:|Error found:|Warning \d+ of \d+:|Warning found:)/m).filter(b => /^Error/.test(b))
if (errors.length) fail(`compile failed:\n${errors.join('\n')}`)
// the stylesheets the app's page loads: an application's design system's
// links, a demo page's template for its variant
const head = demo.set === 'apps' ? linksFor(demo.variant).join('\n')
  : readFileSync(path.join(demo.dir, 'index.html'), 'utf8').match(new RegExp(`<template data-variant="${demo.variant}"[^>]*>([\\s\\S]*?)</template>`))?.[1] ?? ''
for (const name of mounts.keys()) {
  await build({
    stdin: { contents: `import { main } from './${out}/output/PartMain.${name}/index.js'; main();`, resolveDir: process.cwd() },
    bundle: true, format: 'esm', outfile: `${out}/${name}.js`, logLevel: 'error',
  })
  writeFileSync(`${out}/${name}.html`, `<!doctype html>
<html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
${head}<style>body { margin: 16px; font-family: Roboto, sans-serif; }</style>
<script type="module" src="${name}.js"></script></head><body></body></html>
`)
}

const server = await new Promise(resolve => {
  const s = createServer((req, res) => {
    const file = path.join(out, decodeURIComponent(new URL(req.url, 'http://x').pathname))
    if (!file.startsWith(out) || !existsSync(file) || statSync(file).isDirectory()) { res.writeHead(404); return res.end() }
    res.writeHead(200, { 'content-type': file.endsWith('.js') ? 'text/javascript' : 'text/html' })
    res.end(readFileSync(file))
  })
  s.listen(0, '127.0.0.1', () => resolve(s))
})
const profile = mkdtempSync(path.join(tmpdir(), 'bambik-part-'))
const { proc, cdpBase } = await launchChrome(profile)
const sleep = ms => new Promise(r => setTimeout(r, ms))

// the visible control named `label` (or the field named `field`), innermost
// first, inside the smallest element whose text contains `scope`
const locate = (label, scope, field) => `(() => {
  const label = ${JSON.stringify(label)}, scope = ${JSON.stringify(scope ?? null)}, field = ${!!field}
  const visible = el => { const r = el.getBoundingClientRect(); return r.width > 0 && r.height > 0 }
  const name = el => (el.getAttribute('aria-label') || el.innerText || '').trim()
  let all = field ? [...document.querySelectorAll('[name="' + label + '"]')].filter(visible)
    : [...document.querySelectorAll('*')].filter(el => visible(el) && name(el) === label)
  if (!all.length && !field) all = [...document.querySelectorAll('*')].filter(el => visible(el) && name(el).includes(label))
  const innermost = all.filter(el => !all.some(o => o !== el && el.contains(o)))
  const size = el => { if (!scope) return 0; for (let a = el; a; a = a.parentElement) if ((a.innerText || '').includes(scope)) return (a.innerText || '').length; return Infinity }
  return innermost.filter(el => size(el) < Infinity).sort((a, b) => size(a) - size(b))[0] ?? null
})()`

class Failure extends Error {}
let passed = 0, failed = 0
try {
  for (const s of suites) {
    console.log(`\n▶ ${path.relative(process.cwd(), s.file)} (${s.part}.${s.fn})`)
    for (const t of s.tests) {
      const session = await openSession(cdpBase, 'about:blank')
      const { ev, send, events } = session
      await send('Emulation.setDeviceMetricsOverride', { width: t.width ?? 1100, height: 900, deviceScaleFactor: 1, mobile: (t.width ?? 1100) < 600 })
      await send('Page.navigate', { url: `http://127.0.0.1:${server.address().port}/${mountName(s, t.seed)}.html` })
      for (let i = 0; i < 50 && !(await ev('document.body && document.body.childElementCount > 0')); i++) await sleep(100)
      await sleep(800)
      const at = async (expr, what) => {
        const xy = await ev(`(() => { const el = ${expr}; if (!el) return null; el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
        if (!xy) throw new Failure(`no visible ${what}`)
        return xy
      }
      const mouse = (type, [x, y]) => send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
      const press = async xy => { await mouse('mouseMoved', xy); await mouse('mousePressed', xy); await mouse('mouseReleased', xy); await sleep(400) }
      const where = scope => scope ? ` inside "${scope}"` : ''
      const ui = {
        click: async (label, scope) => press(await at(locate(label, scope), `"${label}"${where(scope)}`)),
        fill: async (field, value, scope) => {
          await press(await at(locate(field, scope, true), `field "${field}"${where(scope)}`))
          await send('Input.dispatchKeyEvent', { type: 'keyDown', key: 'a', code: 'KeyA', modifiers: 2, windowsVirtualKeyCode: 65 })
          await send('Input.dispatchKeyEvent', { type: 'keyUp', key: 'a', code: 'KeyA', modifiers: 2, windowsVirtualKeyCode: 65 })
          await send('Input.insertText', { text: value })
          await sleep(400)
        },
        pick: async (field, option) => {
          await press(await at(locate(field, null, true), `select "${field}"`))
          await press(await at(locate(option), `option "${option}" of "${field}"`))
        },
        disabled: async (label, scope) => {
          const d = await ev(`(() => { const el = ${locate(label, scope)}; return el && (el.hasAttribute('disabled') || el.getAttribute('aria-disabled') === 'true' || !!el.disabled) })()`)
          if (d === null) throw new Failure(`no visible "${label}"${where(scope)}`)
          return d
        },
        text: () => ev(`document.body.innerText`),
        ev,
        shot: async file => writeFileSync(file, Buffer.from((await send('Page.captureScreenshot', { format: 'png', captureBeyondViewport: true })).result.data, 'base64')),
        sleep,
        assert: {
          equal: (actual, expected, what) => { if (JSON.stringify(actual) !== JSON.stringify(expected)) throw new Failure(`${what}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`) },
          includes: (text, part, what) => { if (!String(text).includes(part)) throw new Failure(`${what}: "${part}" is not on the page`) },
          excludes: (text, part, what) => { if (String(text).includes(part)) throw new Failure(`${what}: "${part}" is on the page`) },
        },
      }
      let problem = null
      try { await t.run(ui) } catch (e) { problem = e instanceof Failure ? e.message : `threw: ${e.stack ?? e}` }
      if (!problem && events.length) problem = `console: ${events.map(e => `${e.kind}: ${e.text}`).join(' | ')}`
      if (problem) {
        failed++
        const shot = path.resolve(out, `${mountName(s, t.seed)}.failed.png`)
        await ui.shot(shot)
        console.log(`  ✗ ${t.name} [${t.seed}]\n      ${problem}\n      screenshot: ${shot}`)
      } else {
        passed++
        console.log(`  ✓ ${t.name} [${t.seed}]`)
      }
      await session.close()
    }
  }
} finally {
  proc.kill()
  server.close()
  await sleep(300)
  rmSync(profile, { recursive: true, force: true, maxRetries: 10, retryDelay: 200 })
}
console.log(`\n${passed} passed, ${failed} failed`)
process.exit(failed ? 1 : 0)
