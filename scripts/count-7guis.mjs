// Measures the 7GUIs corpora for doc/7guis-comparison.md.
//
//   node scripts/count-7guis.mjs <corpora-dir>            # every corpus, per task
//   node scripts/count-7guis.mjs <corpora-dir> --files    # … and per file
//   node scripts/count-7guis.mjs - --files                # bambik alone, no clones
//
// <corpora-dir> holds the reference implementations cloned under the
// directory names below (`git clone https://github.com/<repo> <dir>`); each
// clone's HEAD must be the pinned commit, so a re-measure measures the same
// code. bambik is read from this repo.
//
// Counting rules, identical for every corpus: comments and blank lines
// stripped (line and block comments of the file's language, HTML comments,
// Svelte <style> blocks); every per-task source file counted, business
// modules included; code shared by all tasks (a corpus's helpers, the
// library or framework) and CSS excluded; tests excluded. Tokens are cl100k
// BPE via gpt-tokenizer, characters are of the same stripped text.
//
// gpt-tokenizer is not a repo dependency. Install it anywhere and point
// GPT_TOKENIZER at that directory, or `npm i --no-save gpt-tokenizer` here.
import { execFileSync } from 'node:child_process'
import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs'
import { createRequire } from 'node:module'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const req = createRequire(process.env.GPT_TOKENIZER ? `${process.env.GPT_TOKENIZER}/` : import.meta.url)
const { encode } = req('gpt-tokenizer')
const repo = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')

export const tasks = ['counter', 'temperature', 'flight-booker', 'timer', 'crud', 'circle-drawer', 'cells']

const bambik = view => ({
  root: repo,
  files: Object.fromEntries(Object.entries({
    counter: [`demo/7guis/counter/${view}.purs`, 'demo/7guis/counter/Counter.purs'],
    temperature: [`demo/7guis/temperature-converter/${view}.purs`, 'demo/7guis/temperature-converter/TemperatureConverter.purs'],
    'flight-booker': [`demo/7guis/flight-booker/${view}.purs`, 'demo/7guis/flight-booker/FlightBooker.purs'],
    timer: [`demo/7guis/timer/${view}.purs`, 'demo/7guis/timer/Timer.purs'],
    crud: [`demo/7guis/crud/${view}.purs`, 'demo/7guis/crud/Crud.purs'],
    'circle-drawer': [`demo/7guis/circle-drawer/${view}.purs`, 'demo/7guis/circle-drawer/CircleDrawer.purs'],
    cells: [`demo/7guis/cells/${view}.purs`, 'demo/7guis/cells/Cells.purs'],
  }).filter(([, [v]]) => existsSync(path.join(repo, v)))),
})

const svelteOfficial = 'apps/svelte.dev/content/examples/20-7guis'
const vue = 'src/examples/src'

export const corpora = {
  'bambik (MDC3)': bambik('MDC3'),
  'bambik (HTML)': bambik('HTML'),
  Elm: {
    dir: 'dwayne_elm-7guis', repo: 'dwayne/elm-7guis', commit: '16fd562',
    files: Object.fromEntries([
      ['counter', 'Counter'], ['temperature', 'TemperatureConverter'], ['flight-booker', 'FlightBooker'],
      ['timer', 'Timer'], ['crud', 'Crud'], ['circle-drawer', 'CircleDrawer'], ['cells', 'Cells'],
    ].map(([t, m]) => [t, [`src/${m}.elm`, `src/Task/${m}.elm`, `src/Task/${m}/`]])),
  },
  'React+MobX': {
    dir: 'eugenkiss_7guis-React-TypeScript-MobX', repo: 'eugenkiss/7guis-React-TypeScript-MobX', commit: 'e121076',
    files: {
      counter: ['src/app/guis/counter.tsx'],
      temperature: ['src/app/guis/tempconv.tsx'],
      'flight-booker': ['src/app/guis/flight.tsx'],
      timer: ['src/app/guis/timer.tsx'],
      crud: ['src/app/guis/crud.tsx'],
      'circle-drawer': ['src/app/guis/circles/model.ts', 'src/app/guis/circles/frame.tsx', 'src/app/guis/circles/drawer-traditional.tsx'],
      cells: ['src/app/guis/cells/'],
    },
  },
  'Vue 3': {
    dir: 'vuejs_docs', repo: 'vuejs/docs', commit: '40aa88a',
    files: Object.fromEntries([
      ['counter', 'counter'], ['temperature', 'temperature-converter'], ['flight-booker', 'flight-booker'],
      ['timer', 'timer'], ['crud', 'crud'], ['circle-drawer', 'circle-drawer'],
    ].map(([t, d]) => [t, [`${vue}/${d}/App/composition.js`, `${vue}/${d}/App/template.html`]])
      .concat([['cells', [`${vue}/cells/App/composition.js`, `${vue}/cells/App/template.html`,
        `${vue}/cells/Cell/composition.js`, `${vue}/cells/Cell/template.html`, `${vue}/cells/store.js`]]])),
  },
  'Svelte 5 (official)': {
    dir: 'sveltejs_svelte.dev', repo: 'sveltejs/svelte.dev', commit: '89f941c',
    files: Object.fromEntries([
      ['counter', '01-7guis-counter'], ['temperature', '02-7guis-temperature'], ['flight-booker', '03-7guis-flight-booker'],
      ['timer', '04-7guis-timer'], ['crud', '05-7guis-crud'], ['circle-drawer', '06-7guis-circles'],
    ].map(([t, d]) => [t, [`${svelteOfficial}/${d}/+assets/`]])),
  },
  'Svelte (joysofcode)': {
    dir: 'joysofcode_svelte-7GUIs', repo: 'joysofcode/svelte-7GUIs', commit: 'c794bdd',
    files: Object.fromEntries([
      ['counter', '01-counter'], ['temperature', '02-temperature-converter'], ['flight-booker', '03-flight-booker'],
      ['timer', '04-timer'], ['crud', '05-crud'], ['circle-drawer', '06-circle-drawer'], ['cells', '07-cells'],
    ].map(([t, d]) => [t, [`src/examples/${d}/index.svelte`]])),
  },
}

const sources = /\.(purs|elm|tsx?|jsx?|html|svelte)$/
const expand = p => p.endsWith(path.sep) && !existsSync(p) ? []
  : statSync(p).isDirectory()
  ? readdirSync(p).sort().flatMap(f => expand(path.join(p, f)))
  : sources.test(p) ? [p] : []

const haskellish = s => s.replace(/\{-[\s\S]*?-\}/g, '').replace(/^\s*--.*$/gm, '').replace(/\s+--\s.*$/gm, '')
const cish = s => s.replace(/\/\*[\s\S]*?\*\//g, '').replace(/^\s*\/\/.*$/gm, '').replace(/\s+\/\/\s.*$/gm, '')
const markup = s => s.replace(/<!--[\s\S]*?-->/g, '')
const strip = (file, src) => {
  const ext = path.extname(file)
  const out = ext === '.purs' || ext === '.elm' ? haskellish(src)
    : ext === '.html' ? markup(src)
    : ext === '.svelte' ? cish(markup(src.replace(/<style[\s\S]*?<\/style>/g, '')))
    : cish(src)
  return out.split('\n').filter(l => l.trim() !== '').join('\n')
}

export function measure(file) {
  const stripped = strip(file, readFileSync(file, 'utf8'))
  return { lines: stripped.split('\n').length, tokens: encode(stripped).length, chars: stripped.length }
}

const [dir, ...flags] = process.argv.slice(2)
if (!dir) { console.error('usage: node scripts/count-7guis.mjs <corpora-dir>|- [--files]'); process.exit(2) }
const perFile = flags.includes('--files')
const rows = []
for (const [name, c] of Object.entries(corpora)) {
  const root = c.root ?? (dir === '-' ? null : path.join(dir, c.dir))
  if (!root) continue
  if (!existsSync(root)) { console.error(`${name}: ${root} missing — git clone https://github.com/${c.repo} ${root}`); process.exit(1) }
  if (c.commit) {
    const head = execFileSync('git', ['-C', root, 'rev-parse', '--short=7', 'HEAD'], { encoding: 'utf8' }).trim()
    if (head !== c.commit) { console.error(`${name}: ${root} is at ${head}, pinned ${c.commit}`); process.exit(1) }
  }
  for (const task of tasks) {
    const globs = c.files[task]
    if (!globs) { rows.push({ name, task, tokens: null, chars: null, lines: null }); continue }
    const files = globs.flatMap(g => expand(path.join(root, g)))
    let tokens = 0, chars = 0, lines = 0
    for (const f of files) {
      const m = measure(f)
      tokens += m.tokens; chars += m.chars; lines += m.lines
      if (perFile) console.log(`  ${m.tokens}\t${m.chars}\t${m.lines}\t${path.relative(root, f)}`)
    }
    rows.push({ name, task, tokens, chars, lines })
  }
}

const names = [...new Set(rows.map(r => r.name))]
const cell = (n, t, k) => rows.find(r => r.name === n && r.task === t)?.[k]
for (const k of ['tokens', 'chars', 'lines']) {
  console.log(`\n${k}\n| task | ${names.join(' | ')} |\n|---|${names.map(() => '---:').join('|')}|`)
  for (const t of tasks) console.log(`| ${t} | ${names.map(n => cell(n, t, k) ?? '—').join(' | ')} |`)
  console.log(`| **total** | ${names.map(n => rows.filter(r => r.name === n).reduce((a, r) => a + (r[k] ?? 0), 0)).join(' | ')} |`)
}
