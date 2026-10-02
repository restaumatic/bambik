// The view determines the view model (guardrails L18, writing.md *Writing
// order*): a view compiled with a typed hole in place of every value it
// imports from its view model module must report each hole at a concrete
// type, every field and case known, unknown only at row tails. This rewrites each chosen demo's
// view that way — the import dropped, every imported name a `?name`, the
// module renamed under `Determined.` — into .determined/ (gitignored),
// compiles it against the real library into a copy of output/, and reads the
// compiler's hole list back. It fails on a second unknown, on an unknown
// anywhere but a row's tail (a field no line declares — `state @l @t`, an
// editor, an accessor — an action's outcome or payload without its type),
// and on any error that is not a hole. Tails are the view model's to close:
// `Cons` constraints bound a row from below, so every row the view declares
// field by field comes back open-tailed, and the view model's signature is
// the hole's type with its tails closed. Runs over every registered demo (an all-view
// demo passes trivially); `node scripts/check-determined.mjs inbox-mdc3 …`
// narrows to named demos. About a minute for all, ten seconds for a few.
import { readFileSync, writeFileSync, mkdirSync, rmSync, cpSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { sets } from './demos.mjs'

const importRe = /^import (\w+ViewModel) \(([^)]*)\)[ \t]*\n/m
const demos = Object.entries(sets).flatMap(([set, byDir]) =>
  Object.entries(byDir).map(([dir, [mod]]) => ({ dir, mod, file: `demo/${set}/${dir}/${mod}.purs` })))
const wanted = process.argv.slice(2)
const chosen = wanted.length ? demos.filter(d => wanted.includes(d.dir)) : demos
if (chosen.length === 0) { console.error('check-determined: no demo selected'); process.exit(2) }

rmSync('.determined', { recursive: true, force: true })
mkdirSync('.determined/src', { recursive: true })
cpSync('output', '.determined/output', { recursive: true })

const holed = chosen.flatMap(d => {
  const src = readFileSync(d.file, 'utf8')
  const m = importRe.exec(src)
  if (!m) { console.log(`– ${d.dir}: no view model module, all view`); return [] }
  const names = m[2].split(',').map(s => s.trim()).filter(Boolean)
  let out = src.replace(m[0], '').replace(/^module (\w+)/m, 'module Determined.$1')
  for (const n of names) out = out.replace(new RegExp(`(?<![\\w."])${n}\\b`, 'g'), `?${n}`)
  writeFileSync(`.determined/src/${d.mod}.purs`, out)
  return [{ ...d, names }]
})

const bin = n => `node_modules/.bin/${n}`
const globs = spawnSync(bin('spago'), ['sources'], { encoding: 'utf8' }).stdout.trim().split('\n')
const compile = () => {
  const purs = spawnSync(bin('purs'), ['compile', ...globs, '.determined/src/*.purs', '-o', '.determined/output'],
    { encoding: 'utf8', maxBuffer: 1 << 28 })
  return (purs.stdout + purs.stderr).split(/^(?=Error \d+ of \d+:|Error found:)/m)
}
const declOf = b => b.match(/in value declaration (\w+)/)?.[1] ?? '?'
const check = (block, problems) => {
  const unknowns = [...new Set([...block.matchAll(/\b(t\d+) is an unknown type/g)].map(m => m[1]))]
  for (const t of unknowns) {
    if (new RegExp(`(?<!\\| )(?<!Record )\\b${t}\\b(?! is an unknown)`).test(block))
      problems.push(`${declOf(block)}: ${t} is unknown inside a type: a field no line declares (a \`state @l @t\` leaf, an editor, an accessor), an action's outcome or a payload without its type, or a view helper composing two view model functions`)
  }
  return unknowns
}

// The compiler reports the holes of one declaration per compile, so a view
// whose helper functions also hold holes is read in rounds: each round's
// reported holes become `hole` (the same fresh unknown to the checker) and
// the next declaration surfaces, until no hole is left.
const results = new Map(holed.map(d => [d.dir, { listed: [], tails: [], problems: [], seen: false, shown: [] }]))
for (let round = 0; round < 12; round++) {
  const blocks = compile()
  let progressed = false
  for (const d of holed) {
    const r = results.get(d.dir)
    const mine = blocks.filter(b => b.includes(`.determined/src/${d.mod}.purs`))
    const summaries = mine.filter(b => b.includes('holes in this declaration'))
    const singles = mine.filter(b => b.includes("Hole '") && !summaries.some(s => declOf(s) === declOf(b)))
    for (const b of mine.filter(b => !b.includes("Hole '") && !b.includes('holes in this declaration')))
      r.problems.push(`an error that is not a hole:\n${b.split('\n').slice(0, 8).join('\n')}`)
    const names = []
    const oneLine = t => t.replace(/\s+/g, ' ').replace(/ ,/g, ',').trim()
    for (const b of summaries) {
      names.push(...[...b.matchAll(/^ {6}(\w+)\s+::/gm)].map(m => m[1])); r.tails.push(...check(b, r.problems))
      if (process.env.SHOW) for (const m of b.matchAll(/^ {6}(\w+)\s+:: ([\s\S]*?)(?=^ {6}\w+\s+::|^\n\n)/gm)) r.shown.push(`${declOf(b)}.${m[1]} :: ${oneLine(m[2])}`)
    }
    for (const b of singles) {
      names.push(b.match(/Hole '(\w+)'/)[1]); r.tails.push(...check(b, r.problems))
      if (process.env.SHOW) r.shown.push(`${declOf(b)}.${b.match(/Hole '(\w+)'/)[1]} :: ${oneLine(b.match(/has the inferred type\n([\s\S]*?)\n\n/)[1])}`)
    }
    if (names.length) {
      r.seen = true; progressed = true; r.listed.push(...names)
      const file = `.determined/src/${d.mod}.purs`
      let src = readFileSync(file, 'utf8')
      for (const n of new Set(names)) src = src.replace(new RegExp(`\\?${n}\\b`, 'g'), 'hole')
      if (!/^import PUI\.Web \(.*\bhole\b/m.test(src)) src = src.replace(/^import PUI\.Web \(/m, 'import PUI.Web (hole, ')
      if (!/^import PUI\.Web /m.test(src)) src = src.replace(/^import PUI /m, 'import PUI.Web (hole)\nimport PUI ')
      writeFileSync(file, src)
    }
  }
  if (!progressed) {
    if (round === 0 && blocks.length > 1) console.error(`compile failed before any hole was reported:\n${blocks[1].split('\n').slice(0, 12).join('\n')}`)
    break
  }
}
let failed = false
for (const d of holed) {
  const r = results.get(d.dir)
  if (!r.seen) r.problems.push('no hole list: the view uses nothing it imports, or did not compile down to holes')
  if (r.problems.length === 0)
    console.log(`✓ ${d.dir}: ${r.listed.length} holes determined${r.tails.length ? ', unknowns only as row tails' : ', nothing unknown'}`)
  else { failed = true; console.error(`✗ ${d.dir}\n  ${r.problems.join('\n  ')}`) }
  if (process.env.SHOW && (r.problems.length || process.env.SHOW === 'all')) for (const l of r.shown) console.log(`    ${l}`)
}
process.exit(failed ? 1 : 0)
