// The view determines the view model (guardrails L18, writing.md *Writing
// order*): a view compiled with a typed hole in place of every value it
// imports from its view model module must report each hole at a concrete
// type, the model's tail the only unknown. This rewrites each chosen demo's
// view that way — the import dropped, every imported name a `?name`, the
// module renamed under `Determined.` — into .determined/ (gitignored),
// compiles it against the real library into a copy of output/, and reads the
// compiler's hole list back. It fails on a second unknown, on an unknown
// anywhere but a row's tail (`Record t1`, `[ … | t2 ]`: a list's row or a
// pane's states not declared, a stored field read through a function the
// holes cannot see through), and on any error that is not a hole. Default
// set: every demo whose view imports a `*ViewModel` module (the swept ones);
// `node scripts/check-determined.mjs inbox-mdc3 …` narrows to named demos.
import { readFileSync, writeFileSync, mkdirSync, rmSync, cpSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { sets } from './demos.mjs'

const importRe = /^import (\w+(?:ViewModel|Logic)) \(([^)]*)\)[ \t]*\n/m
const demos = Object.entries(sets).flatMap(([set, byDir]) =>
  Object.entries(byDir).map(([dir, [mod]]) => ({ dir, mod, file: `demo/${set}/${dir}/${mod}.purs` })))
const wanted = process.argv.slice(2)
const chosen = demos.filter(d => wanted.length
  ? wanted.includes(d.dir)
  : (importRe.exec(readFileSync(d.file, 'utf8'))?.[1] ?? '').endsWith('ViewModel'))
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
const purs = spawnSync(bin('purs'), ['compile', ...globs, '.determined/src/*.purs', '-o', '.determined/output'],
  { encoding: 'utf8', maxBuffer: 1 << 28 })
const blocks = (purs.stdout + purs.stderr).split(/^(?=Error \d+ of \d+:|Error found:)/m)

let failed = false
for (const d of holed) {
  const mine = blocks.filter(b => b.includes(`.determined/src/${d.mod}.purs`))
  const summary = mine.find(b => b.includes('holes in this declaration'))
  const foreign = mine.filter(b => !b.includes("Hole '") && !b.includes('holes in this declaration'))
  const problems = []
  if (!summary) problems.push('no hole list: the view uses nothing it imports, or did not compile down to holes')
  for (const b of foreign) problems.push(`an error that is not a hole:\n${b.split('\n').slice(0, 8).join('\n')}`)
  if (summary) {
    const listed = [...summary.matchAll(/^ {6}(\w+)\s+::/gm)].map(m => m[1])
    const unknowns = [...new Set([...summary.matchAll(/\b(t\d+) is an unknown type/g)].map(m => m[1]))]
    if (unknowns.length > 1) problems.push(`${unknowns.length} unknowns (${unknowns.join(', ')}): the model has one tail`)
    for (const t of unknowns) {
      if (new RegExp(`(?<!\\| )\\b${t}\\b(?! is an unknown)`).test(summary))
        problems.push(`${t} appears off a row's tail: a list's row or a pane's states is undeclared, or a stored field is read through a function instead of an accessor on its line`)
    }
    if (problems.length === 0)
      console.log(`✓ ${d.dir}: ${listed.length} holes determined${unknowns.length ? `, the model open only at its tail ${unknowns[0]}` : ''}`)
  }
  if (problems.length) { failed = true; console.error(`✗ ${d.dir}\n  ${problems.join('\n  ')}`) }
}
process.exit(failed ? 1 : 0)
