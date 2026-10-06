// The view determines the view model (guardrails L18, writing.md *Writing
// order*): a view compiled with a typed hole in place of every value it
// imports from its view model module must report each hole at a concrete
// type, nothing unknown — and the view model module's exported signatures
// must be those types, verbatim. This rewrites each chosen demo's
// view that way — the import dropped, every imported name a `?name`, the
// module renamed under `Determined.` — into .determined/ (gitignored),
// compiles it against the real library into a copy of output/, and reads the
// compiler's hole list back. It fails on a second unknown, on an unknown
// anywhere (the model row not declared where the model first appears, or a
// derived row — a classifier's cases, an action's outcome, a projection's
// element row, a payload — not declared where it is introduced), on any
// error that is not a hole, on an exported signature that differs from
// the hint its views report (whitespace aside; a name every view reports
// at one type must be declared at exactly that type — a name reported at
// two different rows is the one case for a `forall`, and is not compared),
// and on an export no view imports (2026-10-03: the view determines the
// module, so a name no view line types is private, not exported).
// Runs over every registered demo (an all-view demo passes trivially);
// `node scripts/check-determined.mjs inbox-mdc3 …` narrows to named demos.
// About a minute for all, ten seconds for a few. `SHOW=all` prints every hint.
import { readFileSync, writeFileSync, mkdirSync, rmSync, cpSync, existsSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { sets } from './demos.mjs'

const importRe = /^import (\w+ViewModel) \(([^)]*)\)[ \t]*\n/m
const demos = Object.entries(sets).flatMap(([set, byDir]) =>
  Object.entries(byDir).map(([dir, [mod]]) => ({ dir, set, mod, file: `demo/${set}/${dir}/${mod}.purs` })))
const wanted = process.argv.slice(2)
const chosen = wanted.length ? demos.filter(d => wanted.includes(d.dir)) : demos
if (chosen.length === 0) { console.error('check-determined: no demo selected'); process.exit(2) }

rmSync('.determined', { recursive: true, force: true })
mkdirSync('.determined/src', { recursive: true })
cpSync('output', '.determined/output', { recursive: true })

// twins share the view model module from the unsuffixed sibling directory;
// a single-variant demo keeps it beside the view
const viewModelFile = (d, mod) => {
  const base = d.dir.replace(/-(mdc2|mdc3|shoelace|fluent|bootstrap|html)$/, '')
  return [`demo/${d.set}/${base}/${mod}.purs`, `demo/${d.set}/${d.dir}/${mod}.purs`].find(existsSync)
}

const holed = chosen.flatMap(d => {
  const src = readFileSync(d.file, 'utf8')
  const m = importRe.exec(src)
  if (!m) { console.log(`– ${d.dir}: no view model module, all view`); return [] }
  const names = m[2].split(',').map(s => s.trim()).filter(Boolean)
  let out = src.replace(m[0], '').replace(/^module (\w+)/m, 'module Determined.$1')
  // A derived row is declared once, where its function first appears; the
  // function's later occurrences are typed by the function itself once it is
  // written. Each `?name` is its own hole, so that second step is emulated:
  // the first occurrence's declaration is copied to the later, undeclared ones.
  const declared = new Map()
  for (const d of out.matchAll(/@\(((?:[^()]|\([^()]*\))*)\) ([A-Za-z_][\w']*)\b/g)) if (!declared.has(d[2])) declared.set(d[2], d[1])
  for (const [n, row] of declared)
    out = out.replace(new RegExp(`((?:shownWhen|inCase|provided|foreach|shownEach|listOf) @"[^"]+"(?: @"[^"]+")?(?: \\{[^}]*\\})?) ${n}\\b`, 'g'), `$1 @(${row}) ${n}`)
  // a record label (`tick:` in a `match`) or a field accessor is not the value
  for (const n of names) out = out.replace(new RegExp(`(?<![\\w."])${n}\\b(?!\\s*:(?!:))`, 'g'), `?${n}`)
  writeFileSync(`.determined/src/${d.mod}.purs`, out)
  return [{ ...d, names, module: m[1], viewModel: viewModelFile(d, m[1]) }]
})

// what every registered view imports from each view model module, twins
// included and regardless of which demos were chosen, so a name imported by
// one twin only is still a reached export
const importedBy = new Map()
for (const d of demos) {
  const m = importRe.exec(readFileSync(d.file, 'utf8'))
  if (!m) continue
  if (!importedBy.has(m[1])) importedBy.set(m[1], new Set())
  for (const n of m[2].split(',').map(s => s.trim()).filter(Boolean)) importedBy.get(m[1]).add(n)
}
const exportsOf = src => src.match(/^module \w+ \(([\s\S]*?)\) where/)?.[1].split(',').map(s => s.trim()).filter(Boolean) ?? []

const bin = n => `node_modules/.bin/${n}`
const globs = spawnSync(bin('spago'), ['sources'], { encoding: 'utf8' }).stdout.trim().split('\n')
const compile = () => {
  const purs = spawnSync(bin('purs'), ['compile', ...globs, '.determined/src/*.purs', '-o', '.determined/output'],
    { encoding: 'utf8', maxBuffer: 1 << 28 })
  // warnings are split off too and dropped: a redundant import in a holed view is not a determination failure
  return (purs.stdout + purs.stderr).split(/^(?=Error \d+ of \d+:|Error found:|Warning \d+ of \d+:|Warning found:)/m).filter(b => !/^Warning/.test(b))
}
const declOf = b => b.match(/in value declaration (\w+)/)?.[1] ?? '?'
const oneLine = t => t.replace(/\s+/g, ' ').replace(/ ,/g, ',').trim()
const check = (block, problems) => {
  const unknowns = [...new Set([...block.matchAll(/\b(t\d+) is an unknown type/g)].map(m => m[1]))]
  for (const t of unknowns) {
    const where = new RegExp(`(?<!\\| )\\b${t}\\b(?! is an unknown)`).test(block) ? 'inside a type' : 'as a row tail'
    problems.push(`${declOf(block)}: ${t} is unknown ${where}: the model row is not declared where the model first appears (\`# looped @( … ) # with seed\`, or the load action's outcome \`action @{ … }\` when a load stands before the knot), or a derived row is not declared where it is introduced (a classifier's cases, an action's outcome, a projection's element row, a payload, a trace state)`)
  }
  return unknowns
}

// The compiler reports the holes of one declaration per compile, so a view
// whose helper functions also hold holes is read in rounds: each round's
// reported holes become `hole` (the same fresh unknown to the checker) and
// the next declaration surfaces, until no hole is left.
const results = new Map(holed.map(d => [d.dir, { listed: [], tails: [], problems: [], seen: false, hints: new Map(), shown: [] }]))
const hint = (r, decl, name, type) => {
  const t = oneLine(type)
  if (!r.hints.has(name)) r.hints.set(name, new Set())
  r.hints.get(name).add(t)
  r.shown.push(`${decl}.${name} :: ${t}`)
}
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
    for (const b of summaries) {
      names.push(...[...b.matchAll(/^ {6}(\w+)\s+::/gm)].map(m => m[1])); r.tails.push(...check(b, r.problems))
      for (const m of b.matchAll(/^ {6}(\w+)\s+:: ([\s\S]*?)(?=^ {6}\w+\s+::|^\n\n)/gm)) hint(r, declOf(b), m[1], m[2])
    }
    for (const b of singles) {
      const name = b.match(/Hole '(\w+)'/)[1]
      names.push(name); r.tails.push(...check(b, r.problems))
      hint(r, declOf(b), name, b.match(/has the inferred type\n([\s\S]*?)\n\n/)[1])
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

// The signature is the hint, verbatim (writing.md *Business functions*): a
// name reported at one type everywhere a view uses it is declared at that
// type, whitespace aside. A name two lines report at different rows is the
// one case for a `forall`, and is left to the compiler.
const signatureOf = (src, name) => {
  const m = src.match(new RegExp(`^${name} ::([\\s\\S]*?)(?=^${name}(?![\\w']))`, 'm'))
  return m && oneLine(m[1])
}
let failed = false
for (const d of holed) {
  const r = results.get(d.dir)
  if (!r.seen) r.problems.push('no hole list: the view uses nothing it imports, or did not compile down to holes')
  if (r.seen && r.problems.length === 0) {
    if (!d.viewModel) r.problems.push('view model module file not found beside the view or in the unsuffixed sibling directory')
    else {
      const src = readFileSync(d.viewModel, 'utf8')
      for (const name of exportsOf(src)) if (!importedBy.get(d.module).has(name))
        r.problems.push(`${name}: exported from ${d.module} but no view imports it; the view determines the module, so it is private or gone`)
      for (const name of d.names) {
        const hints = r.hints.get(name)
        if (!hints || hints.size !== 1) continue
        const [h] = hints
        const sig = signatureOf(src, name)
        if (sig === null) r.problems.push(`${name}: no signature in ${d.viewModel}; the view reports ${h}`)
        else if (sig !== h) r.problems.push(`${name}: declared\n      ${sig}\n    but the view reports\n      ${h}`)
      }
    }
  }
  if (r.problems.length === 0)
    console.log(`✓ ${d.dir}: ${r.listed.length} holes determined, nothing unknown, signatures verbatim`)
  else { failed = true; console.error(`✗ ${d.dir}\n  ${r.problems.join('\n  ')}`) }
  if (process.env.SHOW && (r.problems.length || process.env.SHOW === 'all')) for (const l of r.shown) console.log(`    ${l}`)
}
process.exit(failed ? 1 : 0)
