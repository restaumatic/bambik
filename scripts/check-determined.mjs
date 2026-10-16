// The view determines the view model's interface up to its tails (guardrails
// L18, writing.md *Writing order*): a view compiled with a typed hole in
// place of every value it imports from its view model module reports each
// hole at the type the view gives it — which holes share a row, what each
// returns, every field and case a leaf names — and the rest of each row as an
// unknown, the **tail** only the model fills. The view model module's
// exported signatures must be those hints with the tails filled: verbatim
// where the view leaves nothing unknown (or, written through the module's
// type synonyms, equal to it as the compiler decides), otherwise an instance of the hint,
// one filling for every unknown across a declaration's holes. This rewrites
// each chosen demo's view that way — the import dropped, every imported name
// a `?name`, the module renamed under `Determined.` — into .determined/
// (gitignored), compiles it against the real library into a copy of output/,
// reads the compiler's hole list back, and has the compiler decide the
// instances: a probe module per demo applies a function typed at a
// declaration's hints, its unknowns quantified, to the module's values. It
// fails on any error that is not a hole, on a signature that is not its
// hint's instance or that quantifies (a name reported at two different rows
// is the one case for a `forall`, and is not compared), and on an export no
// view imports (2026-10-03: the view determines the module, so a name no view
// line types is private, not exported).
// Runs over every registered demo (an all-view demo passes trivially);
// `node scripts/check-determined.mjs inbox-mdc3 …` narrows to named demos.
// About a minute for all, ten seconds for a few. `SHOW=all` prints every hint.
// `--write` writes the signatures instead of comparing them, where the view
// leaves nothing unknown: every such exported signature becomes its hint in
// the layout of scripts/signature-layout.mjs, a definition without one gets
// one, and a name with no definition yet is appended as a signature over a
// runtime `hole`, ready to be filled. A hint with unknowns is the author's
// to fill, so a missing signature for one is reported, hint and all.
import { readFileSync, writeFileSync, mkdirSync, rmSync, cpSync, existsSync } from 'node:fs'
import { spawnSync } from 'node:child_process'
import { all } from './demos.mjs'
import { layoutSignature } from './signature-layout.mjs'

// the view's one import of its view model module, the module named by the registry
const importRe = d => d.viewModel && new RegExp(`^import (${d.viewModel.replace(/\./g, '\\.')}) \\(([^)]*)\\)[ \\t]*\\n`, 'm')
const demos = all
const write = process.argv.includes('--write')
const wanted = process.argv.slice(2).filter(a => a !== '--write')
const chosen = wanted.length ? demos.filter(d => wanted.includes(d.name)) : demos
if (chosen.length === 0) { console.error('check-determined: no demo selected'); process.exit(2) }

rmSync('.determined', { recursive: true, force: true })
mkdirSync('.determined/src', { recursive: true })
cpSync('output', '.determined/output', { recursive: true })

const holed = chosen.flatMap(d => {
  const src = readFileSync(d.file, 'utf8')
  const m = d.viewModel && importRe(d).exec(src)
  if (!m) { console.log(`– ${d.name}: no view model module, all view`); return [] }
  const names = m[2].split(',').map(s => s.trim()).filter(Boolean)
  let out = src.replace(m[0], '').replace(/^module ([\w.]+)/m, 'module Determined.$1')
  // a record label (`tick:` in a `match`) or a field accessor is not the value
  for (const n of names) out = out.replace(new RegExp(`(?<![\\w."])${n}\\b(?!\\s*:(?!:))`, 'g'), `?${n}`)
  writeFileSync(`.determined/src/${d.mod}.purs`, out)
  return [{ ...d, names, module: m[1], viewModel: existsSync(d.viewModelFile) && d.viewModelFile }]
})

// what every registered view imports from each view model module, twins
// included and regardless of which demos were chosen, so a name imported by
// one twin only is still a reached export
const importedBy = new Map()
for (const d of demos) {
  const m = d.viewModel && importRe(d).exec(readFileSync(d.file, 'utf8'))
  if (!m) continue
  if (!importedBy.has(m[1])) importedBy.set(m[1], new Set())
  for (const n of m[2].split(',').map(s => s.trim()).filter(Boolean)) importedBy.get(m[1]).add(n)
}
// values only: a type synonym the signatures share is exported for them, not for a view
const exportsOf = src => src.match(/^module [\w.]+ \(([\s\S]*?)\) where/)?.[1].split(',').map(s => s.trim()).filter(n => /^[a-z_]/.test(n)) ?? []
const synonymsOf = src => [...src.matchAll(/^type (\w+)/gm)].map(m => m[1])
const usesSynonym = (src, sig) => synonymsOf(src).some(t => new RegExp(`\\b${t}\\b`).test(sig))

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
const unknownsOf = t => [...new Set(t.match(/\bt\d+\b/g) ?? [])]
// a hint's unknowns renumbered in order of appearance, so one type reported in
// two declarations, numbered apart, compares equal
const canonical = t => { const u = unknownsOf(t); return t.replace(/\bt\d+\b/g, x => `u${u.indexOf(x)}`) }

// The compiler reports the holes of one declaration per compile, so a view
// whose helper functions also hold holes is read in rounds: each round's
// reported holes become `hole` (the same fresh unknown to the checker) and
// the next declaration surfaces, until no hole is left.
const results = new Map(holed.map(d => [d.name, { listed: [], problems: [], seen: false, hints: new Map(), groups: [], shown: [], written: [] }]))
// a group is the holes the compiler numbered together: one declaration's
// summary, or one hole reported alone
const hint = (r, group, decl, name, type) => {
  const t = oneLine(type)
  if (!r.hints.has(name)) r.hints.set(name, new Map())
  r.hints.get(name).set(canonical(t), t)
  group.set(name, t)
  r.shown.push(`${decl}.${name} :: ${t}`)
}
for (let round = 0; round < 12; round++) {
  const blocks = compile()
  let progressed = false
  for (const d of holed) {
    const r = results.get(d.name)
    const mine = blocks.filter(b => b.includes(`.determined/src/${d.mod}.purs`))
    const summaries = mine.filter(b => b.includes('holes in this declaration'))
    const singles = mine.filter(b => b.includes("Hole '") && !summaries.some(s => declOf(s) === declOf(b)))
    for (const b of mine.filter(b => !b.includes("Hole '") && !b.includes('holes in this declaration')))
      r.problems.push(`an error that is not a hole:\n${b.split('\n').slice(0, 8).join('\n')}`)
    const names = []
    for (const b of summaries) {
      names.push(...[...b.matchAll(/^ {6}(\w+)\s+::/gm)].map(m => m[1]))
      const group = new Map(); r.groups.push(group)
      for (const m of b.matchAll(/^ {6}(\w+)\s+:: ([\s\S]*?)(?=^ {6}\w+\s+::|^\n\n)/gm)) hint(r, group, declOf(b), m[1], m[2])
    }
    for (const b of singles) {
      const name = b.match(/Hole '(\w+)'/)[1]
      names.push(name)
      const group = new Map(); r.groups.push(group)
      hint(r, group, declOf(b), name, b.match(/has the inferred type\n([\s\S]*?)\n\n/)[1])
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

// The signature is the hint with its tails filled (writing.md *Business
// functions*): a name reported at one type everywhere a view uses it is
// declared at that type — verbatim, whitespace aside, where the view leaves
// nothing unknown, and as an instance the probe below confirms where it
// does. A name two lines report at different rows is the one case for a
// `forall`, and is left to the compiler.
const signatureRe = name => new RegExp(`^${name}\\s+::([\\s\\S]*?)(?=^${name}(?![\\w']))`, 'm')
const signatureOf = (src, name) => {
  const m = src.match(signatureRe(name))
  return m && oneLine(m[1])
}
// --write: the signature laid out from the hint, inserted above a definition
// that has none, or appended with a `hole` body when the name is not written yet
const writeSignature = (file, name, h) => {
  let src = readFileSync(file, 'utf8')
  const want = layoutSignature(name, h)
  const re = signatureRe(name)
  if (re.test(src)) src = src.replace(re, `${want}\n`)
  else if (new RegExp(`^${name}\\b`, 'm').test(src)) src = src.replace(new RegExp(`^(?=${name}\\b)`, 'm'), `${want}\n`)
  else {
    src = `${src.replace(/\n*$/, '\n')}\n${want}\n${name} = hole\n`
    src = src.replace(/^module ([\w.]+) \(([\s\S]*?)\) where/m, (m, mod, list) =>
      `module ${mod} (${[...list.split(',').map(x => x.trim()).filter(Boolean), name].sort((a, b) => a.localeCompare(b)).join(', ')}) where`)
    if (!/^import PUI\.Web \(hole\)/m.test(src)) src = src.replace(/^(import .*\n)(?!import)/m, `$1import PUI.Web (hole)\n`)
  }
  const before = readFileSync(file, 'utf8')
  if (src !== before) { writeFileSync(file, src); return true }
  return false
}
// The probe: per demo, one function per group typed at the group's hints,
// its unknowns quantified, applied to the module's own values — so it
// compiles exactly when one filling of the unknowns turns every hint of the
// group into its signature. The module's imports are copied, so the hints'
// types resolve as its signatures' do.
const probes = []
for (const d of holed) {
  const r = results.get(d.name)
  if (!r.seen || r.problems.length || !d.viewModel) continue
  const src = readFileSync(d.viewModel, 'utf8')
  const imports = src.match(/^import .*(?:\n[ \t]+.*)*$/gm) ?? []
  const lines = [`module Determined.Probe.${d.mod} where`, '', ...imports,
    `import ${d.module}`, `import ${d.module} as ProbeM`, 'import Data.Tuple as ProbeT', 'import Data.Unit as ProbeU', '']
  let n = 0
  for (const group of r.groups) {
    // a closed hint is compared verbatim, unless its signature is written
    // through a synonym: then the compiler decides, as for an open one
    const names = [...group.keys()].filter(name => r.hints.get(name)?.size === 1
      && (unknownsOf(group.get(name)).length || usesSynonym(src, signatureOf(src, name) ?? '')))
    if (!names.length) continue
    const us = [...new Set(names.flatMap(name => unknownsOf(group.get(name))))]
    const quantified = us.length ? `forall ${us.join(' ')}. ` : ''
    const tuple = (xs, unit) => xs.reduceRight((acc, x) => `(ProbeT.Tuple (${x}) ${acc})`, unit)
    lines.push(`probe${n} :: ${quantified}${tuple(names.map(name => group.get(name)), 'ProbeU.Unit')} -> ProbeU.Unit`,
      `probe${n} _ = ProbeU.unit`, '',
      `checked${n} :: ProbeU.Unit`, `checked${n} = probe${n} ${tuple(names.map(name => `ProbeM.${name}`), 'ProbeU.unit')}`, '')
    n++
  }
  if (!n) continue
  const file = `.determined/src/Probe.${d.mod}.purs`
  writeFileSync(file, lines.join('\n'))
  probes.push({ d, file })
}
if (probes.length && !write) {
  const blocks = compile()
  for (const { d, file } of probes) for (const b of blocks.filter(b => b.includes(file)))
    results.get(d.name).problems.push(`a signature that is not an instance of its hint:\n${b.split('\n').slice(0, 24).join('\n')}`)
}

let failed = false
for (const d of holed) {
  const r = results.get(d.name)
  if (!r.seen) r.problems.push('no hole list: the view uses nothing it imports, or did not compile down to holes')
  if (r.seen && r.problems.length === 0) {
    if (!d.viewModel) r.problems.push('view model module file not found beside the view')
    else {
      const src = readFileSync(d.viewModel, 'utf8')
      for (const name of exportsOf(src)) if (!importedBy.get(d.module).has(name))
        r.problems.push(`${name}: exported from ${d.module} but no view imports it; the view determines the module, so it is private or gone`)
      for (const name of d.names) {
        const hints = r.hints.get(name)
        if (!hints || hints.size !== 1) continue
        const [h] = hints.values()
        const open = unknownsOf(h).length > 0
        const sig = signatureOf(src, name)
        const aliased = sig !== null && usesSynonym(src, sig)
        if (write && !open && !aliased) { if (writeSignature(d.viewModel, name, h)) r.written.push(name); continue }
        if (sig === null) r.problems.push(`${name}: no signature in ${d.viewModel}; the view reports ${h}${open ? ', its unknowns the model\'s to fill' : ''}`)
        else if (/\bforall\b/.test(sig)) r.problems.push(`${name}: declared\n      ${sig}\n    quantified, but the view reports one type\n      ${h}`)
        else if (!open && !aliased && sig !== h) r.problems.push(`${name}: declared\n      ${sig}\n    but the view reports\n      ${h}`)
      }
    }
  }
  if (r.problems.length === 0 && write)
    console.log(`✎ ${d.name}: ${r.listed.length} holes determined, ${r.written.length ? `wrote ${r.written.join(', ')}` : 'every signature already written'}`)
  else if (r.problems.length === 0)
    console.log(`✓ ${d.name}: ${r.listed.length} holes, every signature its hint with the tails filled`)
  else { failed = true; console.error(`✗ ${d.name}\n  ${r.problems.join('\n  ')}`) }
  if (process.env.SHOW && (r.problems.length || process.env.SHOW === 'all')) for (const l of r.shown) console.log(`    ${l}`)
}
process.exit(failed ? 1 : 0)
