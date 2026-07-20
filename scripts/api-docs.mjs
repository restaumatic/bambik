// Refresh the committed API reference (doc/api/) from the generated docs:
// `npm run api-docs` runs `spago docs --format markdown` and then this
// script, which copies the library's own modules (not deps, not demos)
// into doc/api/ and writes its index. The module headers are the single
// source of truth for combinator contracts; this makes them browsable.
import { copyFileSync, mkdirSync, writeFileSync, existsSync } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..')
const src = path.join(root, 'generated-docs/md')
const out = path.join(root, 'doc/api')

// the public vocabulary, in reading order
const modules = [
  ['PUI', 'the core type, its instances, and the pipeline combinators'],
  ['PUI.HTML', 'the 1-1 HTML vocabulary: element oculars, leaves, collections'],
  ['PUI.SVG', 'the SVG element oculars'],
  ['PUI.MDC', 'the Material Design 2 components and oculars'],
  ['PUI.Web', 'the DOM carrier'],
  ['Data.Profunctor.Row', 'the shared row-constraint floor'],
  ['Data.Profunctor.Row.RecordToRecord', '×→× — editors: merge, lenses, Colens/feedback'],
  ['Data.Profunctor.Row.RecordToVariant', '×→+ — events: merge, Resolving/Coresolving, Shutter/folding'],
  ['Data.Profunctor.Row.VariantToRecord', '+→× — statuses: merge, Retaining/Coretaining, Reel/unfolding'],
  ['Data.Profunctor.Row.VariantToVariant', '+→+ — dispatch: merge, prisms, Coprism/iterate'],
  ['Data.Profunctor.Row.Sequence', 'the sequence direction: keyed collections'],
]

mkdirSync(out, { recursive: true })
const missing = modules.filter(([m]) => !existsSync(path.join(src, m + '.md')))
if (missing.length) {
  console.error('missing generated docs (run `spago docs --format markdown` first):')
  missing.forEach(([m]) => console.error('  ' + m))
  process.exit(1)
}
for (const [m] of modules) copyFileSync(path.join(src, m + '.md'), path.join(out, m + '.md'))

writeFileSync(path.join(out, 'README.md'),
`# Bambik API reference

Generated from the module headers and doc comments — the single source of
truth for every combinator's contract (what its type cannot say: gating,
priming, echo protocols, container ownership). Regenerate after changing
any doc comment: \`npm run api-docs\`.

| Module | What lives there |
|---|---|
${modules.map(([m, d]) => `| [${m}](${m}.md) | ${d} |`).join('\n')}

The narrative companion pieces: [why-bambik](../why-bambik.md) (the idea),
[row-profunctors](../row-profunctors.md) (the design note),
[type-errors](../type-errors.md) (reading the compile errors).
`)
console.log(`doc/api refreshed (${modules.length} modules + index)`)
