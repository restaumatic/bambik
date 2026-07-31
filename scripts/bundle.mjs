// Bundle demos for deploy. Every demo is a named module entered at its own
// function, so the esbuild entry is synthesized from the shared registry
// (scripts/demos.mjs) — `spago bundle-app` can only call Main.main.
//
//   node scripts/bundle.mjs               (all demos)
//   node scripts/bundle.mjs 7guis         (one set, or single demos by name)
import { build } from 'esbuild'
import { all, entryFor } from './demos.mjs'

const filters = process.argv.slice(2)
const demos = filters.length
  ? all.filter(d => filters.some(f => f === d.name || f === d.set))
  : all
if (!demos.length) {
  console.error(`no demo matches ${filters.join(' ')}`)
  process.exit(1)
}

for (const d of demos) {
  await build({
    stdin: { contents: entryFor(d), resolveDir: process.cwd() },
    bundle: true,
    minify: true,
    format: 'esm',
    outfile: `${d.dir}/bundle.js`,
  })
  console.log(`bundled ${d.name} (${d.mod}.${d.fn})`)
}
