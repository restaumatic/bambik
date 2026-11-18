// Bundle demos for deploy. Every demo is a named module entered at its own
// function, so the esbuild entry is synthesized from the shared registry
// (scripts/demos.mjs) — `spago bundle-app` can only call Main.main.
//
//   node scripts/bundle.mjs               (all demos)
//   node scripts/bundle.mjs 7guis         (a set, an app, or single demos by name)
//
// An application's index.html is written beside its bundle (`appPage`).
// It also stamps every demo page's `page.js?v=` with a hash of demo/page.js:
// the server sends no cache headers, so an unchanged URL lets a browser keep
// running a stale page.js after a deploy.
import { build } from 'esbuild'
import { createHash } from 'node:crypto'
import { readdirSync, readFileSync, writeFileSync } from 'node:fs'
import { appPage, entryFor, matching } from './demos.mjs'

const filters = process.argv.slice(2)
const demos = matching(filters)
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
    outfile: d.bundle,
  })
  if (d.set === 'apps') writeFileSync(`${d.dir}/index.html`, appPage(d))
  console.log(`bundled ${d.name} (${d.mod}.${d.fn})`)
}

const version = createHash('sha1').update(readFileSync('demo/page.js')).digest('hex').slice(0, 8)
for (const suite of ['demo/7guis', 'demo/nguis'])
  for (const app of readdirSync(suite, { withFileTypes: true }).filter(e => e.isDirectory())) {
    const page = `${suite}/${app.name}/index.html`
    let html
    try { html = readFileSync(page, 'utf8') } catch { continue }
    const stamped = html.replace(/src="\.\.\/\.\.\/page\.js(\?v=\w+)?"/, `src="../../page.js?v=${version}"`)
    if (stamped !== html) writeFileSync(page, stamped)
  }
