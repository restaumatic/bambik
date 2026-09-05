// The stamp invariant as a runtime law, checked by axe-core: every citizen a
// vocabulary renders carries its accessible name — labelled form controls,
// captioned buttons, named selects, and no dangling aria reference (the
// group's `aria-labelledby` must point at its real heading). The audit runs
// over the running demo only (#demo-column — the library's output; the page
// chrome around it is not the vocabulary's), across all six counter twins and
// the flagship order-form, so a vocabulary that drops a stamp fails here with
// the offending element named by axe. The rule set is deliberately the
// name-and-reference core, not a full audit: these rules ARE the stamp
// invariant, stated by someone else's checker.
import { readFileSync } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const here = path.dirname(fileURLToPath(import.meta.url))
const axeSource = readFileSync(path.join(here, '../../../node_modules/axe-core/axe.min.js'), 'utf8')

// label-content-name-mismatch (WCAG 2.5.3) guards the verbatim-caption
// stamps: wherever a face carries aria-label beside visible text, the
// visible words must be contained in the accessible name
const rules = ['label', 'button-name', 'select-name', 'input-button-name', 'aria-valid-attr', 'aria-valid-attr-value', 'label-content-name-mismatch']

const twins = ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html']
export const demos = [...twins.map((t) => `demo/7guis/counter-${t}`), 'demo/nguis/order-form-mdc2']
export const pages = [
  ...twins.map((t) => ({ url: `/demo/7guis/counter-${t}/`, label: `counter-${t}` })),
  { url: '/demo/nguis/order-form-mdc2/', label: 'order-form-mdc2' },
]

export const run = async ({ ev, assertEq, sleep, page }) => {
  await sleep(1500) // custom elements, order-form's load action
  // the audit scopes to the running demo, so wait for page.js to collect it —
  // scanning the page chrome instead would test the harness, not the library
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)
  assertEq(await ev(`!!document.querySelector('#demo-column')`), true, `[${page}] the running demo is collected into #demo-column`)
  await ev(axeSource + '; !!window.axe')
  const violations = await ev(`axe.run(document.querySelector('#demo-column'),
      { runOnly: { type: 'rule', values: ${JSON.stringify(rules)} } })
    .then(r => r.violations.map(v => v.id + ' — ' + v.nodes.map(n => n.target.join(' ')).join(', ')))`)
  assertEq(violations, [], `[${page}] every rendered citizen carries its accessible name (axe: ${rules.join(', ')})`)
}
