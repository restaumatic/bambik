// One behavioral contract, six vocabularies: the counter walked by ROLE and
// ACCESSIBLE NAME instead of design-system markup, so the same test runs on
// every twin unchanged. This is the executable form of "a twin diff is
// view-only": the button is addressed as (button, "Count") — its case label,
// which every catalogue draws as the accessible name by the stamp invariant —
// and the readout as the leaf showing the count. A failure on one twin is
// that vocabulary breaking the stamp, not a selector gone stale.
import { a11y } from '../a11y.mjs'

const twins = ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html']
export const demos = twins.map((t) => `demo/7guis/counter-${t}`)
export const pages = twins.map((t) => ({ url: `/demo/7guis/counter-${t}/`, label: t }))

// the count readout carries no label by design (copy is a function, not a
// field), so it is asserted by content: some leaf element of the running
// demo shows exactly the count
const countShows = (n) => `[...document.querySelectorAll('#demo-column *')]
  .some(e => e.children.length === 0 && e.textContent.trim() === '${n}')`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  const ax = a11y(session)
  await sleep(600) // custom-element upgrade + FAST's deferred bind
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)

  const buttons = await ax.query({ role: 'button', name: 'Count' })
  assertEq(buttons.length >= 1, true, `[${page}] the AX tree has a button named "Count" — the case label is the accessible name`)
  assertEq(await ev(countShows(0)), true, `[${page}] seeded count renders`)

  await ax.click({ role: 'button', name: 'Count' })
  await sleep(150)
  assertEq(await ev(countShows(1)), true, `[${page}] clicking by role+name increments`)

  await ax.click({ role: 'button', name: 'Count' })
  await sleep(150)
  assertEq(await ev(countShows(2)), true, `[${page}] the mvu re-feed re-arms the button`)
}
