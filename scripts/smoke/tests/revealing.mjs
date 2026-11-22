// A scroller that shows what it was last given (`revealing`), on the real
// DOM (diner): a dish added from the menu is in view on the order however
// long the order is, one added again is scrolled back to, and a feed that
// changes nothing in the order — a page of the menu turned — moves nothing.
import { byName } from '../../demos.mjs'

export const demos = ['diner']
export const pages = [{ url: `/${byName('diner').page}`, label: 'diner' }]

const scroller = `[...document.querySelectorAll('*')].find(e => getComputedStyle(e).overflowY === 'auto' && e.querySelector('tbody tr'))`
const tiles = `[...document.querySelectorAll('.md3-tile')].filter(t => t.innerText.includes('dostępne:'))`
const inView = i => `(() => {
  const box = ${scroller}.getBoundingClientRect()
  const row = ${scroller}.querySelectorAll('tbody tr')[${i}].getBoundingClientRect()
  return row.top >= box.top - 1 && row.bottom <= box.bottom + 1
})()`

export const run = async ({ ev, assertEq, sleep, page }) => {
  for (let i = 0; i < 60 && !(await ev(`${tiles}.length > 8`)); i++) await sleep(100)
  await sleep(300)
  for (let i = 0; i < 8; i++) { await ev(`${tiles}[${i}].click()`); await sleep(60) }
  await sleep(200)
  assertEq(await ev(`${scroller}.querySelectorAll('tbody tr').length`), 8, `[${page}] eight dishes on the order`)
  assertEq(await ev(`${scroller}.scrollHeight > ${scroller}.clientHeight`), true, `[${page}] more than the order shows at once`)
  assertEq(await ev(inView(7)), true, `[${page}] the dish added last is in view`)

  await ev(`${tiles}[0].click()`)
  await sleep(200)
  assertEq(await ev(inView(0)), true, `[${page}] a dish added again is scrolled back to`)

  await ev(`[...document.querySelectorAll('md-icon-button')].find(b => b.getAttribute('aria-label') === 'Następna strona').click()`)
  await sleep(300)
  assertEq(await ev(inView(0)), true, `[${page}] a feed that changes nothing in the order moves nothing`)
}
