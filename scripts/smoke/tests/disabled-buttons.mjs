// `iconButton'`'s `disabled` predicate (MDC3, on diner; `button'`'s and
// MDC2's keep their law-bench rows) and diner's own tile, dimmed by the
// same kind of predicate: while it holds for the row last fed, the control
// is drawn disabled and a real click reports nothing; once the row changes
// so it no longer holds, the control reports again.
// Walks diner's menu at the table the service opens on: ordering the last
// portions of a dish dims its tile and disables the + of its order row, a
// click on the tile adds nothing, taking one portion back enables the +
// again, and removing the dish from the order — the trash icon asks first:
// its Cancel, captioned "Nie, nie usuwaj dania", keeps the line, the
// confirm removes it — gives the portions back and the tile takes a tap.
import { byName } from '../../demos.mjs'

export const demos = ['diner']
export const pages = [
  { url: `/${byName('diner').page}`, label: 'mdc3' },
]

const addButton = (dish) => `[...document.querySelectorAll('.md3-tile')].find(t => t.querySelector('.md-typescale-label-large')?.innerText.trim() === ${JSON.stringify(dish)})`
const rowOf = (dish) => `[...document.querySelectorAll('table[aria-label="Zamówienie"] tbody tr')].find(r => r.cells[0].innerText.trim().startsWith(${JSON.stringify(dish)}))`
const quantity = (dish) => `(() => {
  const row = ${rowOf(dish)}
  return row ? [...row.cells[1].childNodes[0].childNodes].filter(n => n.nodeType === 3).map(n => n.nodeValue).join('').trim() : ''
})()`
const rowButton = (dish, label) => `[...${rowOf(dish)}.querySelectorAll('md-icon-button')].find(b => b.getAttribute('aria-label') === ${JSON.stringify(label)})`

const dialogButton = (caption) => `[...document.querySelectorAll('md-dialog[open] [slot=actions] *')].find(b => b.textContent.trim().toLowerCase() === ${JSON.stringify(caption.toLowerCase())})`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await sleep(600)
  // the service loads before anything is listed
  for (let i = 0; i < 40 && !(await ev(`document.querySelector('.md3-tile .md-typescale-label-large') !== null`)); i++) await sleep(200)
  await sleep(500)

  const mouse = (type, x, y) => session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
  const click = async (js) => {
    const [x, y] = await ev(`(() => { const el = ${js}; el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    await mouse('mouseMoved', x, y)
    await mouse('mousePressed', x, y)
    await mouse('mouseReleased', x, y)
    await sleep(400)
  }
  const disabled = (dish) => ev(`${addButton(dish)}.getAttribute('aria-disabled') === 'true'`)

  assertEq(await disabled('Affogato ze szpinakiem'), false, `[${page}] a dish in stock has its tile taking taps`)
  await click(addButton('Affogato ze szpinakiem'))
  await click(addButton('Affogato ze szpinakiem'))
  assertEq(await ev(quantity('Affogato ze szpinakiem')), '2', `[${page}] the last two portions are ordered`)
  assertEq(await disabled('Affogato ze szpinakiem'), true, `[${page}] the sold-out dish's tile is disabled`)

  await ev(`(() => { window.__clicks = 0; ${addButton('Affogato ze szpinakiem')}.addEventListener('click', () => window.__clicks++) })()`)
  await click(addButton('Affogato ze szpinakiem'))
  assertEq(await ev(`window.__clicks`), 0, `[${page}] a real click on the disabled tile reaches no click listener`)
  assertEq(await ev(quantity('Affogato ze szpinakiem')), '2', `[${page}] and adds nothing`)
  assertEq(await ev(`${rowButton('Affogato ze szpinakiem', 'Dodano porcję')}.hasAttribute('disabled')`), true, `[${page}] the order row's + is disabled too`)

  await click(rowButton('Affogato ze szpinakiem', 'Zwrócono porcję'))
  assertEq(await ev(quantity('Affogato ze szpinakiem')), '1', `[${page}] one portion taken back`)
  assertEq(await ev(`${rowButton('Affogato ze szpinakiem', 'Dodano porcję')}.hasAttribute('disabled')`), false, `[${page}] the order row's + is enabled again`)
  await click(rowButton('Affogato ze szpinakiem', 'Dodano porcję'))
  assertEq(await ev(quantity('Affogato ze szpinakiem')), '2', `[${page}] and adds a portion`)

  await click(rowButton('Affogato ze szpinakiem', 'Usuń danie'))
  await sleep(400)
  await click(dialogButton('Nie, nie usuwaj dania'))
  await sleep(400)
  assertEq(await ev(quantity('Affogato ze szpinakiem')), '2', `[${page}] the trash icon asks first; Cancel keeps the line`)
  await click(rowButton('Affogato ze szpinakiem', 'Usuń danie'))
  await sleep(400)
  await click(dialogButton('Usuń danie'))
  await sleep(400)
  assertEq(await ev(quantity('Affogato ze szpinakiem')), '', `[${page}] confirming removes the line`)
  assertEq(await disabled('Affogato ze szpinakiem'), false, `[${page}] the portions back, the tile takes taps again`)
  await click(addButton('Affogato ze szpinakiem'))
  assertEq(await ev(quantity('Affogato ze szpinakiem')), '1', `[${page}] and it reports again`)
}
