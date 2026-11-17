// `button'`'s and `iconButton'`'s `disabled` predicate (MDC3, MDC2): while it holds for the row
// last fed, the button is drawn disabled and a real click reports nothing;
// once the row changes so it no longer holds, the button reports again.
// Walks diner's dish cards: ordering the last portions of a dish disables
// its add button and the + of its order row, a click on it adds nothing,
// taking one portion back enables the + again, and removing the dish from
// the order — the trash icon asks first: its Cancel, captioned "Nie, nie
// usuwaj dania", keeps the line, the confirm removes it — gives the portions back and enables the add button.
import { byName } from '../../demos.mjs'

export const demos = ['diner-mdc3', 'diner-mdc2']
export const pages = [
  { url: `/${byName('diner-mdc3').page}`, label: 'mdc3' },
  { url: `/${byName('diner-mdc2').page}`, label: 'mdc2' },
]

const addButton = (dish) => `(() => {
  const card = [...document.querySelectorAll('.md3-card, .mdc-card')].find(c => c.innerText.trim().startsWith(${JSON.stringify(dish)}))
  return card.querySelector('md-filled-button, button')
})()`
const quantity = (dish) => `(() => {
  const row = [...document.querySelectorAll('tr')].find(r => r.cells[0] && r.cells[0].textContent.trim() === ${JSON.stringify(dish)} && r.cells.length > 3)
  return row ? row.cells[1].textContent.trim() : ''
})()`
const rowButton = (dish, label) => `(() => {
  const row = [...document.querySelectorAll('tr')].find(r => r.cells[0] && r.cells[0].textContent.trim() === ${JSON.stringify(dish)} && r.cells.length > 3)
  return [...row.querySelectorAll('md-icon-button, button')].find(b => b.getAttribute('aria-label') === ${JSON.stringify(label)})
})()`

const dialogButton = (caption) => `[...document.querySelectorAll('md-dialog [slot=actions] *, .mdc-dialog__actions button')].find(b => b.textContent.trim().toLowerCase() === ${JSON.stringify(caption.toLowerCase())})`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)
  await sleep(500)

  const mouse = (type, x, y) => session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
  const click = async (js) => {
    const [x, y] = await ev(`(() => { const el = ${js}; el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    await mouse('mouseMoved', x, y)
    await mouse('mousePressed', x, y)
    await mouse('mouseReleased', x, y)
    await sleep(400)
  }
  const disabled = (dish) => ev(`${addButton(dish)}.hasAttribute('disabled')`)

  assertEq(await disabled('Tiramisu'), false, `[${page}] a dish in stock has its add button enabled`)
  await click(addButton('Tiramisu'))
  await click(addButton('Tiramisu'))
  assertEq(await ev(quantity('Tiramisu')), '2', `[${page}] the last two portions are ordered`)
  assertEq(await disabled('Tiramisu'), true, `[${page}] the sold-out dish's add button is disabled`)

  await ev(`(() => { window.__clicks = 0; ${addButton('Tiramisu')}.addEventListener('click', () => window.__clicks++) })()`)
  await click(addButton('Tiramisu'))
  assertEq(await ev(`window.__clicks`), 0, `[${page}] a real click on the disabled button reaches no click listener`)
  assertEq(await ev(quantity('Tiramisu')), '2', `[${page}] and adds nothing`)
  assertEq(await ev(`${rowButton('Tiramisu', 'Dodano porcję')}.hasAttribute('disabled')`), true, `[${page}] the order row's + is disabled too`)

  await click(rowButton('Tiramisu', 'Zwrócono porcję'))
  assertEq(await ev(quantity('Tiramisu')), '1', `[${page}] one portion taken back`)
  assertEq(await ev(`${rowButton('Tiramisu', 'Dodano porcję')}.hasAttribute('disabled')`), false, `[${page}] the order row's + is enabled again`)
  await click(rowButton('Tiramisu', 'Dodano porcję'))
  assertEq(await ev(quantity('Tiramisu')), '2', `[${page}] and adds a portion`)

  await click(rowButton('Tiramisu', 'Usuń danie'))
  await sleep(400)
  await click(dialogButton('Nie, nie usuwaj dania'))
  await sleep(400)
  assertEq(await ev(quantity('Tiramisu')), '2', `[${page}] the trash icon asks first; Cancel keeps the line`)
  await click(rowButton('Tiramisu', 'Usuń danie'))
  await sleep(400)
  await click(dialogButton('Usuń danie'))
  await sleep(400)
  assertEq(await ev(quantity('Tiramisu')), '', `[${page}] confirming removes the line`)
  assertEq(await disabled('Tiramisu'), false, `[${page}] the portions back, the button is enabled again`)
  await click(addButton('Tiramisu'))
  assertEq(await ev(quantity('Tiramisu')), '1', `[${page}] and it reports again`)
}
