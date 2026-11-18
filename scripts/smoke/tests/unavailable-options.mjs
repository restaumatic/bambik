// `select'`'s unavailable options (MDC3, on diner; MDC2's `select'` keeps
// its law-bench row): the cases the model rules out are drawn disabled,
// re-read on every feed, and a real click on one picks nothing. Walks diner's table select: the seeded tables with an
// order in the kitchen or at the pass are disabled; a click on one leaves
// the picked table unchanged, a click on a free one stores it; paying the
// ready bill frees its table.
import { byName } from '../../demos.mjs'

export const demos = ['diner']
export const pages = [
  { url: `/${byName('diner').page}`, label: 'mdc3' },
]

const faces = {
  mdc3: {
    unavailable: `[...document.querySelectorAll('md-select-option')].filter(o => o.disabled).map(o => o.value)`,
    anchor: `document.querySelector('md-filled-select')`,
    picked: `document.querySelector('md-filled-select').value`,
    open: `document.querySelector('md-filled-select').open`,
    option: (c) => `[...document.querySelectorAll('md-select-option')].find(o => o.value === ${JSON.stringify(c)})`,
    tab: (c) => `[...document.querySelectorAll('md-primary-tab')].find(t => t.textContent.trim() === ${JSON.stringify(c)})`,
    pay: `[...document.querySelectorAll('md-text-button, md-filled-button, md-outlined-button')].find(b => b.textContent.includes('Zapłacono'))`,
  },
}

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  const face = faces[page]
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`document.body.querySelector('md-tabs, [role=tablist]') !== null`)); i++) await sleep(200)
  await sleep(500)

  const mouse = (type, x, y) => session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
  const click = async (js) => {
    const [x, y] = await ev(`(() => { const el = ${js}; el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    await mouse('mouseMoved', x, y)
    await mouse('mousePressed', x, y)
    await mouse('mouseReleased', x, y)
    await sleep(500)
  }

  assertEq(await ev(face.unavailable), ['3', '4'], `[${page}] the tables with an order in the kitchen or at the pass are disabled`)
  assertEq(await ev(face.picked), '1', `[${page}] the seeded table is picked`)

  await click(face.anchor)
  await click(face.option('3'))
  assertEq(await ev(face.picked), '1', `[${page}] a click on a disabled option picks nothing`)

  if (!(await ev(face.open))) await click(face.anchor)
  await click(face.option('2'))
  assertEq(await ev(face.picked), '2', `[${page}] a click on an available option stores it`)

  await click(face.tab('Wydawka'))
  await click(face.pay)
  await click(face.tab('Sala'))
  assertEq(await ev(face.unavailable), ['3'], `[${page}] paying the ready bill frees its table (re-read on the feed)`)
}
