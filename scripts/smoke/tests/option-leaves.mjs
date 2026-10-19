// Selectors as a group over option leaves (MDC3 spike): each option is a
// leaf of one case of the field, merged `VariantToVariant.do`. Walks
// espresso-bar's four selectors with real clicks, checking that a pick is
// stored (the summary line follows) and exclusive (exactly one option shows
// picked), and that a model change made elsewhere (the "The usual" preset)
// is shown by every group.
import { byName } from '../../demos.mjs'

export const demos = ['espresso-bar-mdc3', 'checkout-mdc3']
export const pages = [
  { url: `/${byName('espresso-bar-mdc3').page}`, label: 'mdc3' },
  { url: `/${byName('checkout-mdc3').page}`, label: 'stepper' },
]

const summary = `document.querySelector('#demo-column .md-typescale-body-medium, #demo-column [class*="body-medium"]')?.textContent ?? ''`
const picked = {
  tab: `[...document.querySelectorAll('md-primary-tab')].filter(t => t.active).map(t => t.textContent.trim())`,
  segment: `[...document.querySelectorAll('.md3-segmented-button__segment')].filter(s => s.getAttribute('aria-checked') === 'true').map(s => s.textContent.trim())`,
  select: `document.querySelector('md-filled-select').value`,
  radio: `[...document.querySelectorAll('md-radio')].filter(r => r.checked).map(r => r.getAttribute('aria-label'))`,
}

export const run = async (ctx) => (ctx.page === 'stepper' ? stepperWalk : espressoWalk)(ctx)

// checkout's packaged stepper: Back and Next pick the neighbouring step leaf
const stepperWalk = async ({ ev, session, assertEq, sleep, page }) => {
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)
  await sleep(500)
  const caption = () => ev(`document.querySelector('.md3-stepper__caption').textContent`)
  const press = async (label) => {
    const [x, y] = await ev(`(() => { const el = [...document.querySelectorAll('.md3-stepper md-text-button')].find(b => b.textContent.includes(${JSON.stringify(label)})); el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    for (const type of ['mouseMoved', 'mousePressed', 'mouseReleased']) await session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
    await sleep(400)
  }
  assertEq(await caption(), 'Step 1 of 3 · Cart', `[${page}] the seeded step is named`)
  await press('Next')
  assertEq(await caption(), 'Step 2 of 3 · Shipping', `[${page}] Next moves to the second step`)
  await press('Next')
  assertEq(await caption(), 'Step 3 of 3 · Payment', `[${page}] Next moves to the last step`)
  assertEq(await ev(`[...document.querySelectorAll('.md3-stepper md-text-button')].find(b => b.textContent.includes('Next')).hasAttribute('disabled')`), true, `[${page}] Next is disabled on the last step`)
  await press('Back')
  assertEq(await caption(), 'Step 2 of 3 · Shipping', `[${page}] Back moves to the previous step`)
}

const espressoWalk = async ({ ev, session, assertEq, sleep, page }) => {
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
  const line = () => ev(summary)

  assertEq(await ev(picked.tab), ['Cappuccino'], `[${page}] the seeded drink's tab is the active one`)
  assertEq(await ev(picked.segment), ['Medium'], `[${page}] the seeded size's segment is the checked one`)
  assertEq(await ev(picked.select), 'Whole milk', `[${page}] the seeded milk is selected`)
  assertEq(await ev(picked.radio), ['Medium'], `[${page}] the seeded roast's radio is the checked one`)

  await click(`[...document.querySelectorAll('md-primary-tab')].find(t => t.textContent.trim() === 'Latte')`)
  assertEq(await ev(picked.tab), ['Latte'], `[${page}] clicking a tab makes it the one active tab`)
  assertEq((await line()).includes('Latte'), true, `[${page}] the picked drink is stored (the summary says Latte)`)

  await click(`[...document.querySelectorAll('.md3-segmented-button__segment')].find(s => s.textContent.trim() === 'Large')`)
  assertEq(await ev(picked.segment), ['Large'], `[${page}] clicking a segment makes it the one checked segment`)
  assertEq((await line()).includes('Large Latte'), true, `[${page}] the picked size is stored (the summary says Large Latte)`)

  await click(`document.querySelector('md-filled-select')`)
  await sleep(400)
  await click(`[...document.querySelectorAll('md-select-option')].find(o => o.value === 'Oat milk')`)
  assertEq(await ev(picked.select), 'Oat milk', `[${page}] picking an option selects it`)
  assertEq((await line()).includes('oat milk'), true, `[${page}] the picked milk is stored (the summary says oat milk)`)

  await click(`[...document.querySelectorAll('md-radio')].find(r => r.getAttribute('aria-label') === 'Dark')`)
  assertEq(await ev(picked.radio), ['Dark'], `[${page}] clicking a radio makes it the one checked radio`)
  assertEq((await line()).includes('Dark roast'), true, `[${page}] the picked roast is stored (the summary says Dark roast)`)

  await click(`[...document.querySelectorAll('md-outlined-button')].find(b => b.textContent.includes('Presets'))`)
  await sleep(300)
  await click(`[...document.querySelectorAll('md-menu-item, [role=menuitem]')].find(i => i.textContent.includes('The usual'))`)
  assertEq(await ev(picked.tab), ['Cappuccino'], `[${page}] a preset shows its drink: Cappuccino is the one active tab`)
  assertEq(await ev(picked.segment), ['Medium'], `[${page}] a preset shows its size: Medium is the one checked segment`)
  assertEq(await ev(picked.select), 'Whole milk', `[${page}] a preset shows its milk`)
  assertEq(await ev(picked.radio), ['Medium'], `[${page}] a preset shows its roast: Medium is the one checked radio`)
}
