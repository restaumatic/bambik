// Selectors as a group over option leaves (MDC3, MDC2): each option is a
// leaf of one case of the field, merged `VariantToVariant.do`. Walks
// espresso-bar's four selectors with real clicks, checking that a pick is
// stored (the summary line follows) and exclusive (exactly one option shows
// picked), and that a model change made elsewhere (the "The usual" preset)
// is shown by every group.
import { byName } from '../../demos.mjs'

export const demos = ['espresso-bar-mdc3', 'checkout-mdc3', 'espresso-bar-mdc2', 'checkout-mdc2']
export const pages = [
  { url: `/${byName('espresso-bar-mdc3').page}`, label: 'mdc3' },
  { url: `/${byName('checkout-mdc3').page}`, label: 'stepper' },
  { url: `/${byName('espresso-bar-mdc2').page}`, label: 'mdc2' },
  { url: `/${byName('checkout-mdc2').page}`, label: 'stepper-mdc2' },
]

// each face's selectors: what shows picked, and the element to click for a
// case of each group
const faces = {
  mdc3: {
    summary: `document.querySelector('#demo-column .md-typescale-body-medium, #demo-column [class*="body-medium"]')?.textContent ?? ''`,
    picked: {
      tab: `[...document.querySelectorAll('md-primary-tab')].filter(t => t.active).map(t => t.textContent.trim())`,
      segment: `[...document.querySelectorAll('.md3-segmented-button__segment')].filter(s => s.getAttribute('aria-checked') === 'true').map(s => s.textContent.trim())`,
      select: `document.querySelector('md-filled-select').value`,
      radio: `[...document.querySelectorAll('md-radio')].filter(r => r.checked).map(r => r.getAttribute('aria-label'))`,
    },
    tab: (c) => `[...document.querySelectorAll('md-primary-tab')].find(t => t.textContent.trim() === ${JSON.stringify(c)})`,
    segment: (c) => `[...document.querySelectorAll('.md3-segmented-button__segment')].find(s => s.textContent.trim() === ${JSON.stringify(c)})`,
    selectAnchor: `document.querySelector('md-filled-select')`,
    option: (c) => `[...document.querySelectorAll('md-select-option')].find(o => o.value === ${JSON.stringify(c)})`,
    radio: (c) => `[...document.querySelectorAll('md-radio')].find(r => r.getAttribute('aria-label') === ${JSON.stringify(c)})`,
    presets: `[...document.querySelectorAll('md-outlined-button')].find(b => b.textContent.includes('Presets'))`,
    toggle: `document.querySelector('md-switch')`,
  },
  mdc2: {
    summary: `document.querySelector('#demo-column .mdc-typography--body2')?.textContent ?? ''`,
    picked: {
      tab: `[...document.querySelectorAll('.mdc-tab.mdc-tab--active')].map(t => t.getAttribute('aria-label'))`,
      segment: `[...document.querySelectorAll('.mdc-segmented-button__segment')].filter(s => s.getAttribute('aria-checked') === 'true').map(s => s.getAttribute('aria-label'))`,
      select: `document.querySelector('.mdc-select .mdc-select__selected-text').textContent`,
      radio: `[...document.querySelectorAll('input.mdc-radio__native-control')].filter(r => r.checked).map(r => r.closest('.mdc-form-field').querySelector('label').textContent)`,
    },
    tab: (c) => `document.querySelector('.mdc-tab[aria-label=${JSON.stringify(c)}]')`,
    segment: (c) => `document.querySelector('.mdc-segmented-button__segment[aria-label=${JSON.stringify(c)}]')`,
    selectAnchor: `document.querySelector('.mdc-select__anchor')`,
    option: (c) => `document.querySelector('.mdc-select li[data-value=${JSON.stringify(c)}]')`,
    radio: (c) => `[...document.querySelectorAll('.mdc-form-field label')].find(l => l.textContent === ${JSON.stringify(c)})`,
    presets: `[...document.querySelectorAll('.mdc-menu-surface--anchor > button')].find(b => b.textContent.includes('Presets'))`,
    toggle: `document.querySelector('.mdc-switch')`,
  },
}

export const run = async (ctx) => (ctx.page.startsWith('stepper') ? stepperWalk : espressoWalk)(ctx)

// checkout's packaged stepper: Back and Next pick the neighbouring step leaf
const stepperWalk = async ({ ev, session, assertEq, sleep, page }) => {
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)
  await sleep(500)
  const prefix = page === 'stepper' ? '.md3-stepper' : '.mdc2-stepper'
  const buttons = page === 'stepper' ? `${prefix} md-text-button` : `${prefix} button.mdc-button`
  const caption = () => ev(`document.querySelector('${prefix}__caption').textContent`)
  const button = (label) => `[...document.querySelectorAll('${buttons}')].find(b => b.textContent.includes(${JSON.stringify(label)}))`
  const press = async (label) => {
    const [x, y] = await ev(`(() => { const el = ${button(label)}; el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    for (const type of ['mouseMoved', 'mousePressed', 'mouseReleased']) await session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
    await sleep(400)
  }
  assertEq(await caption(), 'Step 1 of 3 · Cart', `[${page}] the seeded step is named`)
  await press('Next')
  assertEq(await caption(), 'Step 2 of 3 · Shipping', `[${page}] Next moves to the second step`)
  await press('Next')
  assertEq(await caption(), 'Step 3 of 3 · Payment', `[${page}] Next moves to the last step`)
  assertEq(await ev(`${button('Next')}.hasAttribute('disabled')`), true, `[${page}] Next is disabled on the last step`)
  await press('Back')
  assertEq(await caption(), 'Step 2 of 3 · Shipping', `[${page}] Back moves to the previous step`)
}

const espressoWalk = async ({ ev, session, assertEq, sleep, page }) => {
  const face = faces[page]
  const picked = face.picked
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
  const line = () => ev(face.summary)

  assertEq(await ev(picked.tab), ['Cappuccino'], `[${page}] the seeded drink's tab is the active one`)
  assertEq(await ev(picked.segment), ['Medium'], `[${page}] the seeded size's segment is the checked one`)
  assertEq(await ev(picked.select), 'Whole milk', `[${page}] the seeded milk is selected`)
  assertEq(await ev(picked.radio), ['Medium'], `[${page}] the seeded roast's radio is the checked one`)

  await click(face.tab('Latte'))
  assertEq(await ev(picked.tab), ['Latte'], `[${page}] clicking a tab makes it the one active tab`)
  assertEq((await line()).includes('Latte'), true, `[${page}] the picked drink is stored (the summary says Latte)`)

  await click(face.segment('Large'))
  assertEq(await ev(picked.segment), ['Large'], `[${page}] clicking a segment makes it the one checked segment`)
  assertEq((await line()).includes('Large Latte'), true, `[${page}] the picked size is stored (the summary says Large Latte)`)

  await click(face.selectAnchor)
  await sleep(400)
  await click(face.option('Oat milk'))
  assertEq(await ev(picked.select), 'Oat milk', `[${page}] picking an option selects it`)
  assertEq((await line()).includes('oat milk'), true, `[${page}] the picked milk is stored (the summary says oat milk)`)

  await click(face.radio('Dark'))
  assertEq(await ev(picked.radio), ['Dark'], `[${page}] clicking a radio makes it the one checked radio`)
  assertEq((await line()).includes('Dark roast'), true, `[${page}] the picked roast is stored (the summary says Dark roast)`)

  // a pick survives every other widget's edit: each re-feeds the row
  await click(face.segment('Small'))
  assertEq(await ev(picked.radio), ['Dark'], `[${page}] picking a size leaves the roast's radio checked`)
  await click(face.tab('Espresso'))
  assertEq(await ev(picked.radio), ['Dark'], `[${page}] picking a drink leaves the roast's radio checked`)
  assertEq(await ev(picked.segment), ['Small'], `[${page}] picking a drink leaves the size's segment checked`)
  await click(face.toggle)
  assertEq(await ev(picked.radio), ['Dark'], `[${page}] flipping a switch leaves the roast's radio checked`)
  assertEq(await ev(picked.segment), ['Small'], `[${page}] flipping a switch leaves the size's segment checked`)
  assertEq(await ev(picked.tab), ['Espresso'], `[${page}] flipping a switch leaves the drink's tab active`)
  assertEq((await line()).includes('Dark roast'), true, `[${page}] the roast is still stored after other edits`)

  await click(face.presets)
  await sleep(300)
  await click(`[...document.querySelectorAll('md-menu-item, [role=menuitem]')].find(i => i.textContent.includes('The usual'))`)
  assertEq(await ev(picked.tab), ['Cappuccino'], `[${page}] a preset shows its drink: Cappuccino is the one active tab`)
  assertEq(await ev(picked.segment), ['Medium'], `[${page}] a preset shows its size: Medium is the one checked segment`)
  assertEq(await ev(picked.select), 'Whole milk', `[${page}] a preset shows its milk`)
  assertEq(await ev(picked.radio), ['Medium'], `[${page}] a preset shows its roast: Medium is the one checked radio`)
}
