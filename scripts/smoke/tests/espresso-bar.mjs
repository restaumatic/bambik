// PUI.Web.MDC3 (Material Design 3 via @material/web): the espresso-bar demo's
// editors drive the model through host-level events on the custom elements,
// the tapped summary and gauge re-render, presets fold via updates, and the
// order button lands in the hand-rolled snackbar.
export const demos = ['demo/nguis/espresso-bar-mdc3']
export const url = '/demo/nguis/espresso-bar-mdc3/'

const summary = `(document.querySelector('p.md-typescale-body-medium') || { textContent: '' }).textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('md-filled-button')`), true, '@material/web elements registered')

  assertEq(
    (await ev(summary)).includes('Medium cappuccino with whole milk, medium roast, 1 sugar — €3.50'),
    true,
    'seed order summarized (' + await ev(summary) + ')'
  )

  await ev(`(() => { const tf = document.querySelector('md-filled-text-field'); tf.value = 'Ada'; tf.dispatchEvent(new Event('input', { bubbles: true })); return true })()`)

  assertEq(
    await ev(`(() => { const seg = [...document.querySelectorAll('.md3-segmented-button__segment')].find(s => s.textContent.includes('Large')); if (!seg) return false; seg.click(); return true })()`),
    true,
    'segmented button: pick Large'
  )
  await sleep(50)
  assertEq((await ev(summary)).includes('Large cappuccino'), true, 'segmented emission reached the summary')
  assertEq((await ev(summary)).includes('€4.00'), true, 'size raised the price')

  await ev(`(() => { const chip = document.querySelector('md-filter-chip'); chip.selected = true; chip.click(); return true })()`)
  await sleep(50)
  assertEq((await ev(summary)).includes('extra shot'), true, 'filter chip emission reached the summary')

  await ev(`(() => { const sel = document.querySelector('md-filled-select'); sel.selectedIndex = 1; sel.dispatchEvent(new Event('change')); return true })()`)
  await sleep(50)
  assertEq((await ev(summary)).includes('with oat milk'), true, 'select emission reached the summary')

  await ev(`(() => { const tabs = document.querySelector('md-tabs'); tabs.activeTabIndex = 0; tabs.dispatchEvent(new Event('change')); return true })()`)
  await sleep(50)
  assertEq((await ev(summary)).includes('Large espresso'), true, 'tab bar emission reached the summary')

  await ev(`(() => { const r = document.querySelectorAll('md-radio')[2]; r.checked = true; r.dispatchEvent(new Event('change')); return true })()`)
  await sleep(50)
  assertEq((await ev(summary)).includes('dark roast'), true, 'radio emission reached the summary')

  await ev(`(() => { const sw = document.querySelector('md-switch'); sw.selected = true; sw.dispatchEvent(new Event('change')); return true })()`)
  await sleep(50)
  assertEq((await ev(summary)).includes('to go'), true, 'switch emission reached the summary')

  await ev(`(() => { const sl = document.querySelector('md-slider'); sl.value = 3; sl.dispatchEvent(new Event('change')); return true })()`)
  await sleep(50)
  assertEq((await ev(summary)).includes('3 sugars'), true, 'slider emission reached the summary')

  await ev(`(() => { const cb = document.querySelector('md-checkbox'); cb.checked = true; cb.dispatchEvent(new Event('change')); return true })()`)
  await sleep(50)
  assertEq((await ev(summary)).includes('€4.41'), true, 'loyalty discount applied (' + await ev(summary) + ')')

  const gauge = await ev(`document.querySelector('md-linear-progress').value`)
  assertEq(gauge > 0.9, true, 'caffeine gauge fed through projection (' + gauge + ')')

  assertEq(
    await ev(`[...document.querySelectorAll('span')].some(s => s.textContent === 'Sugar')`),
    true,
    'slider renders its visible label'
  )
  assertEq(
    await ev(`[...document.querySelectorAll('span')].some(s => s.textContent === 'Caffeine')`),
    true,
    'caffeine gauge carries its label chrome'
  )

  await ev(`(() => { const ib = document.querySelector('md-icon-button'); ib.selected = true; ib.dispatchEvent(new Event('change')); return true })()`)

  assertEq(
    await ev(`(() => { const mi = [...document.querySelectorAll('md-menu-item')].find(m => m.textContent.includes('no frills')); if (!mi) return false; mi.click(); return true })()`),
    true,
    'preset menu item clicked'
  )
  await sleep(50)
  assertEq(
    (await ev(summary)).includes('Small espresso, dark roast, to go — €2.70'),
    true,
    'preset folded via updates (' + await ev(summary) + ')'
  )

  await ev(`(() => { document.querySelector('md-filled-button').click(); return true })()`)
  await sleep(100)
  assertEq(
    await ev(`document.querySelector('.md3-snackbar').classList.contains('md3-snackbar--open')`),
    true,
    'snackbar opened on order'
  )
  const toast = await ev(`document.querySelector('.md3-snackbar').textContent`)
  assertEq(
    toast.includes('Coming right up, Ada: Small espresso, dark roast, to go — €2.70') && toast.includes('★'),
    true,
    'order event dispatched into the snackbar (' + toast + ')'
  )
}
