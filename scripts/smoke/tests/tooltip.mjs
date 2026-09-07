// A tooltip is transient: it shows while the pointer rests on its anchor (or
// the anchor has keyboard focus) and goes away when the pointer leaves — also
// after a click, which leaves focus on the control. MD3's hand-rolled tooltip
// once stayed up after a click because it showed on `:focus-within`; the
// MDC2 twin runs the same walk over the MDCTooltip foundation.
import { a11y } from '../a11y.mjs'

const twins = ['mdc2', 'mdc3']
export const demos = twins.map((t) => `demo/nguis/espresso-bar-${t}`)
export const pages = twins.map((t) => ({ url: `/demo/nguis/espresso-bar-${t}/`, label: t }))

const tipVisible = `(() => {
  const tip = document.querySelector('.md3-tooltip, .mdc-tooltip')
  if (!tip) return 'missing'
  const cs = getComputedStyle(tip.querySelector('.mdc-tooltip__surface') || tip)
  const own = getComputedStyle(tip)
  return own.display !== 'none' && own.visibility !== 'hidden' && cs.visibility !== 'hidden' && parseFloat(cs.opacity) > 0.5
})()`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  const ax = a11y(session)
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)

  const named = await ax.query({ role: 'checkbox', name: 'Loyalty member' })
  assertEq(named.length, 1, `[${page}] the loyalty checkbox is in the AX tree under its rendered caption`)

  // the anchor is whatever the tooltip describes — both vocabularies stamp
  // aria-describedby on it — hovered at its box center like a hand would
  const [x, y] = await ev(`(() => {
    const el = document.querySelector('#demo-column [aria-describedby]')
    el.scrollIntoView({ block: 'center' })
    const r = el.getBoundingClientRect()
    return [r.left + r.width / 2, r.top + r.height / 2]
  })()`)
  const mouse = (type, px, py, extra = {}) => session.send('Input.dispatchMouseEvent', { type, x: px, y: py, ...extra })

  assertEq(await ev(tipVisible), false, `[${page}] the tooltip starts hidden`)

  await mouse('mouseMoved', x, y)
  await sleep(900)
  assertEq(await ev(tipVisible), true, `[${page}] hovering the anchor shows the tooltip`)

  await mouse('mouseMoved', 5, 5)
  await sleep(1200)
  assertEq(await ev(tipVisible), false, `[${page}] leaving the anchor hides it`)

  await mouse('mouseMoved', x, y)
  await mouse('mousePressed', x, y, { button: 'left', clickCount: 1 })
  await mouse('mouseReleased', x, y, { button: 'left', clickCount: 1 })
  await sleep(300)
  await mouse('mouseMoved', 5, 5)
  await sleep(1200)
  assertEq(await ev(tipVisible), false, `[${page}] after a click (focus left on the control) leaving the anchor still hides it`)
}
