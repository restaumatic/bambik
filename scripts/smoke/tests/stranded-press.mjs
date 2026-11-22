// A touch press whose click never reaches its control (MDC3's `body`), on
// the real DOM (diner): `md-ripple` would hold the press for good — the
// tab drawn pressed, deaf to every later touch — and the page releases it
// half a second after the finger lifts; the tab answers the next tap.
import { byName } from '../../demos.mjs'

export const demos = ['diner']
export const pages = [{ url: `/${byName('diner').page}`, label: 'diner' }]

const tabNamed = label => `[...document.querySelectorAll('md-primary-tab')].find(t => t.innerText.trim() === ${JSON.stringify(label)})`
const pressed = label => `(() => { const r = ${tabNamed(label)}.shadowRoot.querySelector('md-ripple'); return r.pressed || r.rippleStartEvent !== undefined })()`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await session.send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 1 })
  for (let i = 0; i < 60 && !(await ev(`!!document.querySelector('.md3-tile')`)); i++) await sleep(100)
  await sleep(300)
  const tap = async label => {
    const [x, y] = await ev(`(() => { const r = ${tabNamed(label)}.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    await session.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x, y }] })
    await sleep(40)
    await session.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] })
  }

  // the click of the next tap is lost on its way, as one that lands on another control is lost to this one
  await ev(`window.addEventListener('click', e => e.stopImmediatePropagation(), { capture: true, once: true })`)
  await tap('Kuchnia')
  await sleep(200)
  assertEq(await ev(pressed('Kuchnia')), true, `[${page}] a press whose click is lost is held`)
  await sleep(900)
  assertEq(await ev(pressed('Kuchnia')), false, `[${page}] and released half a second after the finger lifts`)

  await tap('Kuchnia')
  await sleep(600)
  assertEq(await ev(`${tabNamed('Kuchnia')}.active`), true, `[${page}] the tab answers the next tap`)
  assertEq(await ev(pressed('Kuchnia')), false, `[${page}] and is left unpressed`)
}
