// A pane's content is silent while parked: the ticker's clock and its quote
// action sit inside the `watching` pane, and stepping away suspends the
// pane's scope — the clock skips its beats, and a quote that settles
// meanwhile is dropped — while coming back resumes both.
import { byName } from '../../demos.mjs'
export const demos = ['ticker-mdc2']
export const pages = [{ url: `/${byName('ticker-mdc2').page}`, label: 'ticker' }]

const press = (name) => `(() => {
  const b = [...document.querySelectorAll('button')].find(b => b.getAttribute('aria-label') === ${JSON.stringify(name)})
  if (!b) return false
  b.click()
  return true
})()`
const shown = (re) => `(() => { const m = document.body.innerText.match(${re}); return m ? Number(m[1]) : null })()`
const ticksShown = shown('/(\\d+) ticks? while watching/')
const quotesShown = shown('/(\\d+) quotes? received/')

export const run = async ({ ev, assertEq, sleep, page }) => {
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)

  await sleep(800)
  assertEq(await ev(ticksShown), 0, `[${page}] the clock in a pane never attached has not ticked`)

  assertEq(await ev(press('Watch')), true, `[${page}] Watch is on screen while away`)
  await sleep(1000)
  const watching = await ev(ticksShown)
  assertEq(watching >= 2, true, `[${page}] the clock ticks while the pane is attached (${watching})`)

  assertEq(await ev(press('Step away')), true, `[${page}] Step away is on screen while watching`)
  await sleep(300)
  const parked = await ev(ticksShown)
  await sleep(1000)
  assertEq(await ev(ticksShown), parked, `[${page}] a parked pane's clock skips its beats`)

  await ev(press('Watch'))
  await sleep(1000)
  const resumed = await ev(ticksShown)
  assertEq(resumed > parked, true, `[${page}] the clock ticks again once the pane is back (${parked} → ${resumed})`)

  assertEq(await ev(quotesShown), 0, `[${page}] no quote yet`)
  assertEq(await ev(press('Fetch quote')), true, `[${page}] Fetch quote is on screen while watching`)
  await sleep(100)
  await ev(press('Step away'))
  await sleep(1400)
  assertEq(await ev(quotesShown), 0, `[${page}] a quote that settles in a parked pane is dropped`)

  await ev(press('Watch'))
  await sleep(200)
  await ev(press('Fetch quote'))
  await sleep(1400)
  assertEq(await ev(quotesShown), 1, `[${page}] a quote that settles in an attached pane is recorded`)
}
