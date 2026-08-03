// The plain-HTML vocabulary's new components on a live page: rangeInput
// (native <input type=range>, bounded quantity riding the row) re-scopes the
// duration, and progress (native <progress>) follows the elapsed fraction.
export const demos = ['demo/7guis/timer-html']
export const url = '/demo/7guis/timer-html/'

const readout = `document.querySelectorAll('#demo-column p')[0].textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  const seeded = await ev(readout)
  assertEq(seeded.includes('s / 10.0s'), true, `seeded timer renders (${seeded})`)
  assertEq(await ev(`document.querySelector('input[type=range]').max`), '60.0', 'the quantity bounds reach the native range from the seed')

  await sleep(1300)
  const ticked = await ev(`parseFloat(document.querySelector('progress').getAttribute('value'))`)
  assertEq(ticked > 0, true, `the tick advances the native progress (${ticked})`)

  await ev(`(() => { const r = document.querySelector('input[type=range]'); r.value = '30'; r.dispatchEvent(new Event('input')) })()`)
  await sleep(100)
  assertEq((await ev(readout)).includes('s / 30.0s'), true, 'dragging the native range re-scopes the duration')

  await ev(`[...document.querySelectorAll('#demo-column button')].find(b => b.textContent.includes('Reset')).click()`)
  await sleep(100)
  assertEq((await ev(readout)).startsWith('0.0s'), true, 'Reset zeroes the elapsed time')
}
