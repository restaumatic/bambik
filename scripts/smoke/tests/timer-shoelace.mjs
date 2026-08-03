// The Shoelace vocabulary's new components on a live page: sliderLive
// (sl-range, bounded quantity riding the row) re-scopes the duration, and
// progressBar (sl-progress-bar) follows the elapsed fraction.
export const demos = ['demo/7guis/timer-shoelace']
export const url = '/demo/7guis/timer-shoelace/'

const readout = `document.querySelector('#demo-column p').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('sl-range')`), true, 'sl-range registered')

  const seeded = await ev(readout)
  assertEq(seeded.includes('s / 10.0s'), true, `seeded timer renders (${seeded})`)
  assertEq(await ev(`document.querySelector('sl-range').max`), 60, 'the quantity bounds reach the control from the seed')

  await sleep(1300)
  const ticked = await ev(`document.querySelector('sl-progress-bar').value`)
  assertEq(ticked > 0, true, `the tick advances the progress bar (${ticked})`)

  await ev(`(() => { const r = document.querySelector('sl-range'); r.value = 30; r.dispatchEvent(new CustomEvent('sl-input')) })()`)
  await sleep(100)
  assertEq((await ev(readout)).includes('s / 30.0s'), true, 'dragging the sl-range re-scopes the duration')
}
