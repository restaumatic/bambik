// A representative Shoelace sibling demo: the same Counter module with the
// PUI.Web.Shoelace import — the sl-* elements register, the seeded model
// renders, and the sl-button drives the mvu loop.
export const demos = ['demo/7guis/counter-shoelace']
export const url = '/demo/7guis/counter-shoelace/'

const count = `document.querySelector('#demo-column h4').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('sl-button')`), true, 'Shoelace elements registered')
  assertEq(await ev(count), '0', 'seeded count renders inside the sl-card')

  await ev(`document.querySelector('sl-button').click()`)
  await sleep(50)
  assertEq(await ev(count), '1', 'clicking the Shoelace button increments')
}
