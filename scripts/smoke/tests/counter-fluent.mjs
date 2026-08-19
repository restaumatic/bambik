// A representative Fluent sibling demo: the same Counter module with the
// PUI.Web.Fluent import — the fluent-* elements register, the seeded model
// renders in the type ramp, and the fluent-button drives the mvu loop.
export const demos = ['demo/7guis/counter-fluent']
export const url = '/demo/7guis/counter-fluent/'

// the title3 readout is the page's only size-500 fluent-text
const count = `document.querySelector('fluent-text[size="500"]').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('fluent-button')`), true, 'Fluent elements registered')
  await sleep(300) // FAST binds a beat after insertion
  assertEq(await ev(count), '0', 'seeded count renders in the Fluent type ramp')

  await ev(`document.querySelector('fluent-button').click()`)
  await sleep(50)
  assertEq(await ev(count), '1', 'clicking the Fluent button increments')
}
