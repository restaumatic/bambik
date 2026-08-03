// The plain-HTML floor as a sibling demo: the same Counter module over
// PUI.Web.HTML alone — the bare <button> replays the model on click, the
// mvu re-feed re-enables it, and the generalized design-system switcher
// lists the sibling vocabularies of a non-MDC suffix.
export const demos = ['demo/7guis/counter-html']
export const url = '/demo/7guis/counter-html/'

const count = `document.querySelector('#demo-column h4').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(count), '0', 'seeded count renders')

  await ev(`document.querySelector('#demo-column button').click()`)
  await sleep(50)
  assertEq(await ev(count), '1', 'clicking the bare button increments')

  // the click disabled the button; the loop's re-feed re-enables it
  await ev(`document.querySelector('#demo-column button').click()`)
  await sleep(50)
  assertEq(await ev(count), '2', 'the mvu re-feed re-arms the button for a second click')

  await sleep(400) // the switcher appears after page.js probes the siblings
  assertEq(await ev(`(document.querySelector('#page-header a[href$="/counter-mdc2/"]') || {}).textContent`), 'MDC2',
    'the header switcher links an -html page back to its MDC2 sibling')
}
