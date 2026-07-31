// A representative MD3 sibling demo: the same Counter module with the
// PUI.MDC3 import — the custom elements register, the seeded model renders,
// and the md-filled-button drives the mvu loop.
export const demos = ['demo/7guis/counter-md3']
export const url = '/demo/7guis/counter-md3/'

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('md-filled-button')`), true, '@material/web elements registered')
  assertEq(await ev(`document.querySelector('h2.md-typescale-headline-large').textContent`), '0', 'seeded count renders in the MD3 typescale')

  await ev(`document.querySelector('md-filled-button').click()`)
  await sleep(50)
  assertEq(await ev(`document.querySelector('h2.md-typescale-headline-large').textContent`), '1', 'clicking the MD3 button increments')

  await sleep(300) // the switcher appears after page.js probes the sibling
  assertEq(await ev(`(document.querySelector('#page-header a[href$="/counter/"]') || {}).textContent`), 'MD2',
    'the header switcher links back to the MD2 sibling')
}
