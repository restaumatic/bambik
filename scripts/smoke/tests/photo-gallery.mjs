// Slot parts (MDC `Slot`): the top app bar's title is `fed _.album`, so it
// renders on the first feed and follows the model in place; the drawer's own
// title is pinned at construction and never moves.
export const demos = ['demo/nguis/photo-gallery']
export const url = '/demo/nguis/photo-gallery/'

const title = `document.querySelector('.mdc-top-app-bar__title').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(400)
  assertEq(await ev(title), 'Landscapes', 'a fed slot renders on the first feed (the seeded album names the bar)')
  assertEq(await ev(`document.querySelector('.mdc-drawer__title').textContent`), 'Darkroom', 'a pinned slot renders its constant')

  await ev(`(() => { window.__bar = document.querySelector('.mdc-top-app-bar__title'); return true })()`)
  await ev(`[...document.querySelectorAll('.mdc-drawer .mdc-deprecated-list-item')].find(li => li.textContent.includes('Abstract')).click()`)
  await sleep(300)

  assertEq(await ev(title), 'Abstract', 'the fed slot follows the model')
  assertEq(
    await ev(`window.__bar === document.querySelector('.mdc-top-app-bar__title')`),
    true,
    'the slot updates its text node in place — the chrome is built once'
  )
  assertEq(await ev(`document.querySelector('.mdc-drawer__title').textContent`), 'Darkroom', 'the pinned slot is unaffected')
}
