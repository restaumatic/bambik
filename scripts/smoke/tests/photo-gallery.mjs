// The gallery is a keyed `foreach` of channel-fed `imagePane`s: switching
// albums re-feeds the panes in place rather than rebuilding them, so the
// `<li>` nodes survive and only their src/caption change.
export const demos = ['demo/nguis/photo-gallery-mdc2']
export const url = '/demo/nguis/photo-gallery-mdc2/'

const gallery = `document.querySelectorAll('.mdc-image-list')[1]`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`${gallery}.querySelectorAll('.mdc-image-list__item').length`), 10, 'the seeded album renders its photos')
  assertEq(await ev(`document.querySelector('.mdc-typography--headline2').textContent`), 'Landscapes', 'the headline shows the seeded album')

  const captions = `Array.from(${gallery}.querySelectorAll('.mdc-image-list__label')).map(n => n.textContent).join('|')`
  const first = await ev(captions)
  assertEq(first.startsWith('Dawn Ridge|Quiet Lake|Amber Dunes'), true, 'each pane renders its caption through the channel')
  assertEq(await ev(`${gallery}.querySelector('.mdc-image-list__image').getAttribute('src').startsWith('data:image/svg+xml')`), true, 'attrWith fed the pane its src')

  await ev(`${gallery}.querySelectorAll('.mdc-image-list__item')[0].setAttribute('data-probe', 'kept')`)
  await ev(`Array.from(document.querySelectorAll('.mdc-deprecated-list-item')).find(n => n.textContent.includes('Portraits')).click()`)
  await sleep(400)

  assertEq(await ev(`document.querySelector('.mdc-typography--headline2').textContent`), 'Portraits', 'picking an album re-feeds the headline')
  assertEq(await ev(`${gallery}.querySelectorAll('.mdc-image-list__item')[0].getAttribute('data-probe')`), 'kept', 'keyed reconciliation re-fed the pane in place instead of rebuilding it')
  assertEq(await ev(captions) !== first, true, 'the captions followed the new album')
}
