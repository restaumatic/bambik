// The plain-HTML vocabulary's new select and output on a live page: the
// native <select> is the type-changing flight-type selector, provided panes
// come and go with the selection, and both booked/rejected outcomes land in
// the one native <output> through the forCases classifier.
export const demos = ['demo/7guis/flight-booker-html']
export const url = '/demo/7guis/flight-booker-html/'

const outputText = `document.querySelector('#demo-column output').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(300)

  assertEq(await ev(`document.querySelectorAll('#demo-column select').length`), 1, 'the native select renders')
  assertEq(await ev(`document.querySelectorAll('#demo-column input').length`), 1, 'the return-date pane is absent on a one-way flight')

  await ev(`[...document.querySelectorAll('#demo-column button')].find(b => b.textContent.includes('Book')).click()`)
  await sleep(400)
  assertEq((await ev(outputText)).includes('You have booked: A one-way flight on 27.03.2026'),
    true, 'the booked case renders through the classifier into the native output')

  await ev(`(() => { const s = document.querySelector('#demo-column select'); s.value = '1'; s.dispatchEvent(new Event('change')) })()`)
  await sleep(600) // the return pane attaches, and the debounced model settles
  assertEq(await ev(`document.querySelectorAll('#demo-column input').length`), 2, 'picking a return flight attaches the return-date pane')

  await ev(`(() => {
    const input = document.querySelectorAll('#demo-column input')[0]
    input.value = 'not-a-date'
    input.dispatchEvent(new Event('input'))
  })()`)
  await sleep(600) // the button replays the debounced model (300ms settle)
  await ev(`[...document.querySelectorAll('#demo-column button')].find(b => b.textContent.includes('Book')).click()`)
  await sleep(400)
  assertEq((await ev(outputText)).includes('Cannot book: start date "not-a-date" is not a valid DD.MM.YYYY date'),
    true, 'the rejected case renders through the same classifier')
}
