// forCases end-to-end: flight-booker's booked/rejected outcomes carry bare
// business payloads (the itinerary variant, the problem string) into ONE
// snackbar instance — bookingToast renders both cases through one match
// classifier, where two sibling forCase snackbars used to stand.
export const demos = ['demo/7guis/flight-booker-mdc2']
export const url = '/demo/7guis/flight-booker-mdc2/'

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(300)

  assertEq(await ev(`document.querySelectorAll('.mdc-snackbar').length`), 1,
    'one snackbar instance serves both outcomes')

  const type = (idx, value) => ev(`(() => {
    const input = document.querySelectorAll('.mdc-text-field__input')[${idx}]
    input.value = ${JSON.stringify(value)}
    input.dispatchEvent(new Event('input', { bubbles: true }))
    return true
  })()`)

  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Book')).click()`)
  await sleep(400)
  assertEq(await ev(`document.querySelector('.mdc-snackbar__label').textContent.includes('You have booked: A one-way flight on 27.03.2026')`),
    true, 'the booked case renders through the classifier (bare itinerary in, copy out)')

  await type(0, 'not-a-date')
  await sleep(600) // the button replays the debounced model (300ms settle)
  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Book')).click()`)
  await sleep(400)
  assertEq(await ev(`document.querySelector('.mdc-snackbar__label').textContent.includes('Cannot book: start date "not-a-date" is not a valid DD.MM.YYYY date')`),
    true, 'the rejected case renders through the same classifier (bare problem in, copy out)')
}
