// A full mvu loop over MDC components: the list renders from the model,
// an iconToggle emission folds back through `updates`, and both the
// selected styling and the tapped favorites line reflect the new model.
export const demos = ['demo/nguis/movie-browser']
export const url = '/demo/nguis/movie-browser/'

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('.mdc-deprecated-list-item').length`), 12, 'all movies render')
  assertEq(await ev(`document.querySelector('.mdc-elevation--z1').textContent`), '0 favorites', 'favorites line starts at 0')

  await ev(`document.querySelector('.mdc-icon-button').click()`)
  await sleep(400)
  assertEq(await ev(`document.querySelectorAll('.mdc-deprecated-list-item--selected').length`), 1, 'favorite toggle selects its row')
  assertEq(await ev(`document.querySelector('.mdc-elevation--z1').textContent`), '1 favorite', 'favorites line follows the model')
}
