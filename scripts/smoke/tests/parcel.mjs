// focusRecord end-to-end: the model stays flat ({ recipient, street, city })
// while the reusable address sub-form is a citizen over its own closed row
// { street, city } — focusRecord threads the background field, so edits on
// either side of the focus never lose the other side.
export const demos = ['demo/nguis/parcel-mdc2']
export const url = '/demo/nguis/parcel-mdc2/'

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(300)
  assertEq(await ev(`document.body.textContent.includes('Ada Lovelace · 12 Analytical Row · London')`), true,
    'the seeded flat model renders through the label line')

  const type = (idx, value) => ev(`(() => {
    const input = document.querySelectorAll('.mdc-text-field__input')[${idx}]
    input.value = ${JSON.stringify(value)}
    input.dispatchEvent(new Event('input', { bubbles: true }))
    return true
  })()`)

  await type(1, '5 Difference Lane')
  await sleep(200)
  assertEq(await ev(`document.body.textContent.includes('Ada Lovelace · 5 Difference Lane · London')`), true,
    'a sub-form edit re-merges with the carried background — the recipient survived')

  await type(0, 'Grace Hopper')
  await sleep(200)
  assertEq(await ev(`document.body.textContent.includes('Grace Hopper · 5 Difference Lane · London')`), true,
    'a background edit re-feeds the focus — the address survived')
}
