// The MDC text-field channel end-to-end: typing into the field emits
// through `asField`, the looped form retains it, and Create round-trips
// through the async action back into the list.
export const demos = ['demo/7guis/crud']
export const url = '/demo/7guis/crud/'

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(800) // async catalogue load
  const before = await ev(`document.querySelectorAll('.mdc-deprecated-list-item').length`)
  assertEq(before > 0, true, `catalogue loaded (${before} people)`)

  await ev(`(() => {
    const type = (idx, value) => {
      const input = document.querySelectorAll('.mdc-text-field__input')[idx]
      input.value = value
      input.dispatchEvent(new Event('input', { bubbles: true }))
    }
    type(1, 'Ada')
    type(2, 'Lovelace')
    return true
  })()`)
  await sleep(200)
  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Create')).click()`)
  await sleep(800)

  const after = await ev(`document.querySelectorAll('.mdc-deprecated-list-item').length`)
  assertEq(after, before + 1, 'Create adds a person')
  assertEq(await ev(`[...document.querySelectorAll('.mdc-deprecated-list-item')].some(li => li.textContent.includes('Lovelace, Ada'))`),
    true, 'the typed name flowed field → model → list')
}
