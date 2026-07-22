// The Sequencing carrier laws (Data.Profunctor.Row.Sequence header), on the
// reorder demo — the keyed-reconciliation showcase. The decisive assertion
// is the retraction law: feeding a reordered array must REUSE the element
// instance per surviving key, so DOM-node identity (and DOM-local state,
// here a bare checkbox tick) follows the track, not the position.
export const demos = ['demo/nguis/reorder']
export const url = '/demo/nguis/reorder/'

// The row shows its track title in the rename field (an MDC filledTextField
// fed through the channel), so the title reads from the field's input value.
export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('ul li').length`), 4, 'build: one element per array entry')
  assertEq(await ev(`document.querySelector('ul li .mdc-text-field__input').value`), 'Track 1', 'build: order preserved')

  // tag the first element's DOM node and give it DOM-local state
  const before = await ev(`(() => {
    const li = document.querySelector('ul li')
    li.__smokeTag = true
    li.querySelector('input[type=checkbox]').checked = true
    return li.querySelector('.mdc-text-field__input').value
  })()`)
  assertEq(before, 'Track 1', 'tagged the Track 1 node')

  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Rotate')).click()`)
  await sleep(400)

  const after = await ev(`(() => {
    const lis = [...document.querySelectorAll('ul li')]
    const idx = lis.findIndex(li => li.__smokeTag === true)
    return {
      total: lis.length,
      idx,
      text: idx >= 0 ? lis[idx].querySelector('.mdc-text-field__input').value : null,
      checked: idx >= 0 ? lis[idx].querySelector('input[type=checkbox]').checked : null,
      firstText: lis[0]?.querySelector('.mdc-text-field__input').value ?? null,
    }
  })()`)
  assertEq(after.total, 4, 'reconcile: same number of elements')
  assertEq(after.idx, 3, 'retraction law: the SAME DOM node moved to the tail with its key')
  assertEq(after.text, 'Track 1', 'retraction law: the node still shows its track')
  assertEq(after.checked, true, 'retraction law: DOM-local state (checkbox tick) followed the track')
  assertEq(after.firstText, 'Track 2', 'reconcile: rotation applied')
}
