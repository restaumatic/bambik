// PUI.Web.Shoelace design-system vocabulary on a live page: the star rating,
// text fields, select and switch drive the tapped preview line through the
// same citizenship/protocols as the MDC modules, and the submit event lands
// in the sl-alert toast.
export const demos = ['demo/nguis/product-review-shoelace']
export const url = '/demo/nguis/product-review-shoelace/'

const previewText = `document.querySelector('#demo-column p').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('sl-rating')`), true, 'Shoelace elements registered')

  const seeded = await ev(previewText)
  assertEq(seeded.includes('☆☆☆☆☆') && seeded.includes('owned less than a month'), true, `seeded preview renders (${seeded})`)

  await ev(`(() => { const r = document.querySelector('sl-rating'); r.value = 4; r.dispatchEvent(new CustomEvent('sl-change')) })()`)
  await sleep(100)
  assertEq((await ev(previewText)).includes('★★★★☆'), true, 'rating drives the preview stars')

  await ev(`(() => { const i = document.querySelectorAll('sl-input')[0]; i.value = 'Great crema'; i.dispatchEvent(new CustomEvent('sl-input')) })()`)
  await ev(`(() => { const s = document.querySelector('sl-switch'); s.checked = true; s.dispatchEvent(new CustomEvent('sl-change')) })()`)
  await ev(`(() => { const s = document.querySelector('sl-select'); s.value = '2'; s.dispatchEvent(new CustomEvent('sl-change')) })()`)
  await ev(`(() => { const i = document.querySelectorAll('sl-input')[1]; i.value = 'Kim'; i.dispatchEvent(new CustomEvent('sl-input')) })()`)
  await sleep(100)
  const filled = await ev(previewText)
  assertEq(filled.includes('“Great crema”'), true, `headline quoted in the preview (${filled})`)
  assertEq(filled.includes('would recommend'), true, 'switch adds the recommendation')
  assertEq(filled.includes('owned more than a year'), true, 'select re-scopes the ownership')

  await ev(`document.querySelector('sl-button').click()`)
  await sleep(300)
  assertEq(await ev(`document.querySelector('sl-alert').open`), true, 'submit opens the toast')
  const toastLine = await ev(`document.querySelector('sl-alert').textContent.trim()`)
  assertEq(toastLine.includes('Thanks, Kim!') && toastLine.includes('★★★★☆'), true, `toast carries the review (${toastLine})`)
}
