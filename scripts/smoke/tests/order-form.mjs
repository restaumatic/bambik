// The four-direction pipeline: an Aff load action feeds the form,
// the variant fulfillment editor shows exactly the selected payload's pane,
// an in-form Aff action estimates the delivery distance on a button (and an
// address edit forgets it), and the debounced summary tap mirrors the model.
export const demos = ['demo/nguis/order-form-mdc2']
export const url = '/demo/nguis/order-form-mdc2/'

export const run = async ({ ev, assertEq, sleep }) => {
  const fieldValue = label =>
    ev(`(() => {
      const l = [...document.querySelectorAll('.mdc-floating-label')].find(e => e.textContent === ${JSON.stringify(label)})
      return l ? l.closest('.mdc-text-field').querySelector('input, textarea').value : null
    })()`)

  await sleep(1500) // loadOrder delays 1s before the model arrives
  assertEq(await fieldValue('Short ID'), '7', 'the load action fed the form')
  assertEq(await fieldValue('First name'), 'John', 'nested customer record fed through field @"customer"')
  assertEq(await ev(`document.querySelector('textarea').value`), 'Very spicy, please!', 'the remarks textarea is fed')

  assertEq(await fieldValue('Time'), '8:30', 'the takeaway pane is shown with its payload')
  assertEq(await fieldValue('Table'), null, 'the dineIn pane is detached (provided)')
  assertEq(await fieldValue('Address'), null, 'the delivery pane is detached (provided)')

  await ev(`[...document.querySelectorAll('.mdc-tab')].find(t => t.textContent.includes('Delivery')).click()`)
  await sleep(200)
  assertEq(await fieldValue('Address'), '', 'switching the variant case attaches the delivery pane')
  assertEq(await fieldValue('Time'), null, 'the takeaway pane detached on the case switch')

  const distanceShown = `/Distance [1-6] km/.test(document.body.textContent)`
  assertEq(await ev(distanceShown), false, 'no distance line before an estimate is asked for')
  await ev(`[...document.querySelectorAll('.mdc-button')].find(b => b.textContent.includes('Estimate distance')).click()`)
  await sleep(1500) // estimateDistance delays 700ms before the random 1–6 km arrives
  assertEq(await ev(distanceShown), true, 'the estimate action folded a 1–6 km distance into the model (updated)')
  assertEq(await ev(`/\\([1-6] km away\\)/.test(document.body.textContent)`), true, 'the summary reads the estimated distance')
  await ev(`(() => {
    const l = [...document.querySelectorAll('.mdc-floating-label')].find(e => e.textContent === 'Address')
    const input = l.closest('.mdc-text-field').querySelector('input')
    input.value = 'Baker Street 221B'
    input.dispatchEvent(new Event('input', { bubbles: true }))
  })()`)
  await sleep(600)
  assertEq(await ev(distanceShown), false, 'editing the address forgets the estimate (settled staleDistanceForgotten)')

  assertEq(await ev(`document.body.textContent.includes('Paying by cash')`), true,
    'the payment method projection rendered')
  assertEq(await ev(`document.body.textContent.includes('Summary: Order 7 (uniquely 4617821) for John Doe')`), true,
    'the debounced summary tap mirrors the model')
}
