// PUI.Bootstrap design-system vocabulary on a live page: native inputs
// dressed in Bootstrap classes drive the tapped repayment readouts through
// the same citizenship/protocols as the MDC modules, and the apply event
// lands in the hand-wired toast.
export const demos = ['demo/nguis/loan-calculator-bootstrap']
export const url = '/demo/nguis/loan-calculator-bootstrap/'

const itemText = i => `document.querySelectorAll('#demo-column .list-group-item')[${i}].textContent.trim()`

export const run = async ({ ev, assertEq, sleep }) => {
  const seededMonthly = await ev(itemText(0))
  assertEq(seededMonthly, 'Monthly payment €239.89', `seeded annuity renders (${seededMonthly})`)
  assertEq(await ev(itemText(1)), 'Interest rate 7.4% p.a.', 'seeded rate renders')

  await ev(`(() => { const r = document.querySelectorAll('input.form-range')[0]; r.value = '24000'; r.dispatchEvent(new Event('input')) })()`)
  await sleep(100)
  assertEq(await ev(itemText(0)), 'Monthly payment €479.77', 'amount slider recomputes the annuity')

  await ev(`(() => { const s = document.querySelector('select.form-select'); s.value = '1'; s.dispatchEvent(new Event('change')) })()`)
  await ev(`(() => { const c = document.querySelector('input.form-check-input'); c.checked = true; c.dispatchEvent(new Event('change')) })()`)
  await sleep(100)
  assertEq(await ev(itemText(1)), 'Interest rate 4.6% p.a.', 'purpose and insurance re-rate the loan')

  const share = await ev(`parseInt(document.querySelector('#demo-column .progress-bar').style.width, 10)`)
  assertEq(share >= 10 && share <= 13, true, `interest-share gauge follows the rate (${share}%)`)

  await ev(`(() => { const t = document.querySelector('input.form-control'); t.value = 'Alex'; t.dispatchEvent(new Event('input')) })()`)
  await ev(`document.querySelector('button.btn-primary').click()`)
  await sleep(200)
  assertEq(await ev(`document.querySelector('.toast').classList.contains('show')`), true, 'applying shows the toast')
  const applied = await ev(`document.querySelector('.toast').textContent.trim()`)
  assertEq(applied.includes('Application received, Alex') && applied.includes('€24000 over 5 years'), true, `toast carries the application (${applied})`)
}
