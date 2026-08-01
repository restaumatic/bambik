// PUI.Fluent design-system vocabulary on a live page, and the no-defaults
// discipline: nothing is pre-picked — the unmade choices flow as Nothing
// (`optional` selectors), the attendees slider exists only once a room
// gives it bounds, and the plan/booking stage exists only once the pick is
// complete. Fluent's internal update queue is rAF-driven and this harness
// runs frameless, so the leaves' timer-based bind deferral is exercised
// throughout.
export const demos = ['demo/nguis/meeting-booker-fluent']
export const url = '/demo/nguis/meeting-booker-fluent/'

const planText = `[...document.querySelectorAll('#demo-column fluent-text')].map(x => x.textContent).find(t => t.startsWith('Plan: '))`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('fluent-dropdown')`), true, 'Fluent elements registered')

  assertEq(await ev(`document.querySelector('fluent-dropdown').value`), null, 'no room pre-picked')
  assertEq(await ev(`[...document.querySelectorAll('fluent-radio')].map(r => r.checked).join()`), 'false,false,false', 'no duration pre-picked')
  assertEq(await ev(`!!document.querySelector('fluent-slider')`), false, 'no attendees slider before a room gives it bounds')
  assertEq(await ev(`!!document.querySelector('fluent-rating-display')`), false, 'no room gauges before a room is picked')
  assertEq(await ev(`!!document.querySelector('fluent-button')`), false, 'no booking stage before the plan is complete')

  await ev(`(() => { const d = document.querySelector('fluent-dropdown'); d.value = '1'; d.dispatchEvent(new Event('change')) })()`)
  await sleep(200)
  assertEq(await ev(`parseFloat(document.querySelector('fluent-slider').getAttribute('max'))`), 12, 'picking a room materializes the attendees slider with its capacity')
  assertEq(await ev(`Number(document.querySelector('fluent-rating-display').value)`), 3.5, 'the room gauges follow the pick')
  assertEq(await ev(`!!document.querySelector('fluent-button')`), false, 'still no booking without a duration')

  await ev(`(() => { const s = document.querySelector('fluent-slider'); s.valueAsNumber = 8; s.dispatchEvent(new Event('change')) })()`)
  await sleep(100)
  const readout = await ev(`document.querySelector('fluent-slider').closest('fluent-field').querySelector('fluent-label span').textContent`)
  assertEq(readout, '8', `the slider readout follows the drag (${readout})`)

  await ev(`(() => { [...document.querySelectorAll('fluent-radio')][2].click() })()`)
  await ev(`(() => { const t = document.querySelector('fluent-text-input'); t.value = 'All hands'; t.dispatchEvent(new Event('input', { bubbles: true })) })()`)
  await ev(`(() => { const s = document.querySelector('fluent-switch'); s.checked = true; s.dispatchEvent(new Event('change')) })()`)
  await sleep(200)
  const plan = await ev(planText)
  assertEq(plan, 'Plan: All hands in the boardroom, 60 min, 8 attendees, with a Teams link', `the complete pick materializes the plan (${plan})`)

  await ev(`(() => { const d = document.querySelector('fluent-dropdown'); d.value = '0'; d.dispatchEvent(new Event('change')) })()`)
  await sleep(200)
  const capped = await ev(planText)
  assertEq(capped.includes('4 attendees'), true, `re-picking a smaller room clamps the headcount (${capped})`)
  assertEq(await ev(`parseFloat(document.querySelector('fluent-slider').getAttribute('max'))`), 4, 'the slider max re-scoped in place from the model')

  await ev(`document.querySelector('fluent-button').click()`)
  await sleep(200)
  assertEq(await ev(`document.querySelector('fluent-message-bar').classList.contains('fluent-toast--open')`), true, 'booking opens the message bar')
  const booked = await ev(`document.querySelector('fluent-message-bar').textContent.trim()`)
  assertEq(booked, 'Booked: All hands — focus pod for 60 min', `message bar carries the booking (${booked})`)
}
