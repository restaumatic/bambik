// PUI.Fluent design-system vocabulary on a live page: the Fluent text
// input, dropdown, radio group, slider and switch drive the tapped plan
// line through the same citizenship/protocols as the MDC modules, the
// rating/progress displays follow the room, and the book event lands in
// the message-bar toast. Fluent's internal update queue is rAF-driven and
// this harness runs frameless, so the leaves' timer-based bind deferral is
// exactly what these assertions exercise.
export const demos = ['demo/nguis/meeting-booker-fluent']
export const url = '/demo/nguis/meeting-booker-fluent/'

const planText = `[...document.querySelectorAll('#demo-column fluent-text')].map(x => x.textContent).find(t => t.startsWith('Plan: '))`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('fluent-dropdown')`), true, 'Fluent elements registered')

  const seeded = await ev(planText)
  assertEq(seeded, 'Plan: Weekly sync in the boardroom, 30 min, 6 attendees, with a Teams link', `seeded plan renders (${seeded})`)
  assertEq(await ev(`document.querySelector('fluent-dropdown').value`), '1', 'dropdown seeded to the boardroom')
  assertEq(await ev(`[...document.querySelectorAll('fluent-radio')].map(r => r.checked).join()`), 'false,true,false', 'radio group seeded to 30 min')

  await ev(`(() => { const rs = [...document.querySelectorAll('fluent-radio')]; rs[2].click() })()`)
  await sleep(150)
  assertEq((await ev(planText)).includes('60 min'), true, 'radio click re-plans the duration')
  assertEq(await ev(`[...document.querySelectorAll('fluent-radio')].map(r => r.checked).join()`), 'false,false,true', 'the echo restores radio exclusivity')

  const seededReadout = await ev(`document.querySelector('fluent-slider').closest('fluent-field').querySelector('fluent-label span').textContent`)
  assertEq(seededReadout, '6', `the slider label carries a live numeric readout (${seededReadout})`)

  await ev(`(() => { const d = document.querySelector('fluent-dropdown'); d.value = '2'; d.dispatchEvent(new Event('change')) })()`)
  await ev(`(() => { const s = document.querySelector('fluent-slider'); s.valueAsNumber = 30; s.dispatchEvent(new Event('change')) })()`)
  await ev(`(() => { const t = document.querySelector('fluent-text-input'); t.value = 'All hands'; t.dispatchEvent(new Event('input', { bubbles: true })) })()`)
  await sleep(150)
  const replanned = await ev(planText)
  assertEq(replanned.includes('All hands in the auditorium') && replanned.includes('30 attendees'), true, `dropdown/slider/text re-plan (${replanned})`)
  assertEq(await ev(`document.querySelector('fluent-slider').closest('fluent-field').querySelector('fluent-label span').textContent`), '30', 'the readout follows the drag through the loop')

  const rating = await ev(`Number(document.querySelector('fluent-rating-display').value)`)
  assertEq(rating, 4, `rating display follows the room (${rating})`)
  const load = await ev(`Number(document.querySelector('fluent-progress-bar').value)`)
  assertEq(load === 0.75, true, `seats-taken gauge fed through projection (${load})`)

  await ev(`document.querySelector('fluent-button').click()`)
  await sleep(200)
  assertEq(await ev(`document.querySelector('fluent-message-bar').classList.contains('fluent-toast--open')`), true, 'booking opens the message bar')
  const booked = await ev(`document.querySelector('fluent-message-bar').textContent.trim()`)
  assertEq(booked, 'Booked: All hands — auditorium for 60 min', `message bar carries the booking (${booked})`)
}
