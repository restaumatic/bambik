// The seeded `unfolding` on a live page: the unfold state's initial value
// is `unfolding`'s argument, fed once as the `resume` case at registration
// — so the gated `retain` inside is primed before the first press (before
// the seed argument this needed a `seeded firstTicket` stage in front of
// it). Each press then emits the next ticket and resumes the counter.
export const demos = ['demo/nguis/ticket-dispenser']
export const url = '/demo/nguis/ticket-dispenser/'

const click =
  `(() => { const b = [...document.querySelectorAll('button')].find(b => b.textContent.includes('Take a number')); if (b) b.click(); return !!b })()`

export const run = async ({ ev, assertEq, sleep }) => {
  const t0 = await ev(`document.body.textContent`)
  assertEq(t0.includes('—'), true, 'before the first ticket the display shows the dash pane')
  assertEq(t0.includes('Press the button to draw the first ticket.'), true, 'the pre-first-ticket hint pane is attached')

  assertEq(await ev(click), true, 'the dispenser button is attached')
  await sleep(100)
  const t1 = await ev(`document.body.textContent`)
  assertEq(t1.includes('#1'), true, 'unfolding seed primed retain: the first press draws ticket #1')
  assertEq(t1.includes('Now serving ticket 1.'), true, 'the serving pane replaced the hint (provided)')

  await ev(click)
  await sleep(100)
  assertEq((await ev(`document.body.textContent`)).includes('Now serving ticket 2.'), true, 'the counter resumed as the resume case: second press serves #2')
}
