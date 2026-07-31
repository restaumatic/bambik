// The seeded `feedback` on a live page: the traced chain's initial state
// is `feedback`'s argument, fed once at registration — so the high-water
// readout inside the loop renders at t=0 instead of starving the
// `Costrong` gate (before the seed argument this needed a `seeded noBids`
// stage inside the chain; an unprimed loop rendered a bare
// "Highest bid so far: $" with no number and a watchdog warning).
export const demos = ['demo/nguis/auction-mdc2']
export const url = '/demo/nguis/auction-mdc2/'

export const run = async ({ ev, assertEq }) => {
  const text = await ev(`document.body.textContent`)
  assertEq(text.includes('Your current bid: $0.0'), true, 'mvu seed renders the bid readout at t=0')
  assertEq(text.includes('Highest bid so far: $0.0'), true, 'feedback seed primes the loop: the high-water readout renders at t=0')
  assertEq(await ev(`document.querySelectorAll('.mdc-slider').length`), 1, 'the bid slider is attached')
}
