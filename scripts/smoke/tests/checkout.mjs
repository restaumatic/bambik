// The seeded `folding` on a live page: the fold state's initial value is
// `folding`'s argument, emitted once as the `next` case at registration —
// the wizard opens on its first step with no `announce` operand in the
// event merge. Walks the three steps forward and places the order.
export const demos = ['demo/nguis/checkout-mdc2']
export const url = '/demo/nguis/checkout-mdc2/'

const click = label =>
  `(() => { const b = [...document.querySelectorAll('button')].find(b => b.textContent.includes('${label}')); if (b) b.click(); return !!b })()`

const bodyText = `document.body.textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq((await ev(bodyText)).includes('Step 1 of 3 — Cart: Wireless Headphones'), true, 'folding seed opens the wizard on the cart step at t=0')

  assertEq(await ev(click('Next')), true, 'Next button attached at the cart step (provided)')
  await sleep(100)
  assertEq((await ev(bodyText)).includes('Step 2 of 3 — Shipping to 221B Baker Street'), true, 'the next case loops: shipping step')

  assertEq(await ev(click('Back')), true, 'Back button attached at the shipping step (provided)')
  await sleep(100)
  assertEq((await ev(bodyText)).includes('Step 1 of 3'), true, 'the next case loops backward: cart step again')

  await ev(click('Next'))
  await sleep(100)
  await ev(click('Next'))
  await sleep(100)
  assertEq((await ev(bodyText)).includes('Step 3 of 3 — Pay with card •••• 4242'), true, 'payment step reached')

  assertEq(await ev(click('Place order')), true, 'Place order button attached at the payment step')
  await sleep(100)
  assertEq((await ev(bodyText)).includes('Order placed: Wireless Headphones → 221B Baker Street (card •••• 4242)'), true, 'the placed done-case exits the fold into the model')
}
