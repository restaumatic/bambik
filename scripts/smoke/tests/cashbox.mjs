// subChoice end-to-end: the outgoing money (refund, payout) detours
// through confirmation dialogs — each dialog the +→+ closed-singleton wrap
// simpleDialog … # atCase @l # toCase @l' identity, in merge position —
// while the incoming deposit posts straight to the fold, untouched by the
// focus, so every branch shares exactly { balance } (A13).
// (The MDC dialog reaches --open on a rAF, which this harness starves for
// seconds at a time, so open/close states are polled, never fixed-slept.)
export const demos = ['demo/nguis/cashbox-mdc2']
export const url = '/demo/nguis/cashbox-mdc2/'

const click = label =>
  `[...document.querySelectorAll('button')].find(b => b.textContent.includes(${JSON.stringify(label)})).click()`

export const run = async ({ ev, assertEq, sleep }) => {
  const until = async (expr, what) => {
    for (let i = 0; i < 40; i++) {
      if (await ev(expr)) return
      await sleep(250)
    }
    assertEq(false, true, what)
  }

  await sleep(300)
  assertEq(await ev(`document.body.textContent.includes('Till balance: €200')`), true, 'seeded balance renders')

  await ev(click('Take a deposit'))
  await sleep(400)
  assertEq(await ev(`document.querySelector('.mdc-dialog--open, .mdc-dialog--opening') === null`), true,
    'the background case opens no dialog — it passes the focus untouched')
  assertEq(await ev(`document.body.textContent.includes('Till balance: €250')`), true,
    'the deposit posted straight to the fold')

  await ev(click('Refund a customer'))
  await until(`document.querySelector('.mdc-dialog--open') !== null`, 'the refund dialog reaches open')
  assertEq(await ev(`document.querySelector('.mdc-dialog--open').textContent.includes('Hand €25 back to the customer.')`), true,
    'the dialog shows the bare business payload through its copy line')
  assertEq(await ev(`document.body.textContent.includes('Till balance: €250')`), true,
    'the balance is untouched while the refund awaits confirmation')

  await ev(`document.querySelector('.mdc-dialog--open .mdc-dialog__button').click()`)
  await until(`document.querySelector('.mdc-dialog--open') === null`, 'confirming closes the dialog')
  assertEq(await ev(`document.body.textContent.includes('Till balance: €225')`), true,
    'the confirmed refund re-entered the flow as its business case and folded')

  await ev(click('Pay the courier'))
  await until(`document.querySelector('.mdc-dialog--open') !== null`, 'the payout dialog reaches open')
  await ev(`document.querySelector('.mdc-dialog--open .mdc-dialog__button').click()`)
  await until(`document.body.textContent.includes('Till balance: €215')`,
    'the second wrapped case confirms through its own dialog')
}
