// observed end-to-end: payment's retryToast sits INSIDE the iterate chain —
// each declined attempt is shown by the snackbar AND passes on to loop, so
// the flaky charge still converges (the pass-through forwards every event
// exactly once, at feed time; the status's own emissions are dropped).
export const demos = ['demo/nguis/payment-mdc2']
export const url = '/demo/nguis/payment-mdc2/'

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(300)

  assertEq(await ev(`document.body.textContent.includes('Ready to charge')`),
    true, 'seeded status line renders')

  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Charge card')).click()`)

  // chargeFlaky declines attempts 0 and 1 (700ms each) before approving
  await sleep(1000)
  assertEq(await ev(`document.querySelector('.mdc-snackbar__label').textContent.includes('retrying (attempt 1)')`),
    true, 'the first declined attempt is narrated by the observed snackbar')

  await sleep(1800)
  assertEq(await ev(`document.body.textContent.includes('Approved — $42.0 charged on attempt 3')`),
    true, 'the observed events still passed on — the retry loop converged')
  assertEq(await ev(`document.querySelector('.mdc-snackbar__label').textContent.includes('retrying (attempt 2)')`),
    true, 'the last retry narration stands (attempt 2 was the final decline)')
}
