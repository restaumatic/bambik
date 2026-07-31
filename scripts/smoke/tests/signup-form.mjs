// The rewritten MD2 editor leaves end-to-end: the debounced text field
// (foundation-managed label float), the checkbox behind a real
// <label for=…>, the radio group's native exclusivity, and the
// validation panes driving the sign-up toast.
export const demos = ['demo/nguis/signup-form-mdc2']
export const url = '/demo/nguis/signup-form-mdc2/'

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(300)
  assertEq(await ev(`document.body.textContent.includes('Pick a username to check its availability')`),
    true, 'seeded render shows the unnamed hint pane')

  const type = (idx, value) => ev(`(() => {
    const input = document.querySelectorAll('.mdc-text-field__input')[${idx}]
    input.value = ${JSON.stringify(value)}
    input.dispatchEvent(new Event('input', { bubbles: true }))
    return true
  })()`)

  await type(0, 'eryk')
  await sleep(600) // 300ms leaf debounce
  assertEq(await ev(`document.body.textContent.includes('eryk is already taken')`),
    true, 'a taken username shows the taken pane after the debounce')

  await type(0, 'grace')
  await sleep(600)
  assertEq(await ev(`document.body.textContent.includes('grace is available')`),
    true, 'an available username shows the available pane')
  assertEq(await ev(`document.querySelectorAll('.mdc-floating-label')[0].classList.contains('mdc-floating-label--float-above')`),
    true, 'the foundation floats the label over a non-empty field')

  await type(1, 'grace@example.com')
  await sleep(200)
  assertEq(await ev(`document.body.textContent.includes('accept the terms')`),
    true, 'validation still wants the terms checkbox')

  // the checkbox label is a real <label for=…>: clicking the text toggles the box
  await ev(`[...document.querySelectorAll('label')].find(l => l.textContent.includes('I accept the terms')).click()`)
  await sleep(200)
  assertEq(await ev(`document.querySelector('.mdc-checkbox__native-control').checked`),
    true, 'clicking the label text checked the box (label-for association)')
  assertEq(await ev(`document.body.textContent.includes('Ready to sign up as grace')`),
    true, 'the ready pane appears once the form validates')

  // native name-scoped exclusivity: choosing Pro unchecks Free
  await ev(`document.querySelectorAll('.mdc-radio__native-control')[1].click()`)
  await sleep(200)
  assertEq(await ev(`document.querySelectorAll('.mdc-radio__native-control')[1].checked`),
    true, 'the clicked radio is checked')
  assertEq(await ev(`document.querySelectorAll('.mdc-radio__native-control')[0].checked`),
    false, 'the previously selected radio is not')

  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Sign up')).click()`)
  await sleep(400)
  assertEq(await ev(`document.querySelector('.mdc-snackbar__label').textContent.includes('Welcome, grace!')`),
    true, 'signing up toasts the welcome snackbar')
}
