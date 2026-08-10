// The MDC2 discrete slider over a bounded quantity whose min > 0 — the
// foundation reads its initial numbers from the input's attributes at
// construction, so this asserts the leaf writes them there (a property-only
// write left the markup's value="0" out of range and threw in
// validateProperties, killing the form's merge gate). Then the effectful
// generate walk: button # asCase -> action/atCase -> updated.
export const demos = ['demo/nguis/password-generator-mdc2']
export const url = '/demo/nguis/password-generator-mdc2/'

const strength = `[...document.querySelectorAll('#demo-column *')].map(x => x.textContent).find(t => t.startsWith('Strength: '))`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelector('.mdc-slider--discrete') !== null`), true, 'the discrete slider initialized')
  assertEq(await ev(`document.querySelector('.mdc-slider__input').getAttribute('value')`), '16', 'the seeded length reached the foundation via the value attribute')
  assertEq(await ev(strength), 'Strength: strong', 'the seeded strength renders — the merge gate opened')

  await ev(`(() => { const i = document.querySelector('.mdc-slider__input'); i.value = '60'; i.dispatchEvent(new Event('input', { bubbles: true })); i.dispatchEvent(new Event('change', { bubbles: true })) })()`)
  await sleep(150)
  assertEq(await ev(strength), 'Strength: very strong', 'dragging the slider re-grades the strength')

  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Generate')).click()`)
  await sleep(400)
  assertEq(await ev(`document.getElementById('password').textContent.length`), 60, 'generate samples a password of the chosen length')
}
