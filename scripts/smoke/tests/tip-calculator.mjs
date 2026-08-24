// RESEARCH (open-row editors): the joint merge and the dual-bound editor.
// "Tip percentage" is edited by BOTH the MDC slider and a native range
// input, combined with <> (broadcast in, last writer wins) — a pattern the
// owned merge rejects. Moving the native range must update the model, the
// readout line, AND the sibling MDC slider, within one loop turn.
export const demos = ['demo/nguis/tip-calculator-mdc2']
export const url = '/demo/nguis/tip-calculator-mdc2/'

const tipLine = `[...document.querySelectorAll('#demo-column p')].map(p => p.textContent).find(t => t.startsWith('Tip: '))`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(tipLine), 'Tip: 15%', 'the seeded tip renders through the joint merge')
  assertEq(await ev(`[...document.querySelectorAll('input[type=range]')].find(r => !r.closest('.mdc-slider')).value`), '15', 'the native range is fed the same seeded quantity')

  // the user moves the NATIVE range — the second writer of the same field
  await ev(`(() => { const r = [...document.querySelectorAll('input[type=range]')].find(r => !r.closest('.mdc-slider')); r.value = '25'; r.dispatchEvent(new Event('input', { bubbles: true })) })()`)
  await sleep(50)
  assertEq(await ev(tipLine), 'Tip: 25%', 'the range write reached the model (last writer wins)')
  assertEq(await ev(`document.querySelector('.mdc-slider input').getAttribute('value') || document.querySelector('.mdc-slider input').value`), '25', 'the sibling MDC slider re-fed to the same value — two views, one field')
}
