// The two-label case on a live page: each field's *payload* label ("Degrees
// Celsius") is distinct from the model quantity it edits (celsiusReading), and
// distinct again from the reader that formats it (celsiusText). Typing in one
// field must convert into the other, in both directions, and a non-numeric
// entry must leave the other field untouched.
export const demos = ['demo/7guis/temperature-converter-mdc2']
export const url = '/demo/7guis/temperature-converter-mdc2/'

const fields = `document.querySelectorAll('.mdc-text-field__input')`
const valueOf = i => `${fields}[${i}].value`
const type = (i, value) => `(() => {
  const input = ${fields}[${i}]
  input.value = ${JSON.stringify(value)}
  input.dispatchEvent(new Event('input', { bubbles: true }))
  return input.value
})()`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`${fields}.length`), 2, 'both temperature fields render')

  // the label is a typographic symbol; it must survive bundling into the caption
  assertEq(await ev(`[...document.querySelectorAll('.mdc-floating-label')].map(l => l.textContent)`),
    ['\u00B0C', '\u00B0F'], 'the °C/°F labels caption the fields')
  assertEq(await ev(`[...document.querySelectorAll('.mdc-text-field[name]')].map(f => f.getAttribute('name'))`),
    ['\u00B0C', '\u00B0F'], 'and are stamped as the host name attribute')
  assertEq(await ev(valueOf(0)), '20.0', 'the seeded celsius reading renders')
  assertEq(await ev(valueOf(1)), '68.0', 'the seeded fahrenheit reading renders')

  // the payload label carries the typed text; `informed` lays it over the model
  await ev(type(0, '100'))
  await sleep(80)
  assertEq(await ev(valueOf(1)), '212.0', 'typing celsius converts into fahrenheit')

  await ev(type(1, '32'))
  await sleep(80)
  assertEq(await ev(valueOf(0)), '0.0', 'typing fahrenheit converts back into celsius')

  // a non-numeric entry is not a reading, so the other field must not move
  await ev(type(0, 'warm'))
  await sleep(80)
  assertEq(await ev(valueOf(1)), '32.0', 'a non-numeric entry leaves the other field untouched')
}
