// What becomes of a modal's case, on the real DOM (weather): under
// `acknowledged` the case ends with the dialog and nothing goes on; under
// `continued` the dialog's "OK" lets the case go on as the one its exit
// names — the storm warning, once read, is folded as the forecast — and a
// dialog closed any other way lets nothing go on.
import { byName } from '../../demos.mjs'

export const demos = ['weather-mdc3']
export const pages = [{ url: `/${byName('weather-mdc3').page}`, label: 'weather' }]

const column = `document.querySelector('#demo-column')`
const dialogs = `[...${column}.querySelectorAll('md-dialog')]`
const openTitled = `${dialogs}.filter(d => d.open).map(d => d.getAttribute('aria-label'))`
const ok = `(() => {
  const d = ${dialogs}.find(d => d.open)
  const b = d && [...d.querySelectorAll('[slot=actions] > *')].pop()
  if (!b) return false
  b.click()
  return true
})()`
const city = name => `(() => {
  const i = [...${column}.querySelectorAll('md-list-item')].find(i => i.textContent.trim() === ${JSON.stringify(name)})
  if (!i) return false
  i.click()
  return true
})()`
const about = `(() => {
  const b = [...${column}.querySelectorAll('md-icon-button')].find(b => b.getAttribute('aria-label') === 'About this dashboard')
  if (!b) return false
  b.click()
  return true
})()`
const text = `${column}.innerText`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!${column}?.querySelector('md-list-item')`)); i++) await sleep(200)
  const has = async s => (await ev(text)).includes(s)

  assertEq(await ev(about), true, `[${page}] the About button is on screen`)
  await sleep(400)
  assertEq(await ev(openTitled), ['About this dashboard'], `[${page}] acknowledged: its case opens the modal it titles`)
  assertEq(await ev(ok), true, `[${page}] acknowledged: the modal has its OK`)
  await sleep(400)
  assertEq(await ev(openTitled), [], `[${page}] acknowledged: the OK closes it`)
  assertEq(await has('1 report served'), true, `[${page}] acknowledged: the case ends there, nothing is folded`)

  assertEq(await ev(city('Lisbon')), true, `[${page}] Lisbon is listed`)
  await sleep(1300)
  assertEq(await ev(openTitled), [], `[${page}] a plain forecast opens no modal`)
  assertEq(await has('in Lisbon'), true, `[${page}] a plain forecast is folded as it arrives`)
  assertEq(await has('2 reports served'), true, `[${page}] and counted`)

  assertEq(await ev(city('Singapore')), true, `[${page}] Singapore is listed`)
  await sleep(1300)
  assertEq(await ev(openTitled), ['Weather warning'], `[${page}] continued: the warning opens the modal it titles`)
  assertEq(await has('Thunderstorm over Singapore'), true, `[${page}] continued: the modal shows the warning's payload`)
  assertEq(await has('in Lisbon'), true, `[${page}] continued: nothing is folded while the modal is open`)
  await session.send('Input.dispatchKeyEvent', { type: 'keyDown', key: 'Escape', code: 'Escape', windowsVirtualKeyCode: 27 })
  await session.send('Input.dispatchKeyEvent', { type: 'keyUp', key: 'Escape', code: 'Escape', windowsVirtualKeyCode: 27 })
  await sleep(500)
  assertEq(await ev(openTitled), [], `[${page}] continued: Escape closes the modal`)
  assertEq(await has('in Lisbon') && await has('2 reports served'), true, `[${page}] continued: a modal closed without its OK lets nothing go on`)

  assertEq(await ev(city('Singapore')), true, `[${page}] Singapore is still listed`)
  await sleep(1300)
  assertEq(await ev(openTitled), ['Weather warning'], `[${page}] continued: the next warning opens the modal again`)
  assertEq(await ev(ok), true, `[${page}] continued: the modal has its OK`)
  await sleep(500)
  assertEq(await ev(openTitled), [], `[${page}] continued: the OK closes it`)
  assertEq(await has('Thunderstorm in Singapore'), true, `[${page}] continued: the warning goes on as the forecast its exit names`)
  assertEq(await has('3 reports served'), true, `[${page}] continued: and is folded once`)
}
