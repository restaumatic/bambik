// Fluent selectors as groups over option leaves: walks meeting-booker's
// room dropdown, duration radio group and optional catering dropdown with
// real clicks, checking that each pick is stored (the plan line follows)
// and exclusive, and that picking the catering's empty option clears it.
import { byName } from '../../demos.mjs'

export const demos = ['meeting-booker-fluent']
export const pages = [{ url: `/${byName('meeting-booker-fluent').page}`, label: 'fluent' }]

const planLine = `[...document.querySelectorAll('#demo-column fluent-text')].map(t => t.textContent).find(t => t.startsWith('Plan: ')) ?? ''`
const dropdowns = `[...document.querySelectorAll('fluent-dropdown')]`
const dropdownValue = (i) => `(${dropdowns}[${i}].value ?? '')`
const checkedRadios = `[...document.querySelectorAll('fluent-radio')].filter(r => r.checked).map(r => r.value)`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('fluent-dropdown')`)); i++) await sleep(200)
  await sleep(800)

  const mouse = (type, x, y) => session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
  // an open listbox is anchored to its dropdown, and a headless session
  // repositions it only on a frame, so an option is clicked where it stands
  const click = async (js, scroll = true) => {
    const [x, y] = await ev(`(() => { const el = ${js}; if (${scroll}) el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    await mouse('mouseMoved', x, y)
    await mouse('mousePressed', x, y)
    await mouse('mouseReleased', x, y)
    await sleep(400)
  }
  const pick = async (i, value) => {
    await click(`${dropdowns}[${i}]`)
    await click(`[...${dropdowns}[${i}].querySelectorAll('fluent-option')].find(o => o.value === ${JSON.stringify(value)})`, false)
  }

  assertEq(await ev(dropdownValue(0)), '', `[${page}] no room is picked at first`)
  assertEq(await ev(checkedRadios), [], `[${page}] no duration is checked at first`)
  assertEq(await ev(planLine), '', `[${page}] an incomplete meeting has no plan`)

  await pick(0, 'Boardroom')
  assertEq(await ev(dropdownValue(0)), 'Boardroom', `[${page}] picking a room selects it`)

  await click(`[...document.querySelectorAll('fluent-radio')].find(r => r.value === '30 min')`)
  assertEq(await ev(checkedRadios), ['30 min'], `[${page}] clicking a radio makes it the one checked radio`)
  assertEq((await ev(planLine)).includes('boardroom, 30 min'), true, `[${page}] room and duration are stored (the plan names both)`)

  await click(`[...document.querySelectorAll('fluent-radio')].find(r => r.value === '60 min')`)
  assertEq(await ev(checkedRadios), ['60 min'], `[${page}] another radio takes over the check`)
  assertEq((await ev(planLine)).includes('60 min'), true, `[${page}] the new duration is stored`)

  await pick(1, 'Sandwich lunch')
  assertEq(await ev(dropdownValue(1)), 'Sandwich lunch', `[${page}] picking a catering selects it`)
  assertEq((await ev(planLine)).endsWith(', sandwich lunch'), true, `[${page}] the catering is stored (the plan ends with it)`)

  await pick(1, '')
  assertEq(await ev(dropdownValue(1)), '', `[${page}] picking the empty option clears the catering`)
  assertEq((await ev(planLine)).includes('lunch'), false, `[${page}] the cleared catering leaves the plan`)

  await pick(0, 'Auditorium')
  assertEq(await ev(dropdownValue(0)), 'Auditorium', `[${page}] another room replaces the first`)
  assertEq((await ev(planLine)).includes('auditorium, 60 min'), true, `[${page}] the plan follows the room`)
}
