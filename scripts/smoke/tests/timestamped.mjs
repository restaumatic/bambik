// The time beside the model (`timestamped`, `timestamping`), on the real DOM
// (punch-clock): a line that reads the time follows the wall clock on its
// own, a beat reaches no line that does not read it — the name field, fed
// on every turn of the knot, is fed by none — and a click's event carries
// the time it left at into its fold.
import { byName } from '../../demos.mjs'

export const demos = ['punch-clock-mdc3']
export const pages = [{ url: `/${byName('punch-clock-mdc3').page}`, label: 'punch-clock' }]

const column = `document.querySelector('#demo-column')`
const field = `${column}.querySelector('md-filled-text-field[name="Name"]')`
const button = `[...${column}.querySelectorAll('md-filled-button')].find(b => b.textContent.includes('Punch in'))`
const lines = `${column}.innerText.split('\\n').map(l => l.trim()).filter(l => l)`
const clockLine = `${lines}.find(l => l.startsWith('It is'))`
const punchLine = `${lines}.find(l => l.startsWith('Anna'))`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  for (let i = 0; i < 25 && !(await ev(`!!${button}`)); i++) await sleep(200)
  await sleep(800)

  // every write of the name field's value, counted: the knot's feeds of it
  await ev(`(() => {
    const el = ${field}
    let d = null
    for (let proto = Object.getPrototypeOf(el); proto && !d; proto = Object.getPrototypeOf(proto)) d = Object.getOwnPropertyDescriptor(proto, 'value')
    window.__fed = 0
    Object.defineProperty(el, 'value', { configurable: true, get() { return d.get.call(this) }, set(v) { window.__fed++; d.set.call(this, v) } })
  })()`)

  const before = await ev(clockLine)
  await sleep(2300)
  assertEq(await ev(clockLine) !== before, true, `[${page}] the line that reads the time follows the clock`)
  assertEq(await ev(`window.__fed`), 0, `[${page}] and no beat feeds the name field: the knot is not turned`)

  await ev(`${field}.focus()`)
  await session.send('Input.insertText', { text: 'Anna' })
  await sleep(1300)
  assertEq(await ev(`${field}.value`), 'Anna', `[${page}] what is typed stays typed across a beat`)
  await ev(`${button}.click()`)
  await sleep(300)
  const fedByPunch = await ev(`window.__fed`)
  assertEq(fedByPunch > 0, true, `[${page}] a punch turns the knot (the probe sees a feed)`)
  assertEq(/^Anna · in at \d\d:\d\d:\d\d · just now$/.test(await ev(punchLine)), true, `[${page}] the click's event carried its time into the fold`)
  assertEq((await ev(punchLine)).slice(13, 21) <= (await ev(clockLine)).slice(6), true, `[${page}] a time not after the clock's`)
  await sleep(6200)
  assertEq(/^Anna · in at \d\d:\d\d:\d\d · \d+ s ago$/.test(await ev(punchLine)), true, `[${page}] the list reads the time too: the punch ages`)
  assertEq(await ev(`window.__fed`), fedByPunch, `[${page}] with the knot at rest`)
}
