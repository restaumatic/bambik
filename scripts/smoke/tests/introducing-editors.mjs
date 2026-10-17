// The introducing editor (PUI.introduced), on the seedless signup form. An upstream edit
// re-feeds every introducing editor below it; each must keep the user's value
// (its init unchanged) rather than re-show its init, and the readout below
// must follow the edits.
import { byName } from '../../demos.mjs'

export const demos = ['signup-form-mdc3']
export const pages = [{ url: `/${byName('signup-form-mdc3').page}`, label: 'mdc3' }]

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column md-filled-text-field')`)); i++) await sleep(200)
  await sleep(600)
  const type = async (text) => { await session.send('Input.insertText', { text }) }
  const q = (sel) => `document.querySelector('#demo-column ${sel}')`

  assertEq(await ev(`${q('md-filled-text-field[name="Username"]')}.value`), '', `[${page}] Username starts at its init`)
  assertEq(await ev(`[...document.querySelectorAll('#demo-column md-radio')].map(r => r.checked)`), [true, false, false], `[${page}] Plan starts at its init (Free)`)

  await ev(`${q('md-filled-text-field[name="Username"]')}.focus()`)
  await type('zofia')
  await sleep(500)
  assertEq(await ev(`[...document.querySelectorAll('#demo-column md-radio')].map(r => r.checked)`), [true, false, false], `[${page}] a Username edit leaves Plan at its init`)

  await ev(`[...document.querySelectorAll('#demo-column md-radio')][1].click()`)
  await sleep(300)
  assertEq(await ev(`[...document.querySelectorAll('#demo-column md-radio')].map(r => r.checked)`), [false, true, false], `[${page}] the user picks Pro`)

  await ev(`${q('md-filled-text-field[name="Email"]')}.focus()`)
  await type('z@example.com')
  await sleep(300)
  await ev(`${q('md-filled-text-field[name="Username"]')}.focus()`)
  await type('2')
  await sleep(500)
  assertEq(await ev(`[...document.querySelectorAll('#demo-column md-radio')].map(r => r.checked)`), [false, true, false], `[${page}] a later Username edit keeps the user's Pro`)
  assertEq(await ev(`${q('md-filled-text-field[name="Email"]')}.value`), 'z@example.com', `[${page}] and keeps the typed Email`)
  assertEq(await ev(`${q('md-filled-text-field[name="Username"]')}.value`), 'zofia2', `[${page}] Username holds both edits`)
  const readout = await ev(`document.querySelector('#demo-column').textContent`)
  assertEq(readout.includes('zofia2 is available'), true, `[${page}] the username readout follows the edit`)
  assertEq(readout.includes('accept the terms'), true, `[${page}] validation reads the whole row the editors built`)
}
