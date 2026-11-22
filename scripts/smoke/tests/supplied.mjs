// A source below a line that has released a field (`supplied`), on the real
// DOM (incident-report): the photo capture stands under the description
// editor, its field joins the row the lines above built, a file the user
// picks lands on the row last fed — the description kept — and an edit
// above re-feeds the capture without disturbing it, while the line that
// reads the time goes on by itself.
import { mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { byName } from '../../demos.mjs'

export const demos = ['incident-report-mdc3']
export const pages = [{ url: `/${byName('incident-report-mdc3').page}`, label: 'incident-report' }]

const column = `document.querySelector('#demo-column')`
const field = `${column}.querySelector('md-filled-text-field[name="Description"]')`
const report = `[...${column}.querySelectorAll('*')].map(e => e.textContent).find(t => t.startsWith('Reporting'))`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  for (let i = 0; i < 25 && !(await ev(`!!${field}`)); i++) await sleep(200)
  await sleep(800)
  const result = async (method, params) => { const r = await session.send(method, params); return r.result ?? r }

  assertEq(await ev(`[...${column}.querySelectorAll('md-filled-text-field, input[type=file]')].map(e => e.getAttribute('name'))`), ['Description', 'Photo'], `[${page}] the capture stands under the editor`)
  assertEq((await ev(report)).includes('no photo attached'), true, `[${page}] the capture's field is in the row before anything is picked`)

  await ev(`${field}.focus()`)
  await session.send('Input.insertText', { text: 'a loose kerb' })
  await sleep(300)
  assertEq((await ev(report)).includes('a loose kerb'), true, `[${page}] an edit above the capture reaches the lines below it`)
  assertEq((await ev(report)).includes('no photo attached'), true, `[${page}] and the capture answers it with what it holds`)

  const photo = join(mkdtempSync(join(tmpdir(), 'bambik-smoke-')), 'kerb.jpg')
  writeFileSync(photo, Buffer.alloc(5 * 1024, 7))
  const { root } = await result('DOM.getDocument', { depth: 0 })
  const { nodeId } = await result('DOM.querySelector', { nodeId: root.nodeId, selector: '#demo-column input[type=file]' })
  await session.send('DOM.setFileInputFiles', { nodeId, files: [photo] })
  await sleep(500)
  assertEq((await ev(report)).includes('with kerb.jpg (5 kB)'), true, `[${page}] a picked file joins the row`)
  assertEq((await ev(report)).includes('a loose kerb'), true, `[${page}] landing on the row last fed, the description kept`)

  await ev(`${field}.focus()`)
  await session.send('Input.insertText', { text: ' by the gate' })
  await sleep(300)
  assertEq((await ev(report)).includes('a loose kerb by the gate'), true, `[${page}] an edit after the pick re-feeds the capture`)
  assertEq((await ev(report)).includes('with kerb.jpg (5 kB)'), true, `[${page}] which keeps the file`)

  const before = await ev(report)
  await sleep(2200)
  const after = await ev(report)
  assertEq(after !== before, true, `[${page}] the line that reads the time follows the clock`)
  assertEq(after.includes('with kerb.jpg (5 kB)') && after.includes('a loose kerb by the gate'), true, `[${page}] the row it reads unchanged`)
  assertEq(await ev(`${field}.value`), 'a loose kerb by the gate', `[${page}] the editor still shows what was typed`)
}
