// The updatable selection (`actedWithin`) on the real DOM: an editor fed a
// row a function picked from a stored array writes the row back where its
// key is stored. Carrier-only: the element of a surviving key keeps its
// node and its focus while the list beside it is re-rendered from the same
// data, a row that leaves the selection takes its element with it — the
// content a pane among its siblings attached after the build included (the
// contacts form is a bare run of fields, its last a pane) — and an edit
// made through one selection is there under another.
import { byName } from '../../demos.mjs'

export const demos = ['contacts-mdc3', 'movie-browser-mdc3']
export const pages = [
  { url: `/${byName('contacts-mdc3').page}`, label: 'contacts' },
  { url: `/${byName('movie-browser-mdc3').page}`, label: 'movie-browser' },
]

const column = `document.querySelector('#demo-column')`
const items = `[...${column}.querySelectorAll('md-list-item')]`
const field = name => `${column}.querySelector('md-filled-text-field[name="${name}"]')`
const press = name => `(() => {
  const b = [...${column}.querySelectorAll('md-filled-button, button')].find(b => (b.getAttribute('aria-label') ?? b.textContent).includes(${JSON.stringify(name)}))
  if (!b) return false
  b.click()
  return true
})()`

const contacts = async ({ ev, session, assertEq, sleep, page }) => {
  const type = async text => { await session.send('Input.insertText', { text }) }
  const lines = () => ev(`${items}.map(i => i.textContent.trim())`)

  assertEq((await lines()).length, 3, `[${page}] the list shows the stored contacts`)
  assertEq(await ev(`${field('Name')} === null`), true, `[${page}] nothing is picked, so the selection is empty and no form exists`)

  await ev(`${items}[0].click()`)
  await sleep(300)
  assertEq(await ev(`${field('Name')}.value`), 'Ada Lovelace', `[${page}] the form is the picked row`)
  assertEq(await ev(`${field('Tax number')} === null`), true, `[${page}] a person has no tax number field`)

  await ev(`(() => { const f = ${field('Name')}; f.__mark = 'kept'; f.focus() })()`)
  await type(' King')
  await sleep(400)
  assertEq((await lines())[0].startsWith('Ada Lovelace King'), true, `[${page}] the edit landed in the stored array: the list reads it`)
  assertEq(await ev(`${field('Name')}.__mark`), 'kept', `[${page}] the element of the surviving key keeps its node`)
  assertEq(await ev(`document.activeElement === ${field('Name')}`), true, `[${page}] and its focus`)
  assertEq(await ev(`${field('Name')}.value`), 'Ada Lovelace King', `[${page}] and the text typed`)

  await ev(`${items}[1].click()`)
  await sleep(300)
  assertEq(await ev(`${field('Name')}.value`), 'Analytical Engines Ltd', `[${page}] another pick is another row`)
  assertEq(await ev(`${field('Name')}.__mark === undefined`), true, `[${page}] and another element: the leaver took its node`)
  assertEq(await ev(`${field('Tax number')}.value`), 'GB 123 4567 89', `[${page}] a company's tax number field exists`)

  assertEq(await ev(press('New contact')), true, `[${page}] New contact is on screen`)
  await sleep(300)
  assertEq((await lines()).length, 4, `[${page}] a new row is stored`)
  assertEq(await ev(`${field('Name')}.value`), '', `[${page}] and picked, blank`)
  assertEq(await ev(`${field('Tax number')} === null`), true, `[${page}] the company that left took what its pane had attached since it was built`)

  assertEq(await ev(press('Delete contact')), true, `[${page}] Delete contact exists while a contact is picked`)
  await sleep(300)
  assertEq((await lines()).length, 3, `[${page}] the picked row is gone`)
  assertEq(await ev(`${field('Name')} === null`), true, `[${page}] and the form with it`)
  assertEq((await lines())[0].startsWith('Ada Lovelace King'), true, `[${page}] the earlier edit is still stored`)
}

const movies = async ({ ev, assertEq, sleep, page }) => {
  const toggles = `[...${column}.querySelectorAll('md-list-item md-icon-button')]`
  const tab = name => `[...${column}.querySelectorAll('md-primary-tab')].find(t => t.textContent.includes(${JSON.stringify(name)})).click()`
  const favorites = () => ev(`${column}.textContent.match(/(\\d+) favorites?/)[1]`)

  assertEq(await ev(`${toggles}.length`), 12, `[${page}] every movie is selected at first`)
  await ev(`${toggles}[0].click()`)
  await sleep(300)
  assertEq(await favorites(), '1', `[${page}] a toggle in the selection writes the stored movie`)

  await ev(tab('Drama'))
  await sleep(300)
  assertEq(await ev(`${toggles}.length`), 4, `[${page}] the selection follows the filter`)
  assertEq(await ev(`${toggles}.some(t => t.selected)`), false, `[${page}] the favourite is outside this selection`)
  await ev(`${toggles}[1].click()`)
  await sleep(300)
  assertEq(await favorites(), '2', `[${page}] an edit through a filtered selection lands beside the first`)

  await ev(tab('All'))
  await sleep(300)
  assertEq(await ev(`${toggles}.map(t => t.selected).filter(Boolean).length`), 2, `[${page}] both are there under the whole selection`)
  assertEq(await ev(`${toggles}[0].selected`), true, `[${page}] the first where it was made`)
}

export const run = async ctx => {
  const { ev, sleep, page } = ctx
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column md-list-item')`)); i++) await sleep(200)
  await sleep(600)
  await (page === 'contacts' ? contacts(ctx) : movies(ctx))
}
