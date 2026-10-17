import { byName } from '../../demos.mjs'
const names = ['temperature-converter-mdc3', 'temperature-converter-html', 'flight-booker-mdc3', 'flight-booker-html',
  'crud-mdc3', 'circle-drawer-mdc3', 'circle-drawer-html', 'cells-mdc3']
export const demos = names
export const pages = names.map(n => ({ url: `/${byName(n).page}`, label: n }))

const deepInput = name => `(() => { const h = document.querySelector('#demo-column [name="${name}"]'); if (!h) return null;
  return [h, ...h.querySelectorAll('*')].map(c => c.tagName === 'INPUT' ? c : c.shadowRoot?.querySelector('input')).find(Boolean) })()`
const valueOf = name => `(${deepInput(name)})?.value`
const findButton = n => `[...document.querySelectorAll('#demo-column *, dialog *, md-dialog *')].find(e => /button/i.test(e.tagName) && e.isConnected && e.getClientRects().length && e.textContent.trim().endsWith(${JSON.stringify(n)}))`
const press = n => `(() => { const e = ${findButton(n)}; if (!e) return false; e.click(); return true })()`
const shown = n => `!!${findButton(n)}`
const outside = `[...document.body.children].filter(c => c.id !== 'source-panel').map(c => c.innerText).join(' ')`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column')`)); i++) await sleep(200)
  await sleep(800)
  const typeInto = async (name, text) => {
    await ev(`(() => { const e = ${deepInput(name)}; e.focus(); e.select(); return true })()`)
    await session.send('Input.insertText', { text })
    await sleep(400)
  }
  const mouse = async (type, x, y, button = 'none', clickCount = 0) =>
    session.send('Input.dispatchMouseEvent', { type, x, y, button, clickCount })

  if (page.startsWith('temperature')) {
    assertEq([await ev(valueOf('Celsius')), await ev(valueOf('Fahrenheit'))], ['', ''], `[${page}] both fields start empty`)
    await typeInto('Celsius', '100')
    assertEq(await ev(valueOf('Fahrenheit')), '212', `[${page}] typing C updates F`)
    await typeInto('Fahrenheit', 'abc')
    assertEq(await ev(valueOf('Celsius')), '100', `[${page}] a non-number in F leaves C`)
    await typeInto('Fahrenheit', '50')
    assertEq(await ev(valueOf('Celsius')), '10', `[${page}] typing F updates C`)
  } else if (page.startsWith('flight')) {
    const invalid = `!!document.querySelector('#demo-column [aria-invalid="true"], #demo-column [data-aria-invalid="true"]')`
    assertEq(await ev(invalid), false, `[${page}] no field red at mount`)
    assertEq(await ev(shown('Book')), true, `[${page}] Book shown for the planned trip`)
    await typeInto('Start date', '31.02.2026')
    assertEq(await ev(invalid), true, `[${page}] a date the calendar lacks turns its field red`)
    assertEq(await ev(shown('Book')), false, `[${page}] and Book is gone`)
    await typeInto('Start date', '01.04.2026')
    assertEq(await ev(invalid), false, `[${page}] a valid date clears the red`)
    assertEq(await ev(press('Book')), true, `[${page}] Book pressed`)
    await sleep(600)
    assertEq(await ev(`${outside}.includes('You have booked a one-way flight on 01.04.2026.')`), true, `[${page}] the message states the selection`)
    assertEq(await ev(press('OK')), true, `[${page}] OK closes it`)
  } else if (page.startsWith('crud')) {
    assertEq([await ev(shown('Update')), await ev(shown('Delete'))], [false, false], `[${page}] Update/Delete absent without a selection`)
    const item = t => `[...document.querySelectorAll('#demo-column *')].find(e => e.children.length === 0 && e.textContent.trim() === ${JSON.stringify(t)})`
    assertEq(await ev(`(() => { const e = ${item('Mustermann, Max')}; if (!e) return false; e.click(); return true })()`), true, `[${page}] a person picked`)
    await sleep(400)
    assertEq([await ev(shown('Update')), await ev(shown('Delete')), await ev(valueOf('Name'))], [true, true, 'Max'], `[${page}] Update/Delete present and the fields filled`)
    await typeInto('Surname prefix', 'E')
    assertEq([await ev(shown('Update')), await ev(shown('Delete'))], [false, false], `[${page}] a filter hiding the pick drops it`)
    await typeInto('Surname prefix', '')
    await typeInto('Name', 'Ada'); await typeInto('Surname', 'Lovelace')
    await ev(press('Create')); await sleep(400)
    assertEq(await ev(`!!${item('Lovelace, Ada')}`), true, `[${page}] Create appends`)
  } else if (page.startsWith('circle')) {
    const r = await ev(`(() => { const r = document.querySelector('#demo-column svg').getBoundingClientRect(); return { x: r.x, y: r.y, k: r.width / 500 } })()`)
    const at = (x, y) => [r.x + x * r.k, r.y + y * r.k]
    const fill = `document.querySelector('#demo-column svg circle')?.getAttribute('fill')`
    const radius = `document.querySelector('#demo-column svg circle')?.getAttribute('r')`
    const count = `document.querySelectorAll('#demo-column svg circle').length`
    await mouse('mouseMoved', ...at(150, 150))
    await mouse('mousePressed', ...at(150, 150), 'left', 1); await mouse('mouseReleased', ...at(150, 150), 'left', 1)
    await sleep(300)
    assertEq(await ev(count), 1, `[${page}] a left click on empty canvas adds a circle`)
    await mouse('mouseMoved', ...at(400, 250)); await sleep(300)
    assertEq(await ev(fill), 'transparent', `[${page}] unfilled once the pointer leaves it`)
    await mouse('mouseMoved', ...at(155, 152)); await sleep(300)
    assertEq(await ev(fill), '#ccc', `[${page}] grey under the pointer (hover)`)
    await mouse('mousePressed', ...at(155, 152), 'right', 1); await mouse('mouseReleased', ...at(155, 152), 'right', 1)
    await sleep(300)
    assertEq(await ev(press('Adjust diameter..')), true, `[${page}] a right click opens "Adjust diameter.."`)
    await sleep(300)
    const set = await ev(`(() => { const h = document.querySelector('#demo-column [name="Diameter"]'); if (!h) return false;
      const e = h.tagName === 'INPUT' ? h : h; e.value = 80; e.dispatchEvent(new Event('input', { bubbles: true, composed: true })); e.dispatchEvent(new Event('change', { bubbles: true, composed: true })); return true })()`)
    assertEq(set, true, `[${page}] the adjust frame shows a slider`)
    await sleep(300)
    assertEq(await ev(radius), '40', `[${page}] the slider resizes live`)
    await ev(`(() => { const h = document.querySelector('#demo-column [name="Diameter"]'); h.value = 100; h.dispatchEvent(new Event('input', { bubbles: true, composed: true })); return true })()`)
    await sleep(300)
    assertEq(await ev(press('Close')), true, `[${page}] Close pressed`)
    await sleep(300)
    await ev(press('Undo')); await sleep(300)
    assertEq([await ev(count), await ev(radius)], [1, '20'], `[${page}] one undo reverts the whole adjustment`)
    await ev(press('Undo')); await sleep(300)
    assertEq(await ev(count), 0, `[${page}] the next undo removes the circle`)
    await ev(press('Redo')); await ev(press('Redo')); await sleep(300)
    assertEq(await ev(radius), '50', `[${page}] redo twice restores it adjusted`)
  } else {
    const cell = (row, col) => `document.querySelectorAll('#demo-column table tr')[${row + 1}].children[${col + 1}]`
    assertEq(await ev(`document.querySelectorAll('#demo-column table tr').length`), 101, `[${page}] 100 rows plus the header`)
    assertEq([await ev(`${cell(1, 3)}.innerText.trim()`), await ev(`${cell(3, 3)}.innerText.trim()`)], ['5', '9'], `[${page}] values, not formulas`)
    await ev(`(() => { const b = ${cell(1, 1)}.querySelector('*'); b.dispatchEvent(new MouseEvent('dblclick', { bubbles: true })); return true })()`)
    await sleep(300)
    assertEq(await ev(`!!${cell(1, 1)}.querySelector('input')`), true, `[${page}] a double click edits the cell in place`)
    await ev(`(() => { const i = ${cell(1, 1)}.querySelector('input'); i.focus(); i.select(); return true })()`)
    await session.send('Input.insertText', { text: '10' })
    await ev(`${cell(1, 1)}.querySelector('input').blur()`)
    await sleep(400)
    assertEq([await ev(`${cell(1, 1)}.innerText.trim()`), await ev(`${cell(1, 3)}.innerText.trim()`), await ev(`${cell(3, 3)}.innerText.trim()`)], ['10', '20', '24'], `[${page}] leaving commits; dependents and their dependents update`)
  }
}
