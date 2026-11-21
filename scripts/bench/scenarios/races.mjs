// B3 — races: the application as it starts, its clock running, driven
// faster than a person can — clicks with no wait between them, keys typed
// while seconds pass, clicks while an effect is under way, a page whose
// last row leaves, a dialog confirmed twice. Each is a statement about
// what must be on screen afterwards.
import { answer, dialogButton, digit, dishTile, drawn, perPage, tab, tableTile } from './common.mjs'

const short = { name: 'PassShort', kind: 'station', part: 'Pass', scale: 1, perPage: { ...perPage, list: 9 } }
const orderLine = dish => `([...document.querySelectorAll('table[aria-label="Zamówienie"] tbody tr')].find(r => r.cells[0].querySelector('p').innerText.trim() === ${JSON.stringify(dish)}) ?? null)`
const quantity = dish => `(r => r ? [...r.cells[1].firstElementChild.childNodes].filter(n => n.nodeType === 3).map(n => n.nodeValue).join('').trim() : null)(${orderLine(dish)})`
const stockField = (dish, name) => `([...document.querySelectorAll('table[aria-label="Magazyn"] tbody tr')].find(r => r.cells[0].querySelector('p').innerText.trim() === ${JSON.stringify(dish)})?.querySelector('[name="${name}"]') ?? null)`
const pagerLine = `([...document.querySelectorAll('md-icon-button')].find(b => b.getAttribute('aria-label') === 'Następna strona')?.parentElement.querySelector('.md-typescale-label-large').innerText ?? null)`

export const races = {
  name: 'races',
  mounts: () => [{ name: 'App', kind: 'app' }, short],
  run: async ({ browser }) => {
    const checks = []
    const check = (name, ok, detail) => checks.push({ name, ok: !!ok, detail })
    // tall enough that the hall's tables, its order and its menu are on screen together
    const p = await browser.open('App', { height: 2000 })
    await p.waitFor(drawn.App)
    await p.sleep(300)
    const xyOf = expr => p.ev(`(() => { const el = ${expr}; return el ? window.__bench.centre(el) : null })()`)
    const fire = xy => ['mouseMoved', 'mousePressed', 'mouseReleased'].map(type => p.send('Input.dispatchMouseEvent', { type, x: xy[0], y: xy[1], button: 'left', clickCount: 1 }))

    // twelve clicks on one dish, none awaited
    const dish = 'Arancini z orzechami'
    const at = await xyOf(dishTile(dish))
    await Promise.all(Array.from({ length: 12 }, () => fire(at)).flat())
    await p.sleep(400)
    const burst = { line: await p.ev(quantity(dish)), tile: await p.ev(`${dishTile(dish)}?.innerText`) }
    check('twelve clicks on a dish with no wait between them put twelve portions on the order', burst.line === '12' && /×12/.test(burst.tile) && /dostępne: 5/.test(burst.tile), JSON.stringify(burst))

    // three digits typed over two seconds of ticks
    await p.clickOn(tab('Magazyn'))
    await p.sleep(300)
    const portions = stockField('Affogato', 'Porcje')
    await p.clickOn(portions)
    await p.send('Input.dispatchKeyEvent', { type: 'keyDown', key: 'a', code: 'KeyA', modifiers: 2, windowsVirtualKeyCode: 65 })
    await p.send('Input.dispatchKeyEvent', { type: 'keyUp', key: 'a', code: 'KeyA', modifiers: 2, windowsVirtualKeyCode: 65 })
    const ticks = () => p.ev('window.__bench.fired[1000] || 0')
    const ticksBefore = await ticks()
    for (const d of '123') { await digit(p, d); await p.sleep(750) }
    await p.sleep(1300)
    const typed = await p.ev(`(f => ({ value: f.value, caret: f.shadowRoot.querySelector('input').selectionStart, focused: document.activeElement === f }))(${portions})`)
    const passed = await ticks() - ticksBefore
    check(`three digits typed while ${passed} seconds pass stay as typed, the caret after them`, typed.value === '123' && typed.caret === 3 && typed.focused && passed >= 2, JSON.stringify(typed))
    await p.clickOn(tab('Sala'))
    await p.sleep(300)
    await p.clickOn(tab('Magazyn'))
    await p.sleep(300)
    const kept = await p.ev(`${portions}?.value`)
    check('and are the stock when the stockroom is come back to', kept === '123', JSON.stringify(kept))

    // a dish and a table clicked while the order is on its way to the kitchen
    await p.clickOn(tab('Sala'))
    await p.sleep(300)
    // the three places clicked, read with the page still: one scroll would move the other two
    const still = expr => p.ev(`(() => { const r = ${expr}.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    await p.ev('scrollTo(0, 0)')
    const send = await still(`[...document.querySelectorAll('md-filled-button')].find(b => b.innerText.includes('Wyślij do kuchni'))`)
    const other = await still(dishTile('Arancini'))
    const table = await still(tableTile(7))
    await Promise.all([...fire(send), ...fire(other), ...fire(table)])
    await p.sleep(1500)
    const served = await p.ev(`document.querySelector('h3.md-typescale-title-medium').innerText`)
    await p.clickOn(tableTile(5))
    await p.sleep(300)
    const sent = await p.ev(`document.querySelector('h3.md-typescale-title-medium').innerText`)
    const number = sent.match(/#(\d+) w kuchni/)?.[1]
    await p.clickOn(tab('Kuchnia'))
    await p.sleep(300)
    const ticket = `([...document.querySelectorAll('.md3-card')].find(c => c.querySelector('h3').innerText.startsWith('#${number} ')) ?? null)`
    for (let i = 0; i < 5 && !(await p.ev(`${ticket} !== null`)); i++) { await p.click('Następna strona'); await p.sleep(200) }
    const lines = await p.ev(`[...(${ticket}?.querySelectorAll('md-list-item') ?? [])].map(i => [...i.childNodes].filter(n => n.nodeType === 3).map(n => n.nodeValue).join('').trim())`)
    await p.clickOn(tab('Magazyn'))
    await p.sleep(300)
    await p.clickOn(`window.__bench.field('Szukaj w magazynie')`)
    await p.send('Input.insertText', { text: 'arancini' })
    await p.sleep(300)
    const left = await p.ev(`${stockField('Arancini', 'Porcje')}?.value`)
    const withOther = lines.includes('1 × Arancini')
    check('a dish clicked while the order was on its way went with it or stayed in stock, never both or neither',
      /^Stolik 7 /.test(served) && !!number && lines.includes('12 × Arancini z orzechami') && lines.length === (withOther ? 2 : 1) && left === (withOther ? '14' : '15'),
      JSON.stringify({ served, sent, lines, left }))
    const problems = p.events.map(e => `${e.kind}: ${e.text}`)
    await p.close()

    // the only row of the last page is paid
    const q = await browser.open(short.name)
    await q.waitFor(drawn.Pass)
    await q.click('Następna strona')
    await q.sleep(200)
    const last = await q.ev(pagerLine)
    await q.clickOn(`document.querySelector('table[aria-label="Wydawka"] tbody md-icon-button')`)
    await answer(q, 'Zapłacono')
    await q.sleep(200)
    const after = { pager: await q.ev(pagerLine), rows: await q.ev(`document.querySelectorAll('tbody tr').length`) }
    check('the last page losing its only row falls back to the page before', last === '10–10 z 10' && after.pager === '1–9 z 9' && after.rows === 9, JSON.stringify({ last, ...after }))

    // the confirming button of a dialog clicked twice at once
    await q.clickOn(`document.querySelector('table[aria-label="Wydawka"] tbody md-icon-button')`)
    await q.waitFor(`document.querySelector('md-dialog[open]') !== null && ${dialogButton('Zapłacono')} !== null`, 3000)
    await q.sleep(150)
    const confirm = await q.ev(`window.__bench.centre(${dialogButton('Zapłacono')})`)
    const twice = xy => ['mouseMoved', 'mousePressed', 'mouseReleased'].map(type => q.send('Input.dispatchMouseEvent', { type, x: xy[0], y: xy[1], button: 'left', clickCount: 1 }))
    await Promise.all([...twice(confirm), ...twice(confirm)])
    await q.sleep(600)
    const once = await q.ev(pagerLine)
    check('a payment confirmed twice at once is taken once', once === '1–8 z 8', JSON.stringify(once))
    problems.push(...q.events.map(e => `${e.kind}: ${e.text}`))
    await q.close()
    return { checks, problems }
  },
  report: ({ checks, problems }) => [
    '### Races: the application driven faster than a person can', '',
    ...checks.map(c => `- ${c.ok ? 'holds' : '**FAILS**'}: ${c.name}${c.ok ? '' : ` — ${c.detail}`}`), '',
    problems.length ? `Console problems: ${[...new Set(problems)].join(' | ')}` : 'No exception and no console warning.', '',
  ].join('\n'),
}
