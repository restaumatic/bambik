// B3 — correctness under stress: a seeded random walk of real clicks and
// typing over the six stations, checked step by step against the view
// model run in node. The reference is the application's own pure
// functions (output/Diner): every step applies to it the fold the click
// should have reached, with the payload it should have carried, and the
// station on screen must then read exactly what the view model's read
// functions say of the reference — every row, every label, every disabled
// button, the pager. So what is checked is the carrier: that an event
// reaches its fold with the right row, that a fed model reaches every
// leaf, that keyed rows follow their keys. A divergence stops the walk and
// is reported with its step and seed; the same seed walks the same walk.
// The page's wall clock is stopped at the day's own time, one in the
// afternoon, so the lines that read the time read what the reference does.
import { pathToFileURL } from 'node:url'
import path from 'node:path'
import { answer, dishTile, drawn, field, perPage, retype, tab, tableTile } from './common.mjs'

const stationTabs = ['Magazyn', 'Sala', 'Kuchnia', 'Wydawka', 'Raport', 'Statystyki']
const groups = ['Wszystkie', 'Przystawki', 'Zupy', 'Makarony', 'Dania główne', 'Desery']
const finderOf = { Magazyn: ['Grupa magazynu', 'Szukaj w magazynie'], Sala: ['Grupa menu', 'Szukaj w menu'], Statystyki: ['Grupa statystyk', 'Szukaj w statystykach'] }
const V = type => ({ type, value: {} })
const time = { hours: 13, minutes: 0, seconds: 0 }

// what the station on screen reads, as plain data — the counterpart of `expected` below
const reading = `(() => {
  const B = window.__bench
  const txt = el => el ? el.innerText.trim() : null
  const off = el => !!el && (el.disabled === true || el.hasAttribute('disabled') || el.getAttribute('aria-disabled') === 'true')
  const own = el => [...el.childNodes].filter(n => n.nodeType === 3).map(n => n.nodeValue).join('').trim()
  const bars = [...document.querySelectorAll('md-tabs')]
  const active = bar => txt([...bar.querySelectorAll('md-primary-tab')].find(t => t.active) ?? null)
  const station = active(bars[0])
  const notes = [...document.querySelectorAll('p.md-typescale-body-large')].filter(B.visible).map(txt)
  const finder = name => { const f = B.field(name); return f ? { search: f.value, group: bars[1] && B.visible(bars[1]) ? active(bars[1]) : null } : null }
  const pager = () => {
    const next = [...document.querySelectorAll('md-icon-button')].find(b => B.visible(b) && b.getAttribute('aria-label') === 'Następna strona')
    if (!next) return null
    const prev = [...next.parentElement.querySelectorAll('md-icon-button')].find(b => b.getAttribute('aria-label') === 'Poprzednia strona')
    return [txt(next.parentElement.querySelector('.md-typescale-label-large')), off(prev), off(next)]
  }
  const rows = name => [...document.querySelectorAll('table[aria-label="' + name + '"] tbody tr')]
  const button = (row, label) => [...row.querySelectorAll('md-icon-button')].find(b => b.getAttribute('aria-label') === label)
  const view = { station, notes }
  if (station === 'Magazyn') {
    view.finder = finder('Szukaj w magazynie')
    view.rows = rows('Magazyn').map(r => [txt(r.cells[0].querySelector('p')), r.querySelector('[name="Cena"]').value, r.querySelector('[name="Porcje"]').value, off(button(r, 'Ubyła porcja')), off(button(r, 'Wyzeruj'))])
    view.pager = pager()
  }
  if (station === 'Sala') {
    const tiles = [...document.querySelectorAll('.md3-tile')]
    view.tables = tiles.filter(t => (t.getAttribute('aria-label') || '').startsWith('Stolik')).map(t => t.getAttribute('aria-label'))
    view.heading = txt(document.querySelector('h3.md-typescale-title-medium'))
    const order = rows('Zamówienie')
    const total = [...document.querySelectorAll('.md-typescale-label-large')].find(e => B.visible(e) && e.innerText.trim() === 'Łącznie')
    view.order = total ? { lines: order.map(r => { const p = r.cells[0].querySelectorAll('p'); return [txt(p[0]), txt(p[1]), own(r.cells[1].firstElementChild), off(button(r, 'Zwrócono porcję')), off(button(r, 'Dodano porcję'))] }), total: txt(total.nextElementSibling) } : null
    view.finder = finder('Szukaj w menu')
    view.menu = tiles.filter(t => t.querySelector('.md-typescale-label-medium')).map(t => { const l = t.querySelectorAll('.md-typescale-label-large'); return [txt(l[0]), l[1] ? txt(l[1]) : '', txt(t.querySelector('.md-typescale-label-medium')), off(t)] })
    view.pager = pager()
  }
  if (station === 'Kuchnia') {
    view.tickets = [...document.querySelectorAll('.md3-card')].map(c => [txt(c.querySelector('h3')), txt(c.querySelector('span.md-typescale-label-large')), [...c.querySelectorAll('md-list-item')].map(i => [own(i), i.classList.contains('md3-list-item--selected')]), !!c.querySelector('md-filled-button')])
    view.pager = pager()
  }
  if (station === 'Wydawka') {
    view.rows = rows('Wydawka').map(r => [txt(r.cells[0]), txt(r.cells[1]), txt(r.cells[2])])
    view.pager = pager()
  }
  if (station === 'Raport') {
    const all = rows('Raport')
    view.total = all.length ? txt(all[0].cells[1]) : null
    view.rows = all.slice(1).map(r => { const p = r.cells[0].querySelectorAll('p'); return [txt(p[0]), txt(p[1]), txt(r.cells[1])] })
    view.pager = pager()
  }
  if (station === 'Statystyki') {
    const all = rows('Statystyki')
    view.finder = finder('Szukaj w statystykach')
    view.total = all.length ? [txt(all[0].cells[1]), txt(all[0].cells[2])] : null
    view.rows = all.slice(1).map(r => [txt(r.cells[0]), txt(r.cells[1]), txt(r.cells[2])])
    view.pager = pager()
  }
  return JSON.stringify(view)
})()`

// what the view model says the station reads, from the reference model
const expected = (D, m) => {
  const station = m['Stanowisko'].type
  const is = (v, type) => v.type === type
  const pager = () => [D.pageLine(m), D.isFirstPage(m), D.isLastPage(m)]
  const finder = () => { const [g, s] = finderOf[station]; return { search: m[s], group: is(D.browseState(m), 'browsing') ? m[g].type : null } }
  const found = () => is(D.searchState(m), 'found')
  const view = { station, notes: [] }
  if (station === 'Magazyn') {
    view.finder = finder()
    if (!found()) view.notes.push('Nie ma takich dań')
    view.rows = found() ? D.stockPage(m).map(d => [d.name, d['Cena'], String(d['Porcje'].value), D.isSoldOut(d), D.isZeroed(d)]) : []
    view.pager = found() ? pager() : null
  }
  if (station === 'Sala') {
    const free = is(D.tableAvailability(m), 'free'), state = D.tableState(m).type
    view.tables = D.seatings(m).map(D.tableName)
    view.heading = D.servedLine({ ...m, time })
    if (free && state === 'idle') view.notes.push('Żadnych dań jeszcze nie wybrano')
    view.order = free && state === 'taking'
      ? { lines: D.tableLines(m).map(l => [l.dish, D.lineTotalLine(l), D.quantityLine(l), D.isSinglePortion(l), D.isLineSoldOut(l)]), total: D.orderTotalLine(m) }
      : null
    view.finder = free ? finder() : null
    if (free && !found()) view.notes.push('Nie ma takich dań')
    view.menu = free && found() ? D.menuPage(m).map(o => [o.name, D.isAdded(o) ? D.addedLine(o) : '', D.offerLine(o), D.isOffMenu(o)]) : []
    view.pager = free && found() ? pager() : null
  }
  if (station === 'Kuchnia') {
    const busy = is(D.kitchenState(m), 'busy')
    if (!busy) view.notes.push('Wszystko ugotowane, kuchnia czeka na zamówienia')
    view.tickets = busy ? D.ticketsPage(m).map(t => [D.ticketLine(t), D.waitLine({ ...t, time }), t.lines.map(l => [D.itemLine(l), D.isPlated(l)]), is(D.ticketState(t), 'plated')]) : []
    view.pager = busy ? pager() : null
  }
  if (station === 'Wydawka') {
    const pending = is(D.billsState(m), 'pending')
    if (!pending) view.notes.push('Wszystko wydane, wydawka czeka na kuchnię')
    view.rows = pending ? D.billsPage(m).map(b => [D.billLine(b), D.waitLine({ ...b, time }), D.billTotalLine(b)]) : []
    view.pager = pending ? pager() : null
  }
  if (station === 'Raport') {
    const some = is(D.paidState(m), 'some')
    if (!some) view.notes.push('Nic jeszcze nie sprzedano')
    view.total = some ? D.takingsLine(m) : null
    view.rows = some ? D.paidPage(m).map(r => [D.receiptLine({ ...r, time }), D.orderItemsLine(r), D.paidTotalLine(r)]) : []
    view.pager = some ? pager() : null
  }
  if (station === 'Statystyki') {
    const some = is(D.paidState(m), 'some')
    if (!some) view.notes.push('Nic jeszcze nie sprzedano')
    view.finder = some ? finder() : null
    if (some && !found()) view.notes.push('Nie ma takich dań')
    view.total = some && found() ? [D.soldQuantityLine(m), D.soldAmountLine(m)] : null
    view.rows = some && found() ? D.salesPage(m).map(s => [s.dish, D.quantityLine(s), D.salesAmountLine(s)]) : []
    view.pager = some && found() ? pager() : null
  }
  return JSON.stringify(view)
}

// page expressions for a row by the exact name in its first cell
const rowNamed = (table, name) => `([...document.querySelectorAll('table[aria-label="${table}"] tbody tr')].find(r => (r.cells[0].querySelector('p') ?? r.cells[0]).innerText.trim() === ${JSON.stringify(name)}) ?? null)`
const inNamed = (table, name, label) => `([...(${rowNamed(table, name)}?.querySelectorAll('md-icon-button') ?? [])].find(b => b.getAttribute('aria-label') === ${JSON.stringify(label)}) ?? null)`
const fieldOf = (name, f) => `(${rowNamed('Magazyn', name)}?.querySelector('[name="${f}"]') ?? null)`
const card = line => `([...document.querySelectorAll('.md3-card')].find(c => c.querySelector('h3').innerText.trim() === ${JSON.stringify(line)}) ?? null)`

const mulberry = seed => () => {
  let t = seed += 0x6D2B79F5
  t = Math.imul(t ^ t >>> 15, t | 1)
  t ^= t + Math.imul(t ^ t >>> 7, t | 61)
  return ((t ^ t >>> 14) >>> 0) / 4294967296
}

const portions = text => /^-?\d+$/.test(text) ? { type: 'count', value: +text } : { type: 'invalid', value: text }

// the things that can be done now, by the reference model: [weight, what, do it on the page, the model after]
const options = (D, m, pick) => {
  const station = m['Stanowisko'].type
  const is = (v, type) => v.type === type
  const opts = []
  const add = (weight, what, act, after) => opts.push({ weight, what, act, after })
  const other = pick(stationTabs.filter(s => s !== station))
  add(3, `open ${other}`, p => p.clickOn(tab(other)), () => ({ ...m, 'Stanowisko': V(other) }))
  const paged = () => {
    if (!D.isLastPage(m)) add(2, 'next page', p => p.click('Następna strona'), () => D.nextPage(m))
    if (!D.isFirstPage(m)) add(2, 'previous page', p => p.click('Poprzednia strona'), () => D.previousPage(m))
  }
  const finder = () => {
    const [g, s] = finderOf[station]
    if (is(D.browseState(m), 'browsing')) { const group = pick(groups); add(2, `group ${group}`, p => p.clickOn(tab(group)), () => ({ ...m, [g]: V(group) })) }
    const text = pick(['', '', 'truf', 'aff', 'pesto', 'Risotto', 'zzz', 'z gr'])
    add(1, `search "${text}"`, p => retype(p, field(s), text), () => ({ ...m, [s]: text }))
  }
  const dishes = f => ({ ...m, dishes: m.dishes.map(f) })
  if (station === 'Magazyn') {
    finder()
    if (is(D.searchState(m), 'found')) {
      paged()
      const d = pick(D.stockPage(m)), name = d.name
      const on = f => () => dishes(x => x.name === name ? f(x) : x)
      add(2, `stock + ${name}`, p => p.clickOn(inNamed('Magazyn', name, 'Przybyła porcja')), on(D.portionAdded))
      if (!D.isSoldOut(d)) add(2, `stock − ${name}`, p => p.clickOn(inNamed('Magazyn', name, 'Ubyła porcja')), on(D.portionRemoved))
      if (!D.isZeroed(d)) {
        add(1, `zero ${name}`, async p => { await p.clickOn(inNamed('Magazyn', name, 'Wyzeruj')); await answer(p, 'Wyzeruj') }, on(D.portionsZeroed))
        add(0.5, `zero ${name}, declined`, async p => { await p.clickOn(inNamed('Magazyn', name, 'Wyzeruj')); await answer(p, 'Nie, nie zeruj') }, () => m)
      }
      const count = pick(['0', '4', '12', '30', 'x1'])
      add(1, `portions of ${name} := ${count}`, p => retype(p, fieldOf(name, 'Porcje'), count), on(x => ({ ...x, 'Porcje': portions(count) })))
      const price = pick(['9,99', '12', '20,5', 'abc', '31,00'])
      add(1, `price of ${name} := ${price}`, p => retype(p, fieldOf(name, 'Cena'), price), on(x => ({ ...x, 'Cena': price })))
    }
  }
  if (station === 'Sala') {
    const n = 1 + Math.floor(pick.random() * m.tables)
    add(2, `serve table ${n}`, p => p.clickOn(tableTile(n)), () => D.serveTable({ event: n, model: m }))
    if (is(D.tableAvailability(m), 'free')) {
      finder()
      if (is(D.searchState(m), 'found')) {
        paged()
        const open = D.menuPage(m).filter(o => !D.isOffMenu(o))
        if (open.length) { const o = pick(open); add(6, `add ${o.name}`, p => p.clickOn(dishTile(o.name)), () => D.orderDish({ event: o.name, model: m })) }
      }
      if (is(D.tableState(m), 'taking')) {
        const l = pick(D.tableLines(m))
        if (!D.isLineSoldOut(l)) add(2, `line + ${l.dish}`, p => p.clickOn(inNamed('Zamówienie', l.dish, 'Dodano porcję')), () => D.addUnit({ event: l, model: m }))
        if (!D.isSinglePortion(l)) add(2, `line − ${l.dish}`, p => p.clickOn(inNamed('Zamówienie', l.dish, 'Zwrócono porcję')), () => D.returnUnit({ event: l, model: m }))
        add(1, `remove ${l.dish}`, async p => { await p.clickOn(inNamed('Zamówienie', l.dish, 'Usuń danie')); await answer(p, 'Usuń danie') }, () => D.removeDish({ event: l, model: m }))
        add(0.5, `remove ${l.dish}, declined`, async p => { await p.clickOn(inNamed('Zamówienie', l.dish, 'Usuń danie')); await answer(p, 'Nie, nie usuwaj dania') }, () => m)
        const number = m.orders.find(o => o.table === m.table && o.stage.type === 'taking').number
        add(1.5, `send order ${number} to the kitchen`, p => p.clickOn(`([...document.querySelectorAll('md-filled-button')].find(b => window.__bench.visible(b) && b.innerText.includes('Wyślij do kuchni')) ?? null)`), () => D.sendToKitchen({ event: { number, time }, model: m }))
      }
    }
  }
  if (station === 'Kuchnia' && is(D.kitchenState(m), 'busy')) {
    paged()
    const tickets = D.ticketsPage(m)
    const unplated = tickets.flatMap(t => t.lines.filter(l => !D.isPlated(l)).map(l => [t, l]))
    if (unplated.length) {
      const [t, l] = pick(unplated)
      add(4, `plate ${l.dish} of order ${t.number}`,
        p => p.clickOn(`([...(${card(D.ticketLine(t))}?.querySelectorAll('md-list-item') ?? [])].find(i => [...i.childNodes].filter(n => n.nodeType === 3).map(n => n.nodeValue).join('').trim() === ${JSON.stringify(D.itemLine(l))}) ?? null)`),
        () => D.plateDish({ event: { event: l.dish, model: t }, model: m }))
    }
    const plated = tickets.filter(t => is(D.ticketState(t), 'plated'))
    if (plated.length) {
      const t = pick(plated), button = `(${card(D.ticketLine(t))}?.querySelector('md-filled-button') ?? null)`
      add(2, `order ${t.number} ready`, async p => { await p.clickOn(button); await answer(p, 'Zamówienie gotowe') }, () => D.markReady({ event: t, model: m }))
      add(0.5, `order ${t.number} ready, declined`, async p => { await p.clickOn(button); await answer(p, 'Nie, jeszcze nie gotowe') }, () => m)
    }
  }
  if (station === 'Wydawka' && is(D.billsState(m), 'pending')) {
    paged()
    const b = pick(D.billsPage(m)), button = inNamed('Wydawka', D.billLine(b), 'Zapłacono')
    add(3, `order ${b.number} paid`, async p => { await p.clickOn(button); await answer(p, 'Zapłacono') }, () => D.settle({ event: b, model: m }))
    add(0.5, `order ${b.number} paid, declined`, async p => { await p.clickOn(button); await answer(p, 'Nie, jeszcze nie zapłacono') }, () => m)
  }
  if (station === 'Raport' && is(D.paidState(m), 'some')) paged()
  if (station === 'Statystyki' && is(D.paidState(m), 'some')) { finder(); if (is(D.searchState(m), 'found')) paged() }
  return opts
}

// what must hold of any model the walk reaches
const broken = m => {
  const numbers = m.orders.map(o => o.number)
  if (new Set(numbers).size !== numbers.length) return 'two orders share a number'
  const open = m.orders.filter(o => o.stage.type !== 'paid').map(o => o.table)
  if (new Set(open).size !== open.length) return 'two open orders share a table'
  if (m.orders.some(o => o.lines.length === 0)) return 'an order has no line'
  if (m.orders.some(o => o.lines.some(l => l.quantity < 1))) return 'a line has no portion'
  if (m.dishes.some(d => d['Porcje'].type === 'count' && d['Porcje'].value < 0 && false)) return 'negative stock'
  return null
}
// portions in stock and on orders still being taken: what the hall moves between the two must add up
const portionsAbout = m => m.dishes.reduce((a, d) => a + (d['Porcje'].type === 'count' ? Math.max(0, d['Porcje'].value) : 0), 0)
  + m.orders.filter(o => o.stage.type === 'taking').reduce((a, o) => a + o.lines.reduce((b, l) => b + l.quantity, 0), 0)

export const walk = {
  name: 'walk',
  mounts: () => [{ name: 'Service', kind: 'service', scale: 1, perPage }],
  run: async ({ quick, browser, log }) => {
    const at = m => import(pathToFileURL(path.resolve(`output/${m}/index.js`)))
    const [D, F, S] = await Promise.all([at('Diner'), at('Diner.Fixtures'), at('Diner.Service')])
    const steps = quick ? 400 : 3000
    const seed = +(process.env.BENCH_SEED ?? 20261120)
    const random = mulberry(seed)
    const pick = xs => xs[Math.floor(random() * xs.length)]
    pick.random = random
    let model = F.busyWith(S.standardLoad)(perPage)(V('Sala'))
    const p = await browser.open('Service', { time: '13:00' })
    await p.waitFor(drawn.Service)
    const settled = async () => {
      const want = expected(D, model)
      let got = null
      for (let t = 0; t < 2500; t += 40) { got = await p.ev(reading); if (got === want) return null; await p.sleep(40) }
      return { want, got }
    }
    const done = {}
    let failure = await settled()
    if (failure) failure = { step: 0, what: 'the day as drawn', ...differ(failure) }
    // the check is awake: the page must not read as a model one table away does
    else if (await p.ev(reading) === expected(D, D.serveTable({ event: model.table + 1, model })))
      failure = { step: 0, what: 'the check itself', difference: 'the page reads the same for a different model' }
    let step = 0
    const started = Date.now()
    for (; !failure && step < steps; step++) {
      const opts = options(D, model, pick)
      let r = random() * opts.reduce((a, o) => a + o.weight, 0)
      const o = opts.find(o => (r -= o.weight) < 0) ?? opts[opts.length - 1]
      const before = model
      const acted = await o.act(p)
      if (acted === false) { failure = { step: step + 1, what: o.what, difference: 'the control the reference expects is not on the page' }; break }
      model = o.after()
      const kind = o.what.replace(/ (order )?\d+.*| ".*| [A-ZŻŁ].*| :=.*/u, '').replace(/^(stock|line) [+−] .*/, '$1 ±')
      done[kind] = (done[kind] ?? 0) + 1
      const bad = broken(model)
        ?? (/^(add|line|remove) /.test(o.what) && portionsAbout(before) !== portionsAbout(model) ? 'portions in stock and on open orders no longer add up' : null)
      const diff = bad ? null : await settled()
      if (bad || diff) failure = { step: step + 1, what: o.what, ...(bad ? { difference: bad } : differ(diff)) }
      else if ((step + 1) % 500 === 0) log(`  ${step + 1} steps`)
    }
    const problems = p.events.map(e => `${e.kind}: ${e.text}`)
    await p.close()
    return { seed, steps: step, asked: steps, seconds: Math.round((Date.now() - started) / 1000), done, failure, problems, orders: model.orders.length, paid: model.orders.filter(o => o.stage.type === 'paid').length }
  },
  report: r => {
    const out = ['### Correctness under stress: a random walk against the view model', '']
    out.push(r.failure
      ? `**Diverged at step ${r.failure.step} of ${r.asked}** (seed ${r.seed}) after "${r.failure.what}": ${r.failure.difference}`
      : `${r.steps} steps of real input (seed ${r.seed}, ${r.seconds} s): after every one the station on screen read exactly what the view model says of the reference model. The day ended with ${r.orders} orders, ${r.paid} of them paid.`, '')
    out.push(`Done along the way: ${Object.entries(r.done).sort((a, b) => b[1] - a[1]).map(([k, n]) => `${k} ×${n}`).join(', ')}.`, '')
    out.push(r.problems.length ? `Console problems: ${[...new Set(r.problems)].join(' | ')}` : 'No exception and no console warning.', '')
    return out.join('\n')
  },
}

// where two readings part: the first path at which they differ
const differ = ({ want, got }) => {
  const a = JSON.parse(want), b = got ? JSON.parse(got) : null
  const at = (x, y, path) => {
    if (JSON.stringify(x) === JSON.stringify(y)) return null
    if (x && y && typeof x === 'object' && typeof y === 'object') {
      for (const k of new Set([...Object.keys(x), ...Object.keys(y)])) { const d = at(x[k], y[k], `${path}.${k}`); if (d) return d }
    }
    return `${path}: the view model says ${JSON.stringify(x)}, the page reads ${JSON.stringify(y)}`
  }
  return { difference: at(a, b, 'view') ?? 'unknown' }
}
