// B1 — responsiveness: every kind of thing a user does at each station,
// done with real input on the standard day and timed by what the browser
// spent on it: everything its main thread did up to and including the
// frame that shows the change — script, style, layout, and the painting
// and compositing a forced layout would leave out; a frame with nothing
// done is measured beside them as the floor. Three passes on fresh pages: at full
// speed, at a quarter of it (a mid-range phone), and under a CPU profile
// that says which layer the time went to. Then the application itself: a
// second passing at each station, a station switched to, and the start.
import { summary, round, table } from '../stats.mjs'
import { answer, backspace, digit, dishTile, drawn, field, fieldIn, inRow, letter, pagerButton, stationMount, tab, tableTile } from './common.mjs'

const parts = ['Stock', 'Hall', 'Kitchen', 'Pass', 'Report', 'Statistics']

// what a pass does at a station: `rec(name, act)` measures one doing of `act`
const turnPages = async (p, rec, name, reps) => {
  for (let i = 0; i < reps; i++) await rec(name, () => p.clickOn(pagerButton(i % 2 ? 'Poprzednia strona' : 'Następna strona')))
}
const pickGroups = async (p, rec, name, reps) => {
  for (let i = 0; i < reps; i++) await rec(name, () => p.clickOn(tab(i % 2 ? 'Makarony' : 'Zupy')))
  await p.clickOn(tab('Wszystkie'))
}
const search = async (p, rec, name, fieldName, reps) => {
  await p.clickOn(field(fieldName))
  for (let done = 0; done < reps;) {
    for (const ch of 'truf') { await rec(name, () => letter(p, ch)); done++ }
    for (let i = 0; i < 4; i++) { await rec(name, () => backspace(p)); done++ }
  }
}

const walks = {
  Stock: async (p, rec, reps) => {
    for (let i = 0; i < reps; i++) {
      await rec('stock: a portion more', () => p.clickOn(inRow('Magazyn', 'Affogato', 'Przybyła porcja')))
      await rec('stock: a portion fewer', () => p.clickOn(inRow('Magazyn', 'Affogato', 'Ubyła porcja')))
    }
    await p.clickOn(fieldIn('Magazyn', 'Affogato', 'Porcje'))
    for (let i = 0; i < reps; i++) await rec('stock: a key in a portion count', () => i % 2 ? backspace(p) : digit(p, '1'))
    await p.clickOn(fieldIn('Magazyn', 'Affogato', 'Cena'))
    for (let i = 0; i < reps; i++) await rec('stock: a key in a price', () => i % 2 ? backspace(p) : digit(p, '1'))
    await turnPages(p, rec, 'stock: a page turned', reps)
    await pickGroups(p, rec, 'stock: a group picked', reps)
    await search(p, rec, 'stock: a key in the search', 'Szukaj w magazynie', reps)
  },
  Hall: async (p, rec, reps) => {
    await p.clickOn(dishTile('Affogato z gorgonzolą'))
    const dishes = ['Affogato z pomidorami', 'Arancini z orzechami', 'Arancini']
    for (let i = 0; i < reps; i++) await rec('hall: a dish added', () => p.clickOn(dishTile(dishes[i % 3])))
    for (let i = 0; i < reps; i++) {
      await rec('hall: a portion more on a line', () => p.clickOn(inRow('Zamówienie', 'Affogato z gorgonzolą', 'Dodano porcję')))
      await rec('hall: a portion fewer on a line', () => p.clickOn(inRow('Zamówienie', 'Affogato z gorgonzolą', 'Zwrócono porcję')))
    }
    for (let i = 0; i < reps; i++) await rec('hall: a table picked', () => p.clickOn(tableTile(i % 2 ? 5 : 7)))
    await turnPages(p, rec, 'hall: a page turned', reps)
    await pickGroups(p, rec, 'hall: a group picked', reps)
    await search(p, rec, 'hall: a key in the search', 'Szukaj w menu', reps)
  },
  Kitchen: async (p, rec, reps) => {
    await turnPages(p, rec, 'kitchen: a page turned', reps)
    const unplated = `[...document.querySelectorAll('.md3-card md-list-item')].find(i => !i.classList.contains('md3-list-item--selected')) ?? null`
    for (let i = 0; i < reps && await p.ev(`(${unplated}) !== null`); i++) await rec('kitchen: a line plated', () => p.clickOn(unplated))
    const ready = `[...document.querySelectorAll('.md3-card md-filled-button')][0] ?? null`
    for (let i = 0; i < reps && await p.ev(`(${ready}) !== null`); i++) {
      await rec('kitchen: an order passed on, asked', () => p.clickOn(ready))
      await rec('kitchen: an order passed on, confirmed', () => answer(p, 'Zamówienie gotowe'))
    }
  },
  Pass: async (p, rec, reps) => {
    const pay = `document.querySelector('table[aria-label="Wydawka"] tbody md-icon-button')`
    for (let i = 0; i < reps && await p.ev(`(${pay}) !== null`); i++) {
      await rec('pass: a payment, asked', () => p.clickOn(pay))
      await rec('pass: a payment, confirmed', () => answer(p, 'Zapłacono'))
    }
  },
  Report: (p, rec, reps) => turnPages(p, rec, 'report: a page turned', reps),
  Statistics: async (p, rec, reps) => {
    await turnPages(p, rec, 'statistics: a page turned', reps)
    await pickGroups(p, rec, 'statistics: a group picked', reps)
    await search(p, rec, 'statistics: a key in the search', 'Szukaj w statystykach', reps)
  },
}

const stationTabs = ['Magazyn', 'Sala', 'Kuchnia', 'Wydawka', 'Raport', 'Statystyki']

// the application: a second passing at each station, the stations switched between, the start
const application = async (browser, throttle, reps) => {
  const samples = {}
  const push = (name, s) => (samples[name] ??= []).push(s)
  const p = await browser.open('App', { throttle })
  await p.waitFor(drawn.App)
  const nextTick = () => p.nextBeat(1000)
  for (let i = 0; i < Math.min(reps, 6); i++) { await p.sleep(100); push('app: a frame with nothing done', await p.measure(async () => {})) }
  for (const station of stationTabs) {
    await p.clickOn(tab(station))
    await p.sleep(200)
    for (let i = 0; i < Math.min(reps, 6); i++) {
      await nextTick()
      await p.sleep(100)
      push(`app: a second passes at ${station}`, await p.measure(nextTick))
    }
  }
  // the first round is each station's first visit, left out
  for (let i = 0; i < 6 + reps * 4; i++) {
    const station = stationTabs[i % 6]
    const m = await p.measure(() => p.clickOn(tab(station)))
    if (i >= 6) push(`app: switched to ${station}`, m)
  }
  const problems = p.events.map(e => `${e.kind}: ${e.text}`)
  await p.close()
  for (let i = 0; i < Math.min(reps, 6); i++) {
    const q = await browser.open('App', { throttle })
    await q.waitFor(`document.querySelector('md-linear-progress') !== null`)
    const waiting = await q.metrics()
    await q.waitFor(drawn.App)
    await q.frame()
    const open = await q.metrics()
    const a = q.spent(waiting), b = q.spent(open)
    push('app: started, until it waits for its day', a)
    push('app: started, the day drawn', { total: b.total - a.total, script: b.script - a.script, style: b.style - a.style, layout: b.layout - a.layout, rest: Math.max(0, b.rest - a.rest), nodes: open.nodes })
    await q.close()
  }
  return { samples, problems }
}

export const responsiveness = {
  name: 'responsiveness',
  mounts: () => [...parts.map(part => stationMount(part)), { name: 'App', kind: 'app' }],
  run: async ({ quick, browser, log }) => {
    const reps = quick ? 8 : 30
    const passes = {}
    const problems = []
    for (const throttle of [1, 4]) {
      const samples = {}
      for (const part of parts) {
        log(`  ${part} at ${throttle}×`)
        const p = await browser.open(stationMount(part).name, { throttle })
        await p.waitFor(drawn[part])
        await p.sleep(300)
        const rec = async (name, act) => { (samples[name] ??= []).push(await p.measure(act)) }
        await walks[part](p, rec, reps)
        problems.push(...p.events.map(e => `${part}: ${e.kind}: ${e.text}`))
        await p.close()
      }
      log(`  the application at ${throttle}×`)
      const app = await application(browser, throttle, reps)
      Object.assign(samples, app.samples)
      problems.push(...app.problems)
      passes[throttle] = Object.fromEntries(Object.entries(samples).map(([name, xs]) => [name, {
        total: summary(xs.map(x => x.total)),
        script: round(median(xs.map(x => x.script))), style: round(median(xs.map(x => x.style))), layout: round(median(xs.map(x => x.layout))), rest: round(median(xs.map(x => x.rest))),
      }]))
    }
    // the profiled pass: fewer doings, one profile per station
    const layers = {}
    for (const part of parts) {
      log(`  ${part} under a profile`)
      const p = await browser.open(stationMount(part).name)
      await p.waitFor(drawn[part])
      await p.sleep(300)
      await p.startProfile()
      await walks[part](p, async (_, act) => { await act(); await p.frame() }, quick ? 4 : 10)
      layers[part] = await p.stopProfile()
      await p.close()
    }
    return { reps, passes, layers, problems }
  },
  report: ({ passes, layers, problems }) => {
    const names = Object.keys(passes[1])
    const budget = name => name.includes('a second passes') || name.includes('nothing done') ? 16 : name.startsWith('app: started') ? 1000 : 100
    const rows = names.map(name => {
      const a = passes[1][name], b = passes[4][name]
      const over = b.total.p95 > budget(name)
      return [name, a.total.n, a.total.p50, a.total.p95, `${a.script} / ${a.style} / ${a.layout} / ${a.rest}`, b.total.p50, b.total.p95, over ? `over ${budget(name)}` : 'within']
    })
    const out = ['### Responsiveness: what one doing costs the browser (ms)', '',
      'Everything the main thread does from the doing to the frame that shows it; the split is the median at full speed, the rest being painting, compositing and the browser\'s own — a frame with nothing done is its floor. The budget is on the 95th percentile at a quarter of the speed: 100 ms for something done, 16 ms for a second passing, 1 s for the start.', '',
      table(['done', 'n', 'p50', 'p95', 'script / style / layout / rest', 'p50 at ¼', 'p95 at ¼', 'budget'], rows), '']
    const share = Object.entries(layers).map(([part, { layers: l }]) => {
      const sum = Object.values(l).reduce((a, b) => a + b, 0)
      const pct = k => `${round(100 * (l[k] ?? 0) / sum, 0)}%`
      return [part, round(sum, 0), pct('view model'), pct('carrier'), pct('design system'), pct('browser'), pct('garbage collector')]
    })
    out.push('Where the time of a station\'s doings goes (CPU profile, self time by layer):', '',
      table(['station', 'ms profiled', 'view model', 'carrier', 'design system', 'browser', 'gc'], share), '')
    for (const [part, { top }] of Object.entries(layers))
      out.push(`${part}, the functions that took most: ${top.slice(0, 5).map(t => `${t.fn}${t.module ? ` (${t.module})` : ''} ${t.ms} ms`).join('; ')}.`)
    out.push('', problems.length ? `Console problems: ${[...new Set(problems)].join(' | ')}` : 'No exception and no console warning.', '')
    return out.join('\n')
  },
}

const median = xs => [...xs].sort((a, b) => a - b)[xs.length >> 1]
