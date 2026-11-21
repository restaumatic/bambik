// B4 — reliability: the application as it starts, left running under its
// clock while random things are done to it, sampled after a forced
// collection for what must not grow — heap, DOM nodes, listeners. Then the
// stations cycled through hundreds of times against a baseline, and the
// clock itself: how many of its seconds pass in a second, and what a frozen
// page costs it.
import { drawn, pagerButton, stationMount, tab, tableTile } from './common.mjs'
import { line, round, table } from '../stats.mjs'

const stationTabs = ['Magazyn', 'Sala', 'Kuchnia', 'Wydawka', 'Raport', 'Statystyki']

// everything on the page that can be clicked without a dialog opening
const clickable = `(() => {
  const B = window.__bench
  const asks = ['Usuń danie', 'Wyzeruj', 'Zapłacono']
  return [
    ...[...document.querySelectorAll('md-primary-tab')],
    ...[...document.querySelectorAll('.md3-tile')].filter(t => t.getAttribute('aria-disabled') !== 'true'),
    ...[...document.querySelectorAll('md-icon-button')].filter(b => !b.disabled && !b.hasAttribute('disabled') && !asks.includes(b.getAttribute('aria-label'))),
    ...[...document.querySelectorAll('.md3-card md-list-item')],
  ].filter(B.visible)
})()`

const mulberry = seed => () => {
  let t = seed += 0x6D2B79F5
  t = Math.imul(t ^ t >>> 15, t | 1)
  t ^= t + Math.imul(t ^ t >>> 7, t | 61)
  return ((t ^ t >>> 14) >>> 0) / 4294967296
}

// what one doing leaves behind for good: twenty of them between two forced
// collections, on a station alone with no clock
const leftBehind = async (browser, part, act) => {
  const p = await browser.open(stationMount(part).name)
  await p.waitFor(drawn[part])
  await p.sleep(300)
  for (let i = 0; i < 4; i++) await act(p, i)
  await p.gc()
  const a = await p.metrics()
  for (let i = 0; i < 20; i++) await act(p, i)
  await p.gc(); await p.sleep(200); await p.gc()
  const b = await p.metrics()
  await p.close()
  return { nodes: round((b.nodes - a.nodes) / 20, 1), listeners: round((b.listeners - a.listeners) / 20, 1), heapKB: round((b.heap - a.heap) / 1024 / 20, 1) }
}
const turn = (p, i) => p.clickOn(pagerButton(i % 2 ? 'Poprzednia strona' : 'Następna strona'))
const doings = [
  ['Stock', 'stockroom: a page turned', turn],
  ['Hall', 'hall: a page turned', turn],
  ['Hall', 'hall: a group picked', (p, i) => p.clickOn(tab(i % 2 ? 'Makarony' : 'Zupy'))],
  ['Hall', 'hall: a table picked', (p, i) => p.clickOn(tableTile(i % 2 ? 5 : 7))],
  ['Kitchen', 'kitchen: a page turned', turn],
  ['Report', 'report: a page turned', turn],
  ['Statistics', 'statistics: a page turned', turn],
]

const sample = async (p, t) => { await p.gc(); const m = await p.metrics(); return { t, heapMB: m.heap / 2 ** 20, nodes: m.nodes, listeners: m.listeners } }

export const soak = {
  name: 'soak',
  mounts: () => [{ name: 'App', kind: 'app' }, ...['Stock', 'Hall', 'Kitchen', 'Report', 'Statistics'].map(part => stationMount(part))],
  run: async ({ quick, browser, log }) => {
    const seconds = quick ? 40 : 240, cycles = quick ? 30 : 150, watched = quick ? 12 : 60
    const random = mulberry(7)

    // random input under the clock
    let p = await browser.open('App')
    await p.waitFor(drawn.App)
    const started = Date.now()
    const samples = []
    let actions = 0
    while (Date.now() - started < seconds * 1000) {
      const n = await p.ev(`${clickable}.length`)
      if (n) await p.clickOn(`${clickable}[${Math.floor(random() * n)}]`)
      actions++
      if (actions % 25 === 0) samples.push(await sample(p, (Date.now() - started) / 1000))
    }
    const problems = p.events.map(e => `${e.kind}: ${e.text}`)
    await p.close()
    log(`  ${actions} actions in ${seconds} s`)

    // the stations cycled through
    p = await browser.open('App')
    await p.waitFor(drawn.App)
    for (let i = 0; i < 12; i++) await p.clickOn(tab(stationTabs[i % 6]))
    const before = await sample(p, 0)
    for (let i = 0; i < cycles * 6; i++) await p.clickOn(tab(stationTabs[i % 6]))
    const after = await sample(p, 0)
    problems.push(...p.events.map(e => `${e.kind}: ${e.text}`))
    await p.close()
    log(`  ${cycles * 6} station switches`)

    // the clock: its seconds against the wall's, then across a frozen page
    p = await browser.open('App')
    await p.waitFor(drawn.App)
    const ticks = () => p.ev('window.__bench.fired[1000] || 0')
    await p.waitFor(`(window.__bench.fired[1000] || 0) > 0`, 5000)
    await p.sleep(watched * 1000)
    // its seconds per second, from the first tick to the last
    const rate = await p.ev(`(B => (B.fired[1000] - 1) / ((B.lastAt[1000] - B.firstAt[1000]) / 1000))(window.__bench)`)
    let frozen = null
    try {
      const a = await ticks()
      const r = await p.send('Page.setWebLifecycleState', { state: 'frozen' })
      if (r.error) throw new Error(r.error.message)
      await p.sleep(6000)
      await p.send('Page.setWebLifecycleState', { state: 'active' })
      await p.sleep(1200)
      frozen = { seconds: 7.2, ticks: await ticks() - a }
    } catch (e) { frozen = { skipped: String(e.message ?? e) } }
    await p.close()
    const left = {}
    for (const [part, name, act] of doings) left[name] = await leftBehind(browser, part, act)
    return { seconds, actions, samples, cycles, before, after, left, clock: { watched, rate: round(rate, 4), frozen }, problems }
  },
  report: r => {
    const half = r.samples.slice(Math.floor(r.samples.length / 2))
    const perMinute = k => round(line(half.map(s => s.t / 60), half.map(s => s[k])).slope, 2)
    const first = r.samples[0], last = r.samples[r.samples.length - 1]
    const out = ['### Reliability: left running', '',
      `${r.actions} random clicks over ${r.seconds} s under the clock, sampled every 25 after a forced collection. The slope is over the second half of the run:`, '',
      table(['', 'first sample', 'last sample', 'slope per minute'], [
        ['heap (MB)', round(first.heapMB, 1), round(last.heapMB, 1), perMinute('heapMB')],
        ['DOM nodes', first.nodes, last.nodes, perMinute('nodes')],
        ['listeners', first.listeners, last.listeners, perMinute('listeners')],
      ]), '',
      'What one doing leaves behind for good (twenty of them between two forced collections, a station alone):', '',
      table(['done', 'DOM nodes', 'listeners', 'heap (KB)'], Object.entries(r.left).map(([name, l]) => [name, l.nodes, l.listeners, l.heapKB])), '',
      `${r.cycles * 6} station switches after a warm-up of twelve: heap ${round(r.before.heapMB, 1)} → ${round(r.after.heapMB, 1)} MB, DOM nodes ${r.before.nodes} → ${r.after.nodes}, listeners ${r.before.listeners} → ${r.after.listeners}.`, '',
      `The clock, watched for ${r.clock.watched} s with the hall open: ${r.clock.rate} of its seconds pass in a second (each beat waits a full second after the one before has been handled).`
        + (r.clock.frozen?.skipped ? ` (The frozen-page reading was skipped: ${r.clock.frozen.skipped}.)` : ` Across ${r.clock.frozen.seconds} s with the page frozen for 6 of them it counted ${r.clock.frozen.ticks}.`), '',
      r.problems.length ? `Console problems: ${[...new Set(r.problems)].join(' | ')}` : 'No exception and no console warning.', '']
    return out.join('\n')
  },
}
