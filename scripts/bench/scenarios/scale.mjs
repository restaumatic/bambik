// B2 — large data: each station alone on days of 1× to 64× the standard
// load (64× is 19 456 dishes, 2 688 tables, 16 128 orders), measured three
// ways — drawing the day, a second passing (the whole model fed again with
// nothing but the time changed), and one thing done. Pagination keeps what
// is on screen the same at every scale, so what grows is what the view
// model and the carrier spend on rows that are not shown. Then the same
// with pagination off, where everything is on screen: the cost of a row.
import { exponent, round, summary, table } from '../stats.mjs'
import { answer, drawn, inRow, pagerButton, stationMount } from './common.mjs'

const parts = ['Stock', 'Hall', 'Kitchen', 'Pass', 'Report', 'Statistics']
const everything = { list: 1000000, tiles: 1000000 }
const period = 251
const scalesOf = quick => quick ? [1, 4, 16] : [1, 4, 16, 64]
const openScalesOf = quick => quick ? [1] : [1, 4]

const clocked = (part, scale) => stationMount(part, scale, { suffix: 'Clock', clock: period })
const open = (part, scale) => stationMount(part, scale, { suffix: 'All', perPage: everything })
const openClocked = (part, scale) => stationMount(part, scale, { suffix: 'AllClock', perPage: everything, clock: period })

// what is listed at a station: its rows, tiles or tickets
const listed = {
  Stock: `document.querySelectorAll('table[aria-label="Magazyn"] tbody tr').length`,
  Hall: `document.querySelectorAll('.md3-tile').length`,
  Kitchen: `document.querySelectorAll('.md3-card').length`,
  Pass: `document.querySelectorAll('tbody tr').length`,
  Report: `document.querySelectorAll('tbody tr').length`,
  Statistics: `document.querySelectorAll('tbody tr').length`,
}

// the one thing done at a station, repeatable
const firstOpenTile = `([...document.querySelectorAll('.md3-tile')].find(t => t.querySelector('.md-typescale-label-large') && t.getAttribute('aria-disabled') !== 'true') ?? null)`
const doings = {
  Stock: ['a portion more', async (p, i) => p.clickOn(inRow('Magazyn', 'Affogato', i % 2 ? 'Ubyła porcja' : 'Przybyła porcja'))],
  Hall: ['a dish added', async p => p.clickOn(firstOpenTile)],
  Kitchen: ['a page turned', async (p, i) => p.clickOn(pagerButton(i % 2 ? 'Poprzednia strona' : 'Następna strona'))],
  Pass: ['a payment taken', async p => { await p.clickOn(`document.querySelector('table[aria-label="Wydawka"] tbody md-icon-button')`); await answer(p, 'Zapłacono') }],
  Report: ['a page turned', async (p, i) => p.clickOn(pagerButton(i % 2 ? 'Poprzednia strona' : 'Następna strona'))],
  Statistics: ['a page turned', async (p, i) => p.clickOn(pagerButton(i % 2 ? 'Poprzednia strona' : 'Następna strona'))],
}

const cost = m => m.script + m.style + m.layout

// drawing the day: the browser's spend from navigation until the station stands, once under a profile
const mounting = async (browser, mount, part, reps) => {
  const totals = []
  let nodes = 0, heap = 0, rows = 0, layers = null
  for (let i = 0; i < reps; i++) {
    const p = await browser.open(mount, { profile: i === 0 })
    if (!await p.waitFor(drawn[part], 180000)) { await p.close(); return null }
    await p.ev('document.body.offsetHeight')
    if (i === 0) layers = (await p.stopProfile()).layers
    else totals.push(cost(await p.metrics()))
    const m = await p.metrics()
    nodes = m.nodes; heap = m.heap; rows = await p.ev(listed[part])
    await p.close()
  }
  return { ms: summary(totals), nodes, heapMB: round(heap / 2 ** 20, 1), rows, layers }
}

// a second passing: single ticks of the clock line, each read on its own
const ticking = async (browser, mount, part, reps) => {
  const p = await browser.open(mount)
  if (!await p.waitFor(drawn[part], 180000)) { await p.close(); return null }
  const next = async () => { const n = await p.ev(`window.__bench.beats(${period})`); await p.waitFor(`window.__bench.beats(${period}) > ${n}`, 60000) }
  const samples = []
  await next()
  for (let i = 0; i < reps; i++) { await p.sleep(30); samples.push((await p.measure(next)).total) }
  await p.close()
  return summary(samples)
}

const doing = async (browser, mount, part, reps) => {
  const p = await browser.open(mount)
  if (!await p.waitFor(drawn[part], 180000)) { await p.close(); return null }
  await p.sleep(200)
  const samples = []
  for (let i = 0; i < reps; i++) samples.push((await p.measure(() => doings[part][1](p, i))).total)
  await p.close()
  return summary(samples)
}

export const scale = {
  name: 'scale',
  mounts: ({ quick }) => [
    ...parts.flatMap(part => scalesOf(quick).flatMap(s => [stationMount(part, s), clocked(part, s)])),
    ...parts.flatMap(part => openScalesOf(quick).flatMap(s => [open(part, s), openClocked(part, s)])),
  ],
  run: async ({ quick, browser, log }) => {
    const scales = scalesOf(quick), openScales = openScalesOf(quick)
    const paged = {}, unpaged = {}
    for (const part of parts) {
      paged[part] = {}
      for (const s of scales) {
        log(`  ${part} at ${s}×`)
        paged[part][s] = {
          mount: await mounting(browser, stationMount(part, s).name, part, quick ? 3 : 4),
          tick: await ticking(browser, clocked(part, s).name, part, quick ? 5 : 8),
          doing: await doing(browser, stationMount(part, s).name, part, part === 'Pass' ? 6 : quick ? 6 : 12),
        }
      }
      unpaged[part] = {}
      for (const s of openScales) {
        log(`  ${part} at ${s}×, everything on screen`)
        unpaged[part][s] = {
          mount: await mounting(browser, open(part, s).name, part, 2),
          tick: await ticking(browser, openClocked(part, s).name, part, quick ? 4 : 6),
        }
      }
    }
    return { scales, openScales, paged, unpaged }
  },
  report: ({ scales, openScales, paged, unpaged }) => {
    const out = ['### Large data: a station alone on days of growing load (ms)', '',
      `The standard day is ${scales[0]}×; ${scales[scales.length - 1]}× is ${304 * scales[scales.length - 1]} dishes, ${42 * scales[scales.length - 1]} tables and ${252 * scales[scales.length - 1]} orders. Growth is the exponent k of cost ≈ c·scale^k: 0 is flat, 1 linear, above 1 a finding.`, '']
    const row = (part, what, get) => {
      const ys = scales.map(s => get(paged[part][s]))
      return [part, what, ...ys.map(y => y ?? '—'), ys.every(y => typeof y === 'number') ? exponent(scales, ys) : '—']
    }
    const rows = parts.flatMap(part => [
      row(part, 'drawn', r => r.mount?.ms.p50),
      row(part, 'a second passes', r => r.tick?.p50),
      row(part, doings[part][0], r => r.doing?.p50),
      row(part, 'DOM nodes', r => r.mount?.nodes),
      row(part, 'heap (MB)', r => r.mount?.heapMB),
    ])
    out.push(table(['station', 'measured', ...scales.map(s => `${s}×`), 'growth'], rows), '')
    const top = scales[scales.length - 1]
    const share = parts.filter(part => paged[part][top].mount?.layers).map(part => {
      const l = paged[part][top].mount.layers
      const sum = Object.values(l).reduce((a, b) => a + b, 0)
      const pct = k => `${round(100 * (l[k] ?? 0) / sum, 0)}%`
      return [part, round(sum, 0), pct('view model'), pct('carrier'), pct('design system'), pct('browser'), pct('garbage collector')]
    })
    out.push(`Where drawing the day at ${top}× goes (CPU profile, self time by layer):`, '',
      table(['station', 'ms profiled', 'view model', 'carrier', 'design system', 'browser', 'gc'], share), '')
    out.push('With pagination off, everything listed is on screen. A row\'s cost is the difference to the paged station over the difference in rows:', '')
    const perRow = parts.flatMap(part => openScales.map(s => {
      const a = paged[part][s], b = unpaged[part][s]
      if (!a?.mount || !b?.mount || !a.tick || !b.tick) return [part, `${s}×`, '—', '—', '—', '—', '—', '—']
      const rows = b.mount.rows - a.mount.rows
      const per = (x, y) => rows > 0 ? round((y - x) / rows, 3) : '—'
      return [part, `${s}×`, b.mount.rows, b.mount.ms.p50, per(a.mount.ms.p50, b.mount.ms.p50), b.tick.p50, per(a.tick.p50, b.tick.p50), b.mount.nodes]
    }))
    out.push(table(['station', 'scale', 'listed', 'drawn', 'per row', 'a second passes', 'per row', 'DOM nodes'], perRow), '')
    return out.join('\n')
  },
}
