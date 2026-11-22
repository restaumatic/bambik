// The view model alone, in node: every function a station's lines call on
// each feed, timed on the day at each scale. No browser, no carrier — what
// is left of a slow feed once these are subtracted is not the view model's.
import { pathToFileURL } from 'node:url'
import path from 'node:path'
import { exponent, round, table } from '../stats.mjs'

const load = (S, scale) => ({ ...S.standardLoad, dishes: 304 * scale, tables: 42 * scale, paid: 219 * scale, ready: 10 * scale, cooking: 20 * scale, taking: 3 * scale })
const variant = type => ({ type, value: {} })

// per station, the functions its lines read the model with, once per feed
// (functions of an element's row are the carrier's to call, row by row)
const reads = {
  Magazyn: ['browseState', 'searchState', 'stockPage', 'pageLine', 'isFirstPage', 'isLastPage'],
  Sala: ['seatings', 'servedLine', 'tableAvailability', 'tableState', 'tableLines', 'orderTotalLine', 'browseState', 'searchState', 'menuPage', 'pageLine', 'isFirstPage', 'isLastPage'],
  Kuchnia: ['kitchenState', 'ticketsPage', 'pageLine', 'isFirstPage', 'isLastPage'],
  Wydawka: ['billsState', 'billsPage', 'pageLine', 'isFirstPage', 'isLastPage'],
  Raport: ['paidState', 'takingsLine', 'paidPage', 'pageLine', 'isFirstPage', 'isLastPage'],
  Statystyki: ['paidState', 'browseState', 'searchState', 'soldQuantityLine', 'soldAmountLine', 'salesPage', 'pageLine', 'isFirstPage', 'isLastPage'],
}

// median time of one call, over enough calls to fill `budget` ms
const time = (f, budget) => {
  const samples = []
  const until = performance.now() + budget
  do {
    const t = performance.now()
    f()
    samples.push(performance.now() - t)
  } while (performance.now() < until || samples.length < 5)
  return samples.sort((a, b) => a - b)[samples.length >> 1]
}

export const viewmodel = {
  name: 'viewmodel',
  mounts: () => [],
  run: async ({ quick }) => {
    const at = m => import(pathToFileURL(path.resolve(`output/${m}/index.js`)))
    const [D, F, S] = await Promise.all([at('Diner'), at('Diner.Fixtures'), at('Diner.Service')])
    const scales = quick ? [1, 4, 16] : [1, 4, 16, 64]
    const budget = quick ? 60 : 200
    const stations = {}
    const generation = {}
    for (const scale of scales) {
      const t = performance.now()
      S.dayAt(load(S, scale))(1795000000)
      generation[scale] = round(performance.now() - t)
    }
    for (const [station, fns] of Object.entries(reads)) {
      stations[station] = {}
      for (const scale of scales) {
        const model = F.busyWith(load(S, scale))({ list: 10, tiles: 24 })(variant(station))
        stations[station][scale] = Object.fromEntries(fns.map(fn => [fn, round(time(() => D[fn](fn === 'servedLine' ? { ...model, time: { hours: 13, minutes: 0, seconds: 0 } } : model), budget), 4)]))
      }
    }
    return { scales, generation, stations }
  },
  report: ({ scales, generation, stations }) => {
    const out = ['### View model in node: one feed\'s reads per station (ms)', '']
    const rows = Object.entries(stations).map(([station, by]) => {
      const sums = scales.map(s => Object.values(by[s]).reduce((a, b) => a + b, 0))
      return [station, ...sums.map(x => round(x, 3)), exponent(scales, sums)]
    })
    out.push(table(['station', ...scales.map(s => `${s}×`), 'growth'], rows), '')
    const worst = []
    for (const [station, by] of Object.entries(stations))
      for (const fn of Object.keys(by[scales[0]])) {
        const ys = scales.map(s => by[s][fn])
        worst.push([`${station}: ${fn}`, ...ys.map(y => round(y, 3)), exponent(scales, ys)])
      }
    worst.sort((a, b) => b[scales.length] - a[scales.length])
    out.push('The costliest reads at the largest scale:', '', table(['read', ...scales.map(s => `${s}×`), 'growth'], worst.slice(0, 10)), '')
    out.push(`Generating the day (\`dayAt\`): ${scales.map(s => `${s}× ${generation[s]} ms`).join(', ')}.`, '')
    return out.join('\n')
  },
}
