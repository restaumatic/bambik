#!/usr/bin/env node
// The benchmark of the library on Diner (doc/benchmark-plan.md).
//
//   npm run bench                    # every scenario, in full (about half an hour)
//   npm run bench -- quick           # every scenario, shortened
//   npm run bench -- scale walk      # only the scenarios named
//   npm run bench -- quick viewmodel
//   npm run bench -- compare [file]  # a results file (the latest) against bench/baseline.json
//
// Scenarios:
//   viewmodel       the view model's reads in node, per station and scale
//   responsiveness  real input per station at 1× and 4× CPU throttle, a
//                   second passing, the application starting
//   scale           mount, tick and one interaction per station on days of
//                   1× to 64× the standard load, and with pagination off
//   walk            a seeded random walk of real clicks checked, step by
//                   step, against the view model run in node
//   races           bursts, typing under ticks, clicks during an effect
//   soak            minutes of random input under the clock, sampled for
//                   leaks; station-switch cycles; the clock's own rate
//   size            synthetic applications of 6 to 48 stations: compile
//                   time, bundle bytes, mount and feed cost
//
// Every scenario runs on mounts compiled into .bench/ (gitignored): a
// station alone on a day of a given load (`Diner.Fixtures.busyWith`), the
// six stations under their tab bar, or the application as it starts. Scale
// is a compiled fixture, never a knob in the application. Results are
// written to bench/results/<date>-<commit>.json with a markdown summary
// beside it; bench/baseline.json is the committed run to compare against.
// Bundle nothing first: the bench compiles what it mounts (it needs
// `spago build` to be up to date).
import { spawnSync } from 'node:child_process'
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from 'node:fs'
import os from 'node:os'
import { buildMounts } from './build.mjs'
import { startBrowser } from './page.mjs'
import { round, table } from './stats.mjs'
import { viewmodel } from './scenarios/viewmodel.mjs'
import { responsiveness } from './scenarios/responsiveness.mjs'
import { scale } from './scenarios/scale.mjs'
import { walk } from './scenarios/walk.mjs'
import { races } from './scenarios/races.mjs'
import { soak } from './scenarios/soak.mjs'
import { size } from './scenarios/size.mjs'

const all = [viewmodel, responsiveness, scale, walk, races, soak, size]
const args = process.argv.slice(2)

// the p50 of every summarised sample set in a results file, by its path
const medians = (node, at = [], acc = {}) => {
  if (node && typeof node === 'object') {
    if (typeof node.p50 === 'number' && typeof node.n === 'number') acc[at.join(' / ')] = node.p50
    else for (const [k, v] of Object.entries(node)) medians(v, [...at, k], acc)
  }
  return acc
}

if (args[0] === 'compare') {
  const dir = 'bench/results'
  const file = args[1] ?? (existsSync(dir) ? readdirSync(dir).filter(f => f.endsWith('.json')).sort().map(f => `${dir}/${f}`).pop() : null)
  if (!file || !existsSync('bench/baseline.json')) { console.error('compare needs bench/baseline.json and a results file'); process.exit(1) }
  const base = medians(JSON.parse(readFileSync('bench/baseline.json', 'utf8')).results)
  const now = medians(JSON.parse(readFileSync(file, 'utf8')).results)
  // a median moved by more than a quarter and by more than half a millisecond
  const moved = Object.keys(now).filter(k => k in base && Math.abs(now[k] - base[k]) > 0.5 && Math.abs(now[k] - base[k]) > 0.25 * Math.max(base[k], 1e-9))
    .map(k => [k, base[k], now[k], `${now[k] > base[k] ? '+' : ''}${round(100 * (now[k] - base[k]) / base[k], 0)}%`])
  console.log(`${file} against bench/baseline.json: ${Object.keys(now).length} medians, ${moved.length} moved by more than 25%\n`)
  if (moved.length) console.log(table(['sample', 'baseline p50', 'now p50', 'change'], moved))
  process.exit(0)
}

const quick = args.includes('quick')
const names = args.filter(a => a !== 'quick')
const unknown = names.filter(n => !all.some(s => s.name === n))
if (unknown.length) { console.error(`no scenario "${unknown.join('", "')}"; there are: ${all.map(s => s.name).join(', ')}`); process.exit(1) }
const chosen = names.length ? all.filter(s => names.includes(s.name)) : all
const opts = { quick }

const started = Date.now()
const log = msg => console.error(`[${String(Math.round((Date.now() - started) / 1000)).padStart(4)}s] ${msg}`)
const mounts = new Map()
for (const s of chosen) for (const m of s.mounts(opts)) mounts.set(m.name, m)
if (mounts.size) {
  log(`compiling ${mounts.size} mounts`)
  await buildMounts([...mounts.values()])
}
const browser = mounts.size || chosen.some(s => s.browser) ? await startBrowser() : null
const results = {}
try {
  for (const s of chosen) {
    log(`▶ ${s.name}`)
    results[s.name] = await s.run({ ...opts, browser, log })
  }
} finally {
  if (browser) await browser.stop()
}

const git = (...a) => spawnSync('git', a, { encoding: 'utf8' }).stdout.trim()
const meta = {
  date: new Date().toISOString(),
  commit: git('rev-parse', '--short', 'HEAD') + (git('status', '--porcelain') ? '+dirty' : ''),
  quick,
  machine: `${os.cpus()[0].model.trim()}, ${os.cpus().length} threads, ${Math.round(os.totalmem() / 2 ** 30)} GB`,
  node: process.version,
  chrome: browser?.version ?? null,
  seconds: Math.round((Date.now() - started) / 1000),
}
const summary = [
  `# bambik on Diner — ${meta.date.slice(0, 10)}, ${meta.commit}${quick ? ' (quick)' : ''}`, '',
  `${meta.machine}; ${meta.chrome ?? 'no browser'}; node ${meta.node}; ${meta.seconds} s.`, '',
  ...chosen.map(s => s.report(results[s.name])),
].join('\n')
mkdirSync('bench/results', { recursive: true })
const stem = `bench/results/${meta.date.slice(0, 16).replace(/[:T]/g, '-')}-${meta.commit.replace('+', '-')}${quick ? '-quick' : ''}`
writeFileSync(`${stem}.json`, JSON.stringify({ meta, results }, null, 1))
writeFileSync(`${stem}.md`, summary + '\n')
console.log(summary)
log(`written ${stem}.json and .md`)
