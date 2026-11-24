// The time run beside the model (`timed`, `timing`, `timingEdits`), on the
// real DOM. Timer: the line follows the time on its own, a run that
// reached its duration stops there, a duration raised resumes it from
// where it stopped — the slider's edit carried its moment — and Reset's
// event carries its own. Stopwatch: Stop, Lap and Start each carry the
// time they left at, so a halted watch keeps its reading and resumes it.
import { byName } from '../../demos.mjs'

export const demos = ['timer-html', 'stopwatch-mdc3']
export const pages = [
  { url: `/${byName('timer-html').page}`, label: 'timer' },
  { url: `/${byName('stopwatch-mdc3').page}`, label: 'stopwatch' },
]

const column = `document.querySelector('#demo-column')`
const lines = `${column}.innerText.split('\\n').map(l => l.trim()).filter(l => l)`
const button = name => `[...${column}.querySelectorAll('button, md-filled-button')].find(b => b.textContent.includes('${name}'))`

const timer = async ({ ev, assertEq, sleep, page }) => {
  const line = `${lines}.find(l => l.includes('s / '))`
  const elapsed = async () => parseFloat(await ev(line))
  const setDuration = d => ev(`(() => { const r = ${column}.querySelector('input[type=range]'); r.value = '${d}'; r.dispatchEvent(new Event('input', { bubbles: true })); r.dispatchEvent(new Event('change', { bubbles: true })) })()`)
  for (let i = 0; i < 25 && !(await ev(`!!${button('Reset')}`)); i++) await sleep(200)
  const first = await elapsed()
  await sleep(700)
  assertEq(await elapsed() > first, true, `[${page}] the line follows the time run`)

  await setDuration(1)
  await sleep(200)
  assertEq(await elapsed() > 1, true, `[${page}] a duration lowered under the time run stops the run where it is`)
  await ev(`${button('Reset')}.click()`)
  await sleep(1500)
  assertEq(await ev(line), '1.0s / 1.0s', `[${page}] the run stops at its duration`)
  await setDuration(5)
  await sleep(700)
  const resumed = await elapsed()
  assertEq(resumed >= 1.5 && resumed <= 2.2, true, `[${page}] a duration raised resumes the run from where it stopped (${resumed})`)

  await ev(`${button('Reset')}.click()`)
  await sleep(350)
  const restarted = await elapsed()
  assertEq(restarted >= 0.2 && restarted <= 0.7, true, `[${page}] Reset starts a run at the moment of its click (${restarted})`)
}

const stopwatch = async ({ ev, assertEq, sleep, page }) => {
  const reading = `${lines}.find(l => /^\\d\\d:\\d\\d\\.\\d$/.test(l))`
  const tenths = async () => { const [m, s] = (await ev(reading)).split(':'); return Math.round((+m * 60 + +s) * 10) }
  for (let i = 0; i < 25 && !(await ev(`!!${button('Start')}`)); i++) await sleep(200)
  await sleep(400)
  assertEq(await ev(reading), '00:00.0', `[${page}] a watch not started reads nothing, whatever time has run`)
  await ev(`${button('Start')}.click()`)
  await sleep(850)
  await ev(`${button('Lap')}.click()`)
  await sleep(300)
  await ev(`${button('Stop')}.click()`)
  await sleep(300)
  const halted = await tenths()
  assertEq(halted >= 10 && halted <= 14, true, `[${page}] Stop keeps the time run from Start to its click (${halted})`)
  const lap = await ev(`${lines}.find(l => l.startsWith('Lap 1'))`)
  assertEq(/^Lap 1 — 00:00\.[89]$|^Lap 1 — 00:01\.0$/.test(lap), true, `[${page}] the lap carries the time of its click (${lap})`)
  await sleep(600)
  assertEq(await tenths(), halted, `[${page}] a halted watch stays where it stopped`)
  await ev(`${button('Start')}.click()`)
  await sleep(500)
  const resumed = await tenths()
  assertEq(resumed >= halted + 3 && resumed <= halted + 7, true, `[${page}] and resumes from there (${resumed})`)
}

export const run = ctx => (ctx.page === 'timer' ? timer(ctx) : stopwatch(ctx))
