// Every view runs before its logic exists (guardrails L18): each demo's holey
// twin (scripts/holes.mjs — logic stubbed to bare holes, every other module
// unchanged over the real library) mounts into the demo column, throws nothing
// and logs no warning or error past the starvation watchdog. Then every control
// is exercised — clicked, pointed at, typed into, picked — and still no hole is
// reached (the page flag set by `hole`, so a hole reached inside an Aff counts
// too): the gates withhold the input, whose drop the watchdog is free to
// report. What a design system's own handlers make of synthetic events is not
// this law's business, so only the flag is asserted under input.
import { existsSync } from 'node:fs'
import { all } from '../../demos.mjs'

const twins = all.filter(d => d.set !== 'laws').map(d => `.holey/${d.dir}`)
export const demos = []
export const url = '/.holey/demo/'

const watch = 3500
const batch = 10

const mounted = `(() => {
  const column = document.getElementById('demo-column')
  return !!column && [...column.childNodes].some(n => n.id !== 'demo-note' && (n.nodeType === 1 || (n.nodeType === 3 && n.textContent.trim() !== '')))
})()`

const reached = `globalThis.__bambikHoleReached ?? null`

const exercise = `(() => {
  const column = document.getElementById('demo-column')
  const inDemo = [...column.children].filter(n => n.id !== 'demo-note')
  const every = inDemo.flatMap(n => [n, ...n.querySelectorAll('*')]).filter(e => !e.closest('a[href]'))
  for (const e of every) {
    e.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true, clientX: 10, clientY: 10 }))
    if (typeof e.click === 'function') e.click()
  }
  for (const e of every.filter(e => e.matches('input, textarea') || 'value' in e && e.tagName.includes('-'))) {
    try { e.focus?.(); e.value = '7' } catch {}
    e.dispatchEvent(new Event('input', { bubbles: true, composed: true }))
    e.dispatchEvent(new Event('change', { bubbles: true, composed: true }))
  }
  for (const s of every.filter(e => e.matches('select'))) {
    s.selectedIndex = s.options.length - 1
    s.dispatchEvent(new Event('change', { bubbles: true }))
  }
  return every.length
})()`

export const run = async ({ open, assertEq, sleep }) => {
  const missing = twins.filter(d => !existsSync(`${d}/bundle.js`))
  assertEq(missing, [], 'every demo has a bundled holey twin (node scripts/holes.mjs)')
  const dirs = twins.filter(d => !missing.includes(d))
  for (let i = 0; i < dirs.length; i += batch) {
    const chunk = dirs.slice(i, i + batch)
    const sessions = await Promise.all(chunk.map(dir => open(`/${dir}/`)))
    await sleep(watch)
    for (const [k, dir] of chunk.entries()) {
      const s = sessions[k]
      assertEq(await s.ev(mounted), true, `${dir} mounts from its view alone`)
      assertEq(s.events, [], `${dir} mounts clean: no exception, warning or error`)
      assertEq(await s.ev(reached), null, `${dir} reaches no hole at mount`)
      s.events.length = 0
      await s.ev(exercise)
    }
    await sleep(watch)
    for (const [k, dir] of chunk.entries()) {
      const s = sessions[k]
      assertEq(await s.ev(reached), null, `${dir} reaches no hole under input`)
      await s.close()
    }
  }
}
