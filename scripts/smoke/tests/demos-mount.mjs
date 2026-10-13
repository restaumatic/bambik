// Every demo runs (guardrail L15): each page in the registry loads its bundle,
// mounts into the demo column, throws nothing, and logs no warning or error
// for longer than the knowledge gates' starvation watchdog takes to speak —
// so an unprimed gate, a failed custom-element bind or a crashing leaf on any
// page fails here by name. What an app does with its wiring is its own code's
// business, checked by the compiler and the laws; this checks that the
// carrier actually runs it in a browser. A vocabulary's `body` also dresses
// the page at mount where the dressing leaves a DOM footprint (guardrails L11).
import { all } from '../../demos.mjs'

const pages = all.filter(d => d.set !== 'laws')
export const demos = pages.map(d => d.name)
export const url = '/demo/'

// past the 3s starvation watchdog
const watch = 3500
const batch = 10

const dressed = {
  mdc2: `document.body.classList.contains('mdc-typography')`,
  mdc3: `[...document.adoptedStyleSheets].some(s => [...s.cssRules].some(r => r.cssText.includes('md-typescale')))`,
  fluent: `getComputedStyle(document.documentElement).getPropertyValue('--colorNeutralBackground1').trim() !== ''`,
}

const mounted = `(() => {
  const column = document.getElementById('demo-column')
  return !!column && [...column.childNodes].some(n => n.id !== 'demo-note' && (n.nodeType === 1 || (n.nodeType === 3 && n.textContent.trim() !== '')))
})()`

export const run = async ({ open, assertEq, sleep }) => {
  for (let i = 0; i < pages.length; i += batch) {
    const chunk = pages.slice(i, i + batch)
    const sessions = await Promise.all(chunk.map(d => open(`/${d.page}`)))
    await sleep(watch)
    for (const [k, d] of chunk.entries()) {
      const s = sessions[k]
      const at = d.page
      assertEq(await s.ev(mounted), true, `${at} mounts into the demo column`)
      assertEq(s.events, [], `${at} runs clean: no exception, warning or error`)
      if (dressed[d.variant]) assertEq(await s.ev(dressed[d.variant]), true, `${at} is dressed by its vocabulary's body`)
      await s.close()
    }
  }
}
