// Every demo runs (guardrail L15): each page in the registry loads its bundle,
// mounts into the demo column, throws nothing, and logs no warning or error
// for longer than the knowledge gates' starvation watchdog takes to speak —
// so an unprimed gate, a failed custom-element bind or a crashing leaf on any
// page fails here by name. What an app does with its wiring is its own code's
// business, checked by the compiler and the laws; this checks that the
// carrier actually runs it in a browser. A vocabulary's `body` also dresses
// the page at mount where the dressing leaves a DOM footprint (guardrails L11).
import { all } from '../../demos.mjs'

export const demos = all.filter(d => d.set !== 'laws').map(d => d.dir)
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
  for (let i = 0; i < demos.length; i += batch) {
    const dirs = demos.slice(i, i + batch)
    const sessions = await Promise.all(dirs.map(dir => open(`/${dir}/`)))
    await sleep(watch)
    for (const [k, dir] of dirs.entries()) {
      const s = sessions[k]
      assertEq(await s.ev(mounted), true, `${dir} mounts into the demo column`)
      assertEq(s.events, [], `${dir} runs clean: no exception, warning or error`)
      const vocabulary = Object.keys(dressed).find(v => dir.endsWith('-' + v))
      if (vocabulary) assertEq(await s.ev(dressed[vocabulary]), true, `${dir} is dressed by its vocabulary's body`)
      await s.close()
    }
  }
}
