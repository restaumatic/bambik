// The host places the RUN of nodes an element's builder appended. On the
// reorder demo's plain-HTML twin a track is a bare `dt`/`dd` pair in a `dl`
// — nothing wraps the pair — so keyed reconciliation must move both nodes
// with the track id, in order, and keep each term beside its own
// description. Before (CHANGELOG, 2026-10-11) the parent's last child stood
// for the element, and a rotation would have carried the `dd` alone.
import { byName } from '../../demos.mjs'
export const demos = ['reorder-html']
export const url = `/${byName('reorder-html').page}`

const tags = `[...document.querySelector('dl').children].map(n => n.tagName)`
const pairs = `(() => {
  const c = [...document.querySelector('dl').children]
  const out = []
  for (let i = 0; i < c.length; i += 2) out.push([c[i].textContent.trim(), c[i + 1].querySelector('input[type=text]').value])
  return out
})()`
const press = label => `[...document.querySelectorAll('button')].find(b => b.textContent.includes(${JSON.stringify(label)})).click()`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(tags), ['DT', 'DD', 'DT', 'DD', 'DT', 'DD', 'DT', 'DD'], 'build: a dt/dd pair per track, nothing wrapping it')
  assertEq(await ev(pairs), [['Track 1', 'Track 1'], ['Track 2', 'Track 2'], ['Track 3', 'Track 3'], ['Track 4', 'Track 4']], 'build: each term beside its own description, in order')

  // tag both nodes of the first track and give its term DOM-local state
  await ev(`(() => {
    const c = document.querySelector('dl').children
    c[0].__smokeTag = 'dt'
    c[1].__smokeTag = 'dd'
    c[0].querySelector('input[type=checkbox]').checked = true
    return true
  })()`)

  await ev(press('Rotate'))
  await sleep(400)

  const after = await ev(`(() => {
    const c = [...document.querySelector('dl').children]
    return {
      tags: c.map(n => n.__smokeTag ?? null),
      checked: c[6].querySelector('input[type=checkbox]').checked,
      term: c[6].textContent.trim(),
      description: c[7].querySelector('input[type=text]').value,
    }
  })()`)
  assertEq(after.tags, [null, null, null, null, null, null, 'dt', 'dd'], 'retraction law: BOTH nodes of the pair moved to the tail with their key, in order')
  assertEq(after.checked, true, 'retraction law: DOM-local state in the term (its checkbox tick) followed the track')
  assertEq(after.term, 'Track 1', 'retraction law: the term still shows its track')
  assertEq(after.description, 'Track 1', 'retraction law: the description still holds its track')

  for (let i = 0; i < 3; i++) {
    await ev(press('Shuffle'))
    await sleep(400)
    const p = await ev(pairs)
    assertEq(p.length === 4 && p.every(([term, description]) => term === description), true, `shuffle ${i + 1}: every term still beside its own description (${JSON.stringify(p)})`)
  }
}
