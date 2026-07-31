// The keyed Mealy (PUI `accumulated`, the +→× member): one { key, value }
// case per second grows the board to its three teams, updates known keys in
// place, and re-emits the whole array immediately — the standings line reads
// the aggregate.
export const demos = ['demo/nguis/scoreboard-mdc2']
export const url = '/demo/nguis/scoreboard-mdc2/'

export const run = async ({ ev, assertEq, sleep }) => {
  await sleep(3200)
  assertEq(await ev(`document.querySelectorAll('ul li').length`), 3, 'the key set saturates at the three teams')

  const standings = await ev(`[...document.querySelectorAll('p')].map(p => p.textContent).find(t => t.includes('teams on the board')) ?? ''`)
  assertEq(/3 teams on the board — leading: \S+ \(\d+\)/.test(standings), true, 'the aggregate array drives the standings (' + standings + ')')

  await ev(`(() => { window.__rows = [...document.querySelectorAll('ul li')]; return true })()`)
  await sleep(2400)
  assertEq(
    await ev(`window.__rows.length === 3 && window.__rows.every((n, i) => document.querySelectorAll('ul li')[i] === n)`),
    true,
    'known keys update in place — row nodes survive; the board never rebuilds'
  )

  const later = await ev(`[...document.querySelectorAll('p')].map(p => p.textContent).find(t => t.includes('teams on the board')) ?? ''`)
  assertEq(later === standings, false, 'points keep accumulating (' + later + ')')
}
