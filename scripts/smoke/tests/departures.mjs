// The keyed dispatch (PUI `dispatched`, the +→+ member): a deterministic
// ticker feeds one { key, value } case per second — an unknown key becomes a
// new row (a new runtime case), a known key re-feeds exactly its row, and the
// tagged output drives the last-update line.
export const demos = ['demo/nguis/departures-mdc2']
export const url = '/demo/nguis/departures-mdc2/'

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('ul li').length >= 1`), true, 'the seed tick builds the first case')

  await sleep(3200)
  const rows = await ev(`document.querySelectorAll('ul li').length`)
  assertEq(rows >= 3 && rows <= 5, true, 'new keys instantiate as they first appear (rows=' + rows + ')')

  const first = await ev(`document.querySelector('ul li').textContent`)
  assertEq(first.includes('LH 441'), true, 'first-appearance order: LH 441 leads (' + first + ')')

  const last = await ev(`[...document.querySelectorAll('p')].map(p => p.textContent).find(t => t.includes('Last update:')) ?? ''`)
  assertEq(/Last update: .+ → .+/.test(last), true, 'the tagged output drives the last-update line (' + last + ')')

  await ev(`(() => { window.__rows = [...document.querySelectorAll('ul li')]; return true })()`)
  await sleep(2400)
  assertEq(
    await ev(`window.__rows.every((n, i) => document.querySelectorAll('ul li')[i] === n)`),
    true,
    'known keys re-feed in place — row nodes survive further ticks'
  )
}
