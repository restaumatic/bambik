// The empty/singleton collection laws (a keyed `foreach` over the laps
// array: [] builds nothing, one lap builds one element) plus the `every`
// heartbeat driving the readout — and `provided` panes swapping the
// button row between halted and running modes.
export const demos = ['demo/nguis/stopwatch-mdc2']
export const url = '/demo/nguis/stopwatch-mdc2/'

const click = label =>
  `(() => { const b = [...document.querySelectorAll('button')].find(b => b.textContent.includes('${label}')); if (b) b.click(); return !!b })()`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('ul li').length`), 0, 'empty law: [] builds no elements')
  assertEq(await ev(`document.querySelector('h3').textContent`), '00:00.0', 'readout starts zeroed')

  assertEq(await ev(click('Start')), true, 'Start button present while halted (provided)')
  await sleep(500)
  const running = await ev(`document.querySelector('h3').textContent`)
  assertEq(running === '00:00.0', false, 'every: the heartbeat advances the readout (' + running + ')')

  assertEq(await ev(click('Lap')), true, 'Lap button present while running (provided)')
  await sleep(200)
  assertEq(await ev(`document.querySelectorAll('ul li').length`), 1, 'singleton law: one lap builds one element')
  const lapText = await ev(`document.querySelector('ul li').textContent`)
  assertEq(lapText.startsWith('Lap 1'), true, 'the lap line renders its projection (' + lapText + ')')

  await ev(click('Stop'))
  await sleep(300)
  assertEq(await ev(click('Reset')), true, 'Reset button present after stop (provided)')
  await sleep(200)
  assertEq(await ev(`document.querySelectorAll('ul li').length`), 0, 'empty law again: reset clears the laps')
}
