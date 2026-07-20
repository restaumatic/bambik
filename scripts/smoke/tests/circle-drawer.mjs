// Channel-fed structure-from-data on the SVG carrier: a canvas click adds a
// circle (built through the keyed `foreach`), undo removes it.
export const demos = ['demo/7guis/circle-drawer']
export const url = '/demo/7guis/circle-drawer/'

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('svg').length`), 1, 'one svg canvas')
  assertEq(await ev(`document.querySelectorAll('svg circle').length`), 0, 'starts with no circles')

  await ev(`(() => {
    const s = document.querySelector('svg')
    const r = s.getBoundingClientRect()
    const opts = { bubbles: true, clientX: r.left + 120, clientY: r.top + 90 }
    s.dispatchEvent(new PointerEvent('pointerdown', opts))
    return true
  })()`)
  await sleep(300)
  assertEq(await ev(`document.querySelectorAll('svg circle').length`), 1, 'a click adds a circle')

  await ev(`[...document.querySelectorAll('button')].find(b => b.textContent.includes('Undo')).click()`)
  await sleep(300)
  assertEq(await ev(`document.querySelectorAll('svg circle').length`), 0, 'undo removes it')
}
