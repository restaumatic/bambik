// `select'`'s unavailable options (MDC3, on its law-bench row; MDC2's
// `select'` keeps its row at `const []`): the cases the model rules out
// are drawn disabled, re-read on every feed, and a real click on one picks
// nothing. The bench's two samples are a row that rules option `two` out
// and a row that rules nothing out: fed the first, `two` is disabled and a
// click on it stores nothing; fed the second, it is enabled again and a
// click on an option stores it.
export const demos = ['mdc3']
export const pages = [
  { url: '/demo/laws/mdc3/', label: 'mdc3' },
]

const section = `[...document.querySelectorAll('section')].find(s => s.dataset.bench === "select'")`
const select = `${section}.querySelector('md-filled-select')`
const option = c => `[...${section}.querySelectorAll('md-select-option')].find(o => o.value === ${JSON.stringify(c)})`
const unavailable = `[...${section}.querySelectorAll('md-select-option')].filter(o => o.disabled).map(o => o.value)`
const logged = `window.__laws["select'"].log.length`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await sleep(600)
  const mouse = (type, x, y) => session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
  const click = async (js) => {
    const [x, y] = await ev(`(() => { const el = ${js}; el.scrollIntoView({ block: 'center' }); const r = el.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2] })()`)
    await mouse('mouseMoved', x, y)
    await mouse('mousePressed', x, y)
    await mouse('mouseReleased', x, y)
    await sleep(500)
  }

  await ev(`window.__laws["select'"].feed(0).length`)
  await sleep(300)
  assertEq(await ev(unavailable), ['two'], `[${page}] the option the row rules out is disabled`)
  assertEq(await ev(`${select}.value`), 'one', `[${page}] the fed option is picked`)

  const before = await ev(logged)
  await click(select)
  await click(option('two'))
  assertEq(await ev(`${select}.value`), 'one', `[${page}] a click on a disabled option picks nothing`)
  assertEq(await ev(logged), before, `[${page}] and stores nothing`)
  if (await ev(`${select}.open`)) await click(select)

  await ev(`window.__laws["select'"].feed(1).length`)
  await sleep(300)
  assertEq(await ev(unavailable), [], `[${page}] a row that rules nothing out enables it (re-read on the feed)`)
  assertEq(await ev(`${select}.value`), 'two', `[${page}] and shows the option it brings`)

  const fed = await ev(logged)
  await click(select)
  await click(option('one'))
  assertEq(await ev(`${select}.value`), 'one', `[${page}] a click on an available option picks it`)
  assertEq(await ev(`window.__laws["select'"].log.slice(${fed}).map(e => e.value.Pick.type)`), ['one'], `[${page}] and stores it once`)
}
