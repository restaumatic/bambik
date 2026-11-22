// Content awaited (`awaited`), on the real DOM (diner): the knot a load
// primes is not on screen while the load runs — only the progress bar
// stands — and is attached, for good, by the feed the load lands.
import { byName } from '../../demos.mjs'

export const demos = ['diner']
export const pages = [{ url: `/${byName('diner').page}`, label: 'diner' }]

// what stood beside the progress bar each time the page changed while it showed
const watch = `
window.__beside = []
new MutationObserver(() => {
  const bar = document.querySelector('md-linear-progress')
  if (bar && getComputedStyle(bar).visibility !== 'hidden') window.__beside.push(document.querySelector('md-tabs') !== null)
}).observe(document, { childList: true, subtree: true, attributes: true })
`

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await session.send('Page.enable')
  await session.send('Page.addScriptToEvaluateOnNewDocument', { source: watch })
  await session.send('Page.reload')
  await sleep(500)
  for (let i = 0; i < 60 && !(await ev(`!!document.querySelector('.md3-tile')`).catch(() => false)); i++) await sleep(100)
  assertEq(await ev(`window.__beside.length > 0`), true, `[${page}] the progress bar showed while the day loaded`)
  assertEq(await ev(`window.__beside.some(tabs => tabs)`), false, `[${page}] with no tab bar under it`)
  assertEq(await ev(`document.querySelector('md-tabs').querySelectorAll('md-primary-tab').length`), 6, `[${page}] the tab bar comes with the day`)
}
