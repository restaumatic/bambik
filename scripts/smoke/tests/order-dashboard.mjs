// Custom dashboard controls packaged as a module (DashboardControlsMDC3 over
// PUI.Web.MDC3 + HTML/SVG): the five controls render the deterministic seeded
// order history, the rangePicker window switch re-scopes every tile, and the
// `every` stream keeps the since-open count growing.
export const demos = ['demo/nguis/order-dashboard-mdc3']
export const url = '/demo/nguis/order-dashboard-mdc3/'

const stat = i => `parseInt(document.querySelectorAll('.md-typescale-display-small')[${i}].textContent, 10)`

const pickWindow = label => `(() => {
  const seg = [...document.querySelectorAll('.md3-segmented-button__segment')].find(s => s.textContent.includes('${label}'))
  if (!seg) return false
  seg.click()
  return true
})()`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`!!customElements.get('md-linear-progress')`), true, '@material/web elements registered')

  const labels = await ev(`document.body.textContent`)
  for (const label of ['Showing', 'Orders', 'Revenue', 'Kitchen load', 'Order flow', 'Top dishes']) {
    assertEq(labels.includes(label), true, `control labeled "${label}" rendered`)
  }

  const seeded = await ev(stat(0))
  assertEq(seeded >= 290 && seeded <= 310, true, `seeded history fills the 15-min window (${seeded} orders)`)

  const revenue = await ev(`parseFloat(document.querySelectorAll('.md-typescale-display-small')[1].textContent)`)
  assertEq(revenue > 500, true, `revenue summed over the window (${revenue})`)

  const load = await ev(`document.querySelector('md-linear-progress').value`)
  assertEq(load > 0.2 && load <= 1, true, `kitchen load gauge fed through projection (${load})`)

  const spark = await ev(`document.querySelector('path').getAttribute('d')`)
  assertEq(spark.startsWith('M ') && spark.includes('L '), true, `trend sparkline path computed (${spark.slice(0, 24)}…)`)

  assertEq(await ev(`document.querySelectorAll('md-list-item').length`), 5, 'leaderboard shows the top 5 dishes')

  assertEq(await ev(pickWindow('Last minute')), true, 'rangePicker: pick Last minute')
  await sleep(150)
  const lastMinute = await ev(stat(0))
  assertEq(lastMinute >= 10 && lastMinute <= 35, true, `window switch re-scoped the stats (${lastMinute} orders in the last minute)`)

  assertEq(await ev(pickWindow('Since open')), true, 'rangePicker: pick Since open')
  await sleep(150)
  const sinceOpen = await ev(stat(0))
  assertEq(sinceOpen >= seeded, true, `since-open count covers the whole history (${sinceOpen})`)

  let grown = sinceOpen
  for (let tries = 0; grown <= sinceOpen && tries < 30; tries++) {
    await sleep(500)
    grown = await ev(stat(0))
  }
  assertEq(grown > sinceOpen, true, `the order stream grew the since-open count (${sinceOpen} → ${grown})`)
}
