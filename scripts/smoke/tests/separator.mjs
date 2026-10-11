// A separator is shown only between things: `divider`/`hr` is an ocular
// whose rule hides while nothing rendered precedes it or its content
// renders nothing — decided on the DOM, so a detached pane on its far side
// takes the rule away and the feed that attaches the pane brings it back.
export const demos = ['demo/nguis/meeting-booker-fluent', 'demo/nguis/product-review-shoelace', 'demo/nguis/photo-gallery-mdc2', 'demo/nguis/restaurant-menu']
export const pages = [
  { url: '/demo/nguis/meeting-booker-fluent/', label: 'meeting-booker' },
  { url: '/demo/nguis/product-review-shoelace/', label: 'product-review' },
  { url: '/demo/nguis/photo-gallery-mdc2/', label: 'photo-gallery' },
  { url: '/demo/nguis/restaurant-menu/', label: 'restaurant-menu' },
]

const rules = `[...document.querySelectorAll('hr, md-divider, sl-divider, fluent-divider')].map(r => r.style.display !== 'none')`

export const run = async ({ ev, assertEq, sleep, page }) => {
  await sleep(600)
  for (let i = 0; i < 25 && !(await ev(`!!document.querySelector('#demo-column, article.menu')`)); i++) await sleep(200)
  if (page === 'meeting-booker') {
    assertEq(await ev(rules), [false], `[${page}] no rule above the attendees slider while no room is chosen (the pane is detached)`)
    await ev(`(() => {
      const d = document.querySelector('[name="Room"] fluent-dropdown, fluent-dropdown[name="Room"]')
      d.value = [...d.querySelectorAll('fluent-option')].find(o => o.value !== '').value
      d.dispatchEvent(new Event('change'))
    })()`)
    await sleep(300)
    assertEq(await ev(rules), [true], `[${page}] the rule shows once a room is chosen and the slider exists under it`)
    return
  }
  const shown = await ev(rules)
  assertEq(shown.length > 0 && shown.every(Boolean), true, `[${page}] every rule has something on both sides and shows (${JSON.stringify(shown)})`)
}
