// The plain-HTML showcase: a static page assembled from element oculars,
// content flowing through field lenses and the keyed `foreach` — and not a
// single MDC class on the page.
export const demos = ['demo/nguis/restaurant-menu']
export const url = '/demo/nguis/restaurant-menu/'

export const run = async ({ ev, assertEq }) => {
  assertEq(await ev(`document.querySelectorAll('article.menu').length`), 1, 'one menu article')
  assertEq(await ev(`document.querySelector('.restaurant-name').textContent`), 'Osteria Yoneda', 'restaurant name')
  assertEq(await ev(`document.querySelectorAll('.course').length`), 3, 'three courses')
  assertEq(await ev(`[...document.querySelectorAll('.course h2')].map(e => e.textContent).join('|')`),
    'Antipasti|Primi|Dolci', 'course titles in data order')
  assertEq(await ev(`document.querySelectorAll('.dish').length`), 9, 'nine dishes')
  assertEq(await ev(`document.querySelector('.dish-price').textContent`), '€14', 'price formatted by the projection')
  assertEq(await ev(`document.querySelectorAll('svg.monogram circle.ring').length`), 1, 'SVG monogram renders')
  assertEq(await ev(`document.querySelectorAll('[class*="mdc-"]').length`), 0, 'no MDC classes at all')
}
