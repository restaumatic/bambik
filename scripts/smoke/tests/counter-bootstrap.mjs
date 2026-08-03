// A representative Bootstrap sibling demo: the same Counter module with the
// PUI.Web.Bootstrap import — native elements in Bootstrap classes, the
// seeded model renders, and the .btn-primary drives the mvu loop.
export const demos = ['demo/7guis/counter-bootstrap']
export const url = '/demo/7guis/counter-bootstrap/'

const count = `document.querySelector('#demo-column h4').textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('#demo-column .card').length`), 1, 'the Bootstrap card renders')
  assertEq(await ev(count), '0', 'seeded count renders')

  await ev(`document.querySelector('button.btn-primary').click()`)
  await sleep(50)
  assertEq(await ev(count), '1', 'clicking the Bootstrap button increments')
}
