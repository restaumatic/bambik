// A leaf never escapes its surface: the multi-line text field states its
// preferred size in `columns`, but on a viewport narrower than that (a
// phone) it must clamp to its card, not overflow it. Regression test at a
// 400px device viewport over both Material twins of the flagship.
export const demos = ['demo/nguis/order-form-mdc2', 'demo/nguis/order-form-mdc3']
export const pages = [
  { url: '/demo/nguis/order-form-mdc2/', label: 'mdc2' },
  { url: '/demo/nguis/order-form-mdc3/', label: 'mdc3' },
]

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  await session.send('Emulation.setDeviceMetricsOverride', { width: 400, height: 800, deviceScaleFactor: 1, mobile: true })
  await sleep(1800) // loadOrder feeds the form after 1s
  const m = await ev(`(() => {
    const ta = document.querySelector('.mdc-text-field--textarea, md-filled-text-field[type="textarea"]')
    if (!ta) return { missing: true }
    const card = ta.closest('.mdc-card, .md3-card')
    const t = ta.getBoundingClientRect(), c = card.getBoundingClientRect()
    return { taRight: Math.round(t.right), cardRight: Math.round(c.right) }
  })()`)
  assertEq(m.missing === undefined, true, `[${page}] the remarks textarea is on the page`)
  assertEq(m.taRight <= m.cardRight, true, `[${page}] the textarea clamps to its card at 400px (textarea right ${m.taRight}, card right ${m.cardRight})`)
}
