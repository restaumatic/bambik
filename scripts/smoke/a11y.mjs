// Role + accessible-name locators over the browser's COMPUTED accessibility
// tree (CDP `Accessibility.queryAXTree`), for twin-invariant smoke tests.
//
// The stamp invariant makes a citizen's role and accessible name the model's
// own words — a button's name is its case label, an editor's its field label,
// a group's its field — identical across the vocabulary siblings, because the
// label is stated once in the shared logic's terms and each design system
// merely draws it. So a locator written here walks every twin unchanged, and
// a locator that fails on one twin is a stamp-invariant violation in that
// vocabulary, never test rot. The computed tree (not `[role=…]` selectors) is
// what handles implicit roles on native elements and the shadow DOM inside
// the custom-element catalogues.
//
// Chrome's Recorder panel emits the same vocabulary (`aria/Count[role="button"]`
// selectors), so a recorded walk of a demo translates to these helpers line by
// line — smoke authoring by demonstration.
export const a11y = (session) => {
  const { send } = session
  let doc = null
  const ensure = async () => {
    if (doc) return doc
    await send('DOM.enable')
    await send('Accessibility.enable')
    const r = await send('DOM.getDocument')
    if (r.error) throw new Error('DOM.getDocument: ' + r.error.message)
    doc = r.result.root
    return doc
  }

  // All un-ignored AX nodes matching the role and/or accessible name. Name
  // matching is case-insensitive (after whitespace trim), deliberately: the
  // accessible name is computed from RENDERED text, so a design system's
  // `text-transform` reaches it — MD2 uppercases button labels, making the
  // "Count" button's computed name "COUNT" — while the word itself is the
  // twin-invariant part and its casing is that catalogue's presentation.
  // (Playwright's getByRole matches names the same way, for the same reason.)
  const norm = (s) => (s || '').trim().toLowerCase()
  const query = async ({ role, name }) => {
    const root = await ensure()
    const r = await send('Accessibility.queryAXTree', {
      nodeId: root.nodeId,
      ...(role ? { role } : { accessibleName: name }),
    })
    if (r.error) throw new Error('Accessibility.queryAXTree: ' + r.error.message)
    const nodes = (r.result.nodes || []).filter((n) => !n.ignored)
    return name === undefined ? nodes : nodes.filter((n) => norm(n.name?.value) === norm(name))
  }

  // Click the first matching node the way a user does: real mouse events at
  // the element's box center, so shadow-DOM internals, ripples and focus
  // delegation all behave as in a hand-driven session.
  const click = async ({ role, name }) => {
    const nodes = await query({ role, name })
    if (nodes.length === 0) throw new Error(`no AX node with role=${role} name=${JSON.stringify(name)}`)
    const backendNodeId = nodes[0].backendDOMNodeId
    await send('DOM.scrollIntoViewIfNeeded', { backendNodeId })
    const box = await send('DOM.getBoxModel', { backendNodeId })
    if (box.error) throw new Error('DOM.getBoxModel: ' + box.error.message)
    const q = box.result.model.content
    const x = (q[0] + q[2] + q[4] + q[6]) / 4
    const y = (q[1] + q[3] + q[5] + q[7]) / 4
    for (const type of ['mousePressed', 'mouseReleased']) {
      await send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
    }
  }

  return { query, click }
}
