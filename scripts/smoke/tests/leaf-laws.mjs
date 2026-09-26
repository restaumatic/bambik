// The component laws of Data.Profunctor.Row ("The laws"), checked leaf by
// leaf against the real DOM: every component a vocabulary publishes is
// mounted alone on its bench page (demo/laws/<vocabulary>/, LawBench.purs)
// and fed from outside, so no merge, loop or gate stands between the feed
// and the leaf's answer.
//
//   1 Repetition  feed x ; feed x ≈ feed x
//   2 Answer      ×→× — every feed answered within its step by exactly one
//                 row; ×→+ — a feed answers nothing, it arms the source
//
// Every editor then takes the platform's own input — typed text and arrow
// keys and mouse clicks through CDP, a pick for selects — and must store a changed
// row that keeps the rest of its fields; every status must show the text of
// the event it was fed.
// Optional selectors are then cleared by their face's own gesture and must
// store the none case (the bench's first sample) exactly once.
// Variant-input shapes (+→×, +→+) owe nothing; their entries are only
// checked to register and accept a feed. Emitters are then clicked once to
// check the replay half of the ×→+ protocol: the click leaves as the leaf's
// case carrying the row last fed.
const vocabularies = ['mdc2', 'mdc3', 'shoelace', 'fluent', 'bootstrap', 'html']
export const demos = vocabularies.map((v) => `demo/laws/${v}`)
export const pages = vocabularies.map((v) => ({ url: `/demo/laws/${v}/`, label: v }))

// A leaf that answers late is not answering "within its step": wait long
// enough for any timer- or rAF-deferred echo to land after the feed returns.
const settle = 250

const clickable = [
  'md-filled-button', 'md-outlined-button', 'md-text-button', 'md-elevated-button',
  'md-filled-tonal-button', 'md-fab', 'md-icon-button', 'md-menu-item', 'md-list-item',
  'sl-button', 'fluent-button', 'button', '.mdc-deprecated-list-item', 'li',
].join(', ')

// The clearing gesture of each optional selector face, on its bench section:
// press the checked radio again, click the selected segment, or pick the
// empty option (Shoelace: its clear button).
const clear = `(section) => {
  const radio = [...section.querySelectorAll('input[type=radio], md-radio, fluent-radio')].find(r => r.checked)
  if (radio) {
    ;(radio.closest('.mdc-form-field, label, fluent-field') ?? radio).dispatchEvent(new PointerEvent('pointerdown', { bubbles: true }))
    radio.click()
    return 'radio'
  }
  const segment = section.querySelector('.mdc-segmented-button__segment--selected, .md3-segmented-button__segment--selected')
  if (segment) { segment.click(); return 'segment' }
  const native = section.querySelector('select')
  if (native) { native.value = ''; native.dispatchEvent(new Event('change')); return 'select' }
  const shoelace = section.querySelector('sl-select')
  if (shoelace) { shoelace.shadowRoot.querySelector('[part~="clear-button"]').click(); return 'sl-select' }
  const fluent = section.querySelector('fluent-dropdown')
  if (fluent) { fluent.value = ''; fluent.dispatchEvent(new Event('change')); return 'fluent-dropdown' }
  const md3 = section.querySelector('md-filled-select')
  if (md3) { md3.selectedIndex = 0; md3.dispatchEvent(new Event('change')); return 'md-filled-select' }
  const mdc2 = section.querySelector('.mdc-select .mdc-deprecated-list-item[data-value=""]')
  if (mdc2) { mdc2.click(); return 'mdc-select' }
  return null
}`

const drive = (name) => `(async () => {
  const clear = ${clear}
  const e = window.__laws[${JSON.stringify(name)}]
  const wait = () => new Promise(r => setTimeout(r, ${settle}))
  await wait()
  const registration = e.log.filter(x => x.phase === 'registration').map(x => x.value)
  const steps = []
  for (let k = 0; k < e.samples.length; k++) {
    const first = e.feed(k)
    const second = e.feed(k)
    const at = e.log.length
    await wait()
    steps.push({ k, first, second, late: e.since(at).map(x => x.value) })
  }
  let click = null
  if (e.shape === '×→+') {
    const section = document.querySelector('section[data-bench=' + JSON.stringify(${JSON.stringify(name)}) + ']')
    const host = section.querySelector(${JSON.stringify(clickable)}) ?? section.lastElementChild
    const at = e.log.length
    host.click()
    await wait()
    click = { host: host.tagName.toLowerCase(), emitted: e.since(at).map(x => x.value) }
  }
  let cleared = null
  if (${JSON.stringify(name)}.endsWith('Optional')) {
    const section = document.querySelector('section[data-bench=' + JSON.stringify(${JSON.stringify(name)}) + ']')
    const at = e.log.length
    const gesture = clear(section)
    await wait()
    const checked = [...section.querySelectorAll('input[type=radio], md-radio, fluent-radio')].filter(r => r.checked).length
      + section.querySelectorAll('.mdc-segmented-button__segment--selected, .md3-segmented-button__segment--selected').length
    cleared = { gesture, emitted: e.since(at).map(x => x.value), checked }
  }
  return { shape: e.shape, samples: e.samples, registration, steps, click, cleared }
})()`

const same = (a, b) => JSON.stringify(a) === JSON.stringify(b)

// ×→× components with no input of their own: displays and panes
const displays = new Set(['progress', 'linearProgress', 'progressBar', 'ratingDisplay', 'imagePane',
  'text', 'dynamic', 'each', 'shown', 'shownWhen', 'inCase', 'shownEach'])

// Readies the platform's own input on an editor's bench section: focuses a
// text field ('type') or a range ('key') for the harness to drive through
// CDP, returns where to click an unchecked radio, segment, tab or toggle
// (clicked for real through CDP), or picks another option of a select
// itself ('pick').
const prepare = (name) => `(() => {
  const section = document.querySelector('section[data-bench=' + JSON.stringify(${JSON.stringify(name)}) + ']')
  const e = window.__laws[${JSON.stringify(name)}]
  e.inputAt = e.log.length
  const q = sel => section.querySelector(sel)
  const text = q('textarea, input[type=text], input:not([type]), md-filled-text-field, md-outlined-text-field, sl-input, sl-textarea, fluent-text-input')
  if (text) { text.focus(); return 'type' }
  const range = q('input[type=range], md-slider, sl-range, fluent-slider, sl-rating')
  if (range) { range.focus(); return 'key' }
  const other = (values, current) => values.find(v => v !== '' && v !== current)
  const native = q('select')
  if (native) {
    native.value = other([...native.options].map(o => o.value), native.value)
    native.dispatchEvent(new Event('change'))
    return 'pick'
  }
  const shoelace = q('sl-select')
  if (shoelace) {
    shoelace.value = other([...shoelace.querySelectorAll('sl-option')].map(o => o.value), shoelace.value)
    shoelace.dispatchEvent(new Event('sl-change'))
    return 'pick'
  }
  const fluent = q('fluent-dropdown')
  if (fluent) {
    fluent.value = other([...fluent.querySelectorAll('fluent-option')].map(o => o.value), fluent.value)
    fluent.dispatchEvent(new Event('change'))
    return 'pick'
  }
  const md3 = q('md-filled-select')
  if (md3) {
    const options = [...md3.querySelectorAll('md-select-option')]
    md3.selectedIndex = options.findIndex(o => o.value !== '' && !o.selected)
    md3.dispatchEvent(new Event('change'))
    return 'pick'
  }
  const mdc2 = q('.mdc-select .mdc-deprecated-list-item:not([data-value=""]):not(.mdc-deprecated-list-item--selected)')
  if (mdc2) { mdc2.click(); return 'pick' }
  const clickable = [...section.querySelectorAll('input[type=radio], md-radio, fluent-radio')].find(r => !r.checked)
    ?? q('.mdc-segmented-button__segment:not(.mdc-segmented-button__segment--selected), .md3-segmented-button__segment:not(.md3-segmented-button__segment--selected)')
    ?? q('.mdc-tab:not(.mdc-tab--active), md-primary-tab:not([active])')
    ?? q('input[type=checkbox], .mdc-switch, md-checkbox, md-switch, md-filter-chip, md-icon-button, sl-switch, fluent-switch, .mdc-evolution-chip__action, .mdc-chip, .mdc-icon-button')
  if (clickable) {
    clickable.scrollIntoView({ block: 'center' })
    const box = clickable.getBoundingClientRect()
    return { x: box.left + box.width / 2, y: box.top + box.height / 2 }
  }
  return null
})()`

const typed = async (session) => {
  await session.send('Input.insertText', { text: 'X' })
}
const clicked = async (session, { x, y }) => {
  for (const type of ['mousePressed', 'mouseReleased']) {
    await session.send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 })
  }
}
const keyed = async (session) => {
  const key = { key: 'ArrowRight', code: 'ArrowRight', windowsVirtualKeyCode: 39, nativeVirtualKeyCode: 39 }
  await session.send('Input.dispatchKeyEvent', { type: 'rawKeyDown', ...key })
  await session.send('Input.dispatchKeyEvent', { type: 'keyUp', ...key })
}

export const run = async ({ ev, session, assertEq, sleep, page }) => {
  for (let i = 0; i < 25 && !(await ev(`!!window.__laws`)); i++) await sleep(200)
  await sleep(600) // custom-element upgrade + FAST's deferred bind
  const names = await ev(`Object.keys(window.__laws)`)
  assertEq(names.length > 0, true, `[${page}] the bench registered its components`)

  for (const name of names) {
    const r = await ev(drive(name))
    const at = `[${page}] ${name} (${r.shape})`
    if (process.env.LAWS_DUMP) console.log(`  DUMP ${at} ${JSON.stringify(r)}`)

    if (r.shape === '×→×') {
      for (const s of r.steps) {
        assertEq(s.first.length, 1, `${at} Answer: feed ${s.k} answered by exactly one row (got ${JSON.stringify(s.first)})`)
        assertEq(same(s.second, s.first), true, `${at} Repetition: feeding sample ${s.k} again answers the same (${JSON.stringify(s.first)} then ${JSON.stringify(s.second)})`)
        assertEq(s.late.length, 0, `${at} Answer within the step: nothing arrives after feed ${s.k} returns (late ${JSON.stringify(s.late)})`)
      }
    }
    if (r.cleared) {
      assertEq(same(r.cleared.emitted, [r.samples[0]]), true, `${at} clearing (${r.cleared.gesture}) stores the none case once (got ${JSON.stringify(r.cleared.emitted)})`)
      assertEq(r.cleared.checked, 0, `${at} clearing (${r.cleared.gesture}) leaves nothing checked`)
    }
    if (r.shape === '×→×' && !displays.has(name)) {
      const gesture = await ev(prepare(name))
      if (gesture === 'type') await typed(session)
      if (gesture === 'key') await keyed(session)
      if (gesture?.x !== undefined) await clicked(session, gesture)
      await sleep(700) // past a debounced field's quiet window
      const emitted = await ev(`window.__laws[${JSON.stringify(name)}].since(window.__laws[${JSON.stringify(name)}].inputAt).map(x => x.value)`)
      const fed = r.samples[r.samples.length - 1]
      const stored = emitted[emitted.length - 1]
      assertEq(gesture !== null, true, `${at} Input: the bench knows how to edit it`)
      assertEq(emitted.length >= 1 && !same(stored, fed), true, `${at} Input (${gesture?.x !== undefined ? 'click' : gesture}): the user's edit stores a changed row (got ${JSON.stringify(emitted)} after ${JSON.stringify(fed)})`)
      if (stored && 'other' in fed) assertEq(stored.other, fed.other, `${at} Input: the edit keeps the rest of the row`)
    }
    if (r.shape === '+→×' && r.samples.every(x => x.type === 'event')) {
      const last = r.samples[r.samples.length - 1].value
      const shown = await ev(`document.querySelector('section[data-bench=' + JSON.stringify(${JSON.stringify(name)}) + ']').textContent.includes(${JSON.stringify(last)})`)
      assertEq(shown, true, `${at} Status: the fed event's text is shown (${JSON.stringify(last)})`)
    }
    if (r.shape === '×→+') {
      for (const s of r.steps) {
        assertEq(s.first.length + s.second.length + s.late.length, 0, `${at} Answer: feeding sample ${s.k} fires nothing (got ${JSON.stringify([...s.first, ...s.second, ...s.late])})`)
      }
      assertEq(r.registration.length, 0, `${at} nothing fires at registration`)
    }
  }
}
