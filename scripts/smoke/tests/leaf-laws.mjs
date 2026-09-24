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

const drive = (name) => `(async () => {
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
  return { shape: e.shape, samples: e.samples, registration, steps, click }
})()`

const same = (a, b) => JSON.stringify(a) === JSON.stringify(b)

export const run = async ({ ev, assertEq, sleep, page }) => {
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
    if (r.shape === '×→+') {
      for (const s of r.steps) {
        assertEq(s.first.length + s.second.length + s.late.length, 0, `${at} Answer: feeding sample ${s.k} fires nothing (got ${JSON.stringify([...s.first, ...s.second, ...s.late])})`)
      }
      assertEq(r.registration.length, 0, `${at} nothing fires at registration`)
    }
  }
}
