// The service: every station behind its tab, and the clock passing over
// all of them.
export const tests = [
  {
    name: 'each tab opens its station; a station keeps the page turned to there',
    seed: 'busyHall',
    run: async ({ click, text, assert }) => {
      assert.includes(await text(), 'Stolik 5 · wolny', 'the hall first')
      await click('Następna strona')
      assert.includes(await text(), '11–20 z 300', 'the menu turned')
      await click('Raport')
      assert.includes(await text(), '1–10 z 100', 'the report starts at its first page')
      await click('Kuchnia')
      assert.includes(await text(), '1–10 z 20', 'so does the kitchen')
      await click('Sala')
      assert.includes(await text(), '11–20 z 300', 'the hall, come back to, is where it was left')
    },
  },
  {
    name: 'a minute passing shows on every ticket',
    seed: 'minuteTurning',
    run: async ({ sleep, ev, assert }) => {
      const waits = () => ev(`[...document.querySelectorAll('.md3-card .md-typescale-label-large')].map(w => w.innerText)`)
      assert.equal(await waits(), ['18 min', '15 min', '11 min', '6 min'], 'before the minute turns')
      await sleep(2200)
      assert.equal(await waits(), ['19 min', '16 min', '12 min', '7 min'], 'after it has')
    },
  },
  {
    name: 'what is typed while the clock ticks stays typed',
    seed: 'busyHall',
    run: async ({ fill, sleep, ev, text, assert }) => {
      await fill('Szukaj', 'truf')
      await sleep(2500)
      assert.equal(await ev(`document.querySelector('[name="Szukaj"]').value`), 'truf', 'the search as typed, two ticks later')
      assert.includes(await text(), '1–10 z 30', 'and the menu narrowed by it')
    },
  },
  {
    name: 'on a phone the tab bar scrolls to its last station',
    seed: 'busyHall',
    width: 360,
    run: async ({ click, sleep, text, ev, assert }) => {
      await ev(`[...document.querySelectorAll('md-primary-tab')].find(t => t.innerText.trim() === 'Statystyki').scrollIntoView()`)
      await sleep(400)
      await click('Statystyki')
      assert.includes(await text(), 'Łącznie', 'the statistics opened')
      assert.equal(await ev(`document.documentElement.scrollWidth <= innerWidth`), true, 'and the page still fits')
    },
  },
]
