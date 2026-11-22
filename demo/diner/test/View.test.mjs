// The application itself, opened on the day its stand-in server gives it:
// every station behind its tab, the clock passing over all of them, and
// what one station does showing at another.

// the service is open once the hall's tables are drawn
const opened = async ({ ev, sleep }) => {
  for (let i = 0; i < 60 && !(await ev(`document.querySelector('.md3-tile') !== null`)); i++) await sleep(100)
  await sleep(300)
}

export const tests = [
  {
    name: 'nothing but the progress bar stands while the day loads',
    run: async ui => {
      const { ev, text, assert } = ui
      assert.equal(await ev(`document.querySelector('md-linear-progress') !== null && document.querySelector('md-tabs') === null`), true, 'no tab bar under the progress bar')
      await opened(ui)
      assert.includes(await text(), 'Magazyn', 'the tabs come with the day')
    },
  },
  {
    name: 'each tab opens its station; a station keeps the page turned to there',
    run: async ui => {
      const { click, text, assert } = ui
      await opened(ui)
      assert.includes(await text(), 'Stolik 5 · wolny', 'the hall first')
      await click('Następna strona')
      assert.includes(await text(), '25–48 z 304', 'the menu turned')
      await click('Raport')
      assert.includes(await text(), '1–10 z 219', 'the report starts at its first page')
      await click('Kuchnia')
      assert.includes(await text(), '1–10 z 20', 'so does the kitchen')
      await click('Sala')
      assert.includes(await text(), '25–48 z 304', 'the hall, come back to, is where it was left')
    },
  },
  {
    name: 'the clock runs: within a quarter of a minute some ticket has waited a minute more',
    run: async ui => {
      const { click, sleep, ev, assert } = ui
      await opened(ui)
      await click('Kuchnia')
      const waits = () => ev(`[...document.querySelectorAll('.md3-card .md-typescale-label-large')].map(w => parseInt(w.innerText))`)
      const before = await waits()
      assert.equal(before.length, 10, 'ten tickets')
      let after = before
      for (let i = 0; i < 50 && String(after) === String(before); i++) { await sleep(500); after = await waits() }
      assert.equal(after.every((w, i) => w === before[i] || w === before[i] + 1), true, 'no wait went back or jumped')
      assert.equal(after.some((w, i) => w === before[i] + 1), true, 'and one has grown')
    },
  },
  {
    name: 'what is typed while the clock ticks stays typed',
    run: async ui => {
      const { fill, sleep, ev, text, assert } = ui
      await opened(ui)
      await fill('Szukaj w menu', 'truf')
      await sleep(2500)
      assert.equal(await ev(`document.querySelector('[name="Szukaj w menu"]').value`), 'truf', 'the search as typed, two ticks later')
      assert.includes(await text(), '1–24 z 30', 'and the menu narrowed by it')
    },
  },
  {
    name: 'the hall and the stockroom each look for dishes on their own',
    run: async ui => {
      const { click, fill, ev, text, assert } = ui
      await opened(ui)
      await click('Zupy')
      assert.includes(await text(), '1–20 z 20', 'soups on the menu')
      await click('Magazyn')
      assert.includes(await text(), '1–10 z 304', 'every dish in the stockroom')
      await fill('Szukaj w magazynie', 'tiramisu')
      assert.includes(await text(), '1–10 z 11', 'tiramisu looked for in the stockroom')
      await click('Sala')
      assert.equal(await ev(`document.querySelector('[name="Szukaj w menu"]').value`), '', 'nothing searched for on the menu')
      assert.includes(await text(), '1–20 z 20', 'still soups there')
      await click('Magazyn')
      assert.equal(await ev(`document.querySelector('[name="Szukaj w magazynie"]').value`), 'tiramisu', 'and the stockroom kept its search')
    },
  },
  {
    name: 'on a phone the tab bar scrolls to its last station',
    width: 360,
    run: async ui => {
      const { click, sleep, text, ev, assert } = ui
      await opened(ui)
      await ev(`[...document.querySelectorAll('md-primary-tab')].find(t => t.innerText.trim() === 'Statystyki').scrollIntoView()`)
      await sleep(400)
      await click('Statystyki')
      assert.includes(await text(), 'Łącznie', 'the statistics opened')
      assert.equal(await ev(`document.documentElement.scrollWidth <= innerWidth`), true, 'and the page still fits')
    },
  },
  {
    name: 'a price set in the stockroom is the menu\'s from then on; what was ordered before keeps its price',
    run: async ui => {
      const { click, fill, ev, assert } = ui
      await opened(ui)
      const bill = () => ev(`[...document.querySelectorAll('tbody tr')].find(r => r.cells[0].innerText.startsWith('#220')).cells[2].innerText`)
      const offer = dish => ev(`[...document.querySelectorAll('.md3-tile')].find(t => t.querySelector('.md-typescale-label-large')?.innerText === '${dish}').innerText`)
      const line = dish => ev(`[...document.querySelectorAll('table[aria-label="Zamówienie"] tbody tr')].find(r => r.cells[0].innerText.startsWith('${dish}')).cells[0].innerText`)
      await click('Affogato z pesto', 'dostępne')
      assert.includes(await line('Affogato z pesto'), '21,00 zł', 'ordered at 21,00 zł')
      await click('Wydawka')
      assert.equal(await bill(), '191,50 zł', 'three Risotto among the dishes of bill #220')
      await click('Magazyn')
      await fill('Szukaj w magazynie', 'risotto')
      await fill('Cena', '99', 'Risotto')
      await fill('Szukaj w magazynie', 'affogato z pesto')
      await fill('Cena', '30', 'Affogato z pesto')
      await click('Wydawka')
      assert.equal(await bill(), '191,50 zł', 'the bill keeps the prices its dishes were ordered at')
      await click('Sala')
      assert.includes(await offer('Affogato z pesto'), '30,00 zł', 'the menu offers the new price')
      await click('Dodano porcję', 'Affogato z pesto')
      assert.includes(await line('Affogato z pesto'), '42,00 zł', 'a second portion on the line already ordered, still at 21,00 zł')
      await click('Stolik 7')
      await click('Affogato z pesto', 'dostępne')
      assert.includes(await line('Affogato z pesto'), '30,00 zł', 'a new order takes the new price')
    },
  },
  {
    name: 'a price that cannot be read takes the dish off the menu',
    run: async ui => {
      const { click, disabled, fill, ev, assert } = ui
      await opened(ui)
      await click('Magazyn')
      await fill('Cena', 'abc', 'Affogato z gorgonzolą')
      await click('Sala')
      assert.equal(await disabled('Affogato z gorgonzolą', 'dostępne'), true, 'its tile takes no tap')
      assert.includes(await ev(`[...document.querySelectorAll('.md3-tile')].find(t => t.querySelector('.md-typescale-label-large')?.innerText === 'Affogato z gorgonzolą').innerText`), 'bez ceny', 'and says it has no price')
    },
  },
]
