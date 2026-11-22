import { fits } from '../fits.mjs'

// Sala: dishes go onto the selected table's order, the order goes to the
// kitchen, and a table with an order in progress takes no second one.
export const tests = [
  {
    name: 'a dish added appears on the order with its amount',
    seed: 'dinnerService',
    run: async ({ click, text, assert }) => {
      assert.includes(await text(), 'Żadnych dań jeszcze nie wybrano', 'an empty table says so')
      await click('Risotto', 'dostępne')
      await click('Risotto', 'dostępne')
      const page = await text()
      assert.includes(page, '24,00 zł', 'two Risotto cost 24,00 zł')
      assert.includes(page, 'dostępne: 1', 'the stock went down by two')
      assert.includes(page, '×2', 'the dish says how much of it is on the order')
      await click('Carbonara', 'dostępne')
      assert.includes(await text(), 'Łącznie\n34,50 zł', 'the order comes to two Risotto and a Carbonara')
    },
  },
  {
    name: 'an order sent to the kitchen makes its table busy',
    seed: 'dinnerService',
    run: async ({ click, sleep, text, assert }) => {
      await click('Carbonara', 'dostępne')
      await click('Wyślij do kuchni')
      await sleep(600)
      const page = await text()
      assert.includes(page, 'w kuchni, złożone 0 min temu', 'the table waits for its order')
      assert.excludes(page, 'Szukaj', 'the menu is gone')
    },
  },
  {
    name: '− is disabled at one portion, + when the dish sells out',
    seed: 'dinnerService',
    run: async ({ click, disabled, assert }) => {
      await click('Tiramisu', 'dostępne')
      assert.equal(await disabled('Zwrócono porcję', 'Tiramisu'), true, '− at one portion')
      await click('Tiramisu', 'dostępne')
      assert.equal(await disabled('Zwrócono porcję', 'Tiramisu'), false, '− at two portions')
      assert.equal(await disabled('Dodano porcję', 'Tiramisu'), true, '+ with none left')
      assert.equal(await disabled('Tiramisu', 'dostępne'), true, 'the sold-out dish takes no tap')
    },
  },
  {
    name: 'the bin asks first; its Cancel keeps the line, the confirm removes it',
    seed: 'dinnerService',
    run: async ({ click, text, assert }) => {
      await click('Margherita', 'dostępne')
      await click('Usuń danie', 'Margherita')
      assert.includes(await text(), 'Usunąć 1 × Margherita z zamówienia?', 'the dialog asks')
      await click('Nie, nie usuwaj dania')
      assert.includes(await text(), 'Wyślij do kuchni', 'the line is kept')
      await click('Usuń danie', 'Margherita')
      await click('Usuń danie', 'Usunąć')
      assert.includes(await text(), 'Żadnych dań jeszcze nie wybrano', 'the line is gone')
    },
  },
  {
    name: 'every dish sold out: the menu stays, its tiles dimmed',
    seed: 'soldOut',
    run: async ({ disabled, text, assert }) => {
      assert.includes(await text(), 'Carbonara', 'the menu stays')
      assert.equal(await disabled('Carbonara', 'dostępne'), true, 'and takes no tap')
    },
  },
  {
    name: 'a table with an order in progress is served as busy, a free one takes an order',
    seed: 'rushHour',
    time: '12:20',
    run: async ({ click, text, assert }) => {
      await click('Stolik 5')
      assert.includes(await text(), 'Stolik 5 · zamówienie #9 do zapłaty, złożone 30 min temu', 'the table at the pass says what it waits for')
      assert.excludes(await text(), 'Szukaj', 'a busy table has no menu')
      await click('Stolik 6')
      assert.includes(await text(), 'Stolik 6 · zamawia', 'the table being taken shows its order')
      assert.includes(await text(), 'Tiramisu', 'with its line')
    },
  },
  {
    name: 'the menu is searched and paged; a new search starts at its first page',
    seed: 'busyHall',
    run: async ({ click, disabled, fill, text, ev, assert }) => {
      const dishes = () => ev(`[...document.querySelectorAll('.md3-tile .md-typescale-label-large')].map(n => n.innerText)`)
      assert.includes(await text(), '1–24 z 304', 'two dozen dishes of three hundred and four')
      assert.equal((await dishes()).length, 24, 'two dozen tiles')
      assert.equal((await dishes())[0], 'Affogato', 'by name')
      await click('Następna strona')
      assert.includes(await text(), '25–48 z 304', 'the second page')
      assert.equal((await dishes())[0], 'Bruschetta z kurczakiem', 'the twenty-fifth dish by name first')
      await fill('Szukaj w menu', 'TRUF')
      assert.includes(await text(), '1–24 z 30', 'thirty dishes with truffles, from the first')
      assert.equal((await dishes()).every(d => d.includes('z truflami')), true, 'only they are listed')
      await click('Następna strona')
      assert.includes(await text(), '25–30 z 30', 'the last page')
      assert.equal(await disabled('Następna strona'), true, 'nothing after it')
      await fill('Szukaj w menu', 'zzz')
      assert.includes(await text(), 'Nie ma takich dań', 'no dish answers')
    },
  },
  {
    name: 'a group narrows the menu to its dishes; a search looks through every group',
    seed: 'busyHall',
    run: async ({ click, fill, text, ev, assert }) => {
      const dishes = () => ev(`[...document.querySelectorAll('.md3-tile .md-typescale-label-large')].map(n => n.innerText)`)
      await click('Następna strona')
      await click('Desery')
      assert.includes(await text(), '1–24 z 51', 'the desserts, from the first page')
      assert.equal((await dishes())[0], 'Affogato', 'by name')
      await fill('Szukaj w menu', 'truf')
      assert.includes(await text(), '1–24 z 30', 'every dish with truffles, whatever its group')
      assert.equal((await dishes())[0], 'Affogato z truflami', 'by name')
      assert.excludes(await text(), 'Przystawki', 'no group to pick while searching')
      await fill('Szukaj w menu', '')
      assert.includes(await text(), '1–24 z 51', 'the search cleared, the desserts again')
      await click('Zupy')
      assert.includes(await text(), '1–20 z 20', 'two soups in ten variations')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop, with an order on the table',
    seed: 'busyHall',
    run: async ui => {
      await ui.click('Affogato z pesto', 'dostępne')
      await ui.click('Affogato z gorgonzolą', 'dostępne')
      await fits(ui)
    },
  },
]
