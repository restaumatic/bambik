import { fits } from '../fits.mjs'

// Sala: dishes go onto the selected table's order, the order goes to the
// kitchen, and a table with an order in progress takes no second one.
export const tests = [
  {
    name: 'a dish added appears on the order with its amount',
    seed: 'dinnerService',
    run: async ({ click, text, assert }) => {
      assert.includes(await text(), 'Żadnych dań jeszcze nie wybrano', 'an empty table says so')
      await click('Dodaj', 'Risotto')
      await click('Dodaj', 'Risotto')
      const page = await text()
      assert.includes(page, '24,00 zł', 'two Risotto cost 24,00 zł')
      assert.includes(page, 'dostępne: 1', 'the stock went down by two')
    },
  },
  {
    name: 'an order sent to the kitchen makes its table busy',
    seed: 'dinnerService',
    run: async ({ click, sleep, text, assert }) => {
      await click('Dodaj', 'Carbonara')
      await click('Wyślij do kuchni')
      await sleep(600)
      const page = await text()
      assert.includes(page, 'Stolik zajęty', 'the table is busy')
      assert.excludes(page, 'Dodaj', 'the menu is gone')
    },
  },
  {
    name: '− is disabled at one portion, + when the dish sells out',
    seed: 'dinnerService',
    run: async ({ click, disabled, assert }) => {
      await click('Dodaj', 'Tiramisu')
      assert.equal(await disabled('Zwrócono porcję', 'Tiramisu'), true, '− at one portion')
      await click('Dodaj', 'Tiramisu')
      assert.equal(await disabled('Zwrócono porcję', 'Tiramisu'), false, '− at two portions')
      assert.equal(await disabled('Dodano porcję', 'Tiramisu'), true, '+ with none left')
      assert.equal(await disabled('Dodaj', '6,50'), true, 'Dodaj on the sold-out dish')
    },
  },
  {
    name: 'the bin asks first; its Cancel keeps the line, the confirm removes it',
    seed: 'dinnerService',
    run: async ({ click, text, assert }) => {
      await click('Dodaj', 'Margherita')
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
    name: 'every dish sold out: the menu stays, its buttons disabled',
    seed: 'soldOut',
    run: async ({ disabled, text, assert }) => {
      assert.includes(await text(), 'Carbonara', 'the menu stays')
      assert.equal(await disabled('Dodaj', 'Carbonara'), true, 'Dodaj disabled')
    },
  },
  {
    name: 'a table with an order in progress is served as busy, a free one takes an order',
    seed: 'rushHour',
    run: async ({ click, text, assert }) => {
      await click('Stolik 5')
      assert.includes(await text(), 'Stolik 5 · zajęty', 'the table at the pass is busy')
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
      const dishes = () => ev(`[...document.querySelectorAll('[aria-label="Menu"] tbody tr')].map(r => r.cells[0].innerText.split('\\n')[0])`)
      assert.includes(await text(), '1–10 z 300', 'ten dishes of three hundred')
      assert.equal((await dishes()).length, 10, 'ten rows')
      assert.equal((await dishes())[0], 'Affogato', 'by name')
      await click('Następna strona')
      assert.includes(await text(), '11–20 z 300', 'the second page')
      assert.equal((await dishes())[0], 'Arancini', 'the eleventh dish by name first')
      await fill('Szukaj', 'TRUF')
      assert.includes(await text(), '1–10 z 30', 'thirty dishes with truffles, from the first')
      assert.equal((await dishes()).every(d => d.includes('z truflami')), true, 'only they are listed')
      await click('Następna strona')
      await click('Następna strona')
      assert.includes(await text(), '21–30 z 30', 'the last page')
      assert.equal(await disabled('Następna strona'), true, 'nothing after it')
      await fill('Szukaj', 'zzz')
      assert.includes(await text(), 'Żadne danie nie pasuje', 'no dish answers')
    },
  },
  {
    name: 'a group narrows the menu to its dishes, and the search looks within it',
    seed: 'busyHall',
    run: async ({ click, fill, text, ev, assert }) => {
      const dishes = () => ev(`[...document.querySelectorAll('[aria-label="Menu"] tbody tr')].map(r => r.cells[0].innerText.split('\\n')[0])`)
      await click('Następna strona')
      await click('Desery')
      assert.includes(await text(), '1–10 z 50', 'five desserts in ten variations, from the first page')
      assert.equal((await dishes())[0], 'Affogato', 'by name')
      await fill('Szukaj', 'truf')
      assert.equal(await dishes(), ['Affogato z truflami', 'Cannoli z truflami', 'Panna cotta z truflami', 'Tiramisu z truflami', 'Zabaglione z truflami'], 'the desserts with truffles')
      await click('Zupy')
      assert.equal(await dishes(), ['Minestrone z truflami', 'Ribollita z truflami'], 'the soups with truffles')
      await click('Wszystkie')
      assert.includes(await text(), '1–10 z 30', 'every dish with truffles')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop, with an order on the table',
    seed: 'busyHall',
    run: async ui => {
      await ui.click('Dodaj', 'Affogato z pesto')
      await ui.click('Dodaj', 'Affogato z gorgonzolą')
      await fits(ui)
    },
  },
]
