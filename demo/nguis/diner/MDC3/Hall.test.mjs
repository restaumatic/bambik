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
    run: async ({ click, text, assert }) => {
      await click('Dodaj', 'Carbonara')
      await click('Wyślij do kuchni')
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
      assert.equal(await disabled('Dodaj', '6,50'), true, 'Dodaj on the sold-out card')
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
      assert.includes(await text(), '9,50 zł\t', 'the line is kept')
      await click('Usuń danie', 'Margherita')
      await click('Usuń danie', 'Usunąć')
      assert.includes(await text(), 'Żadnych dań jeszcze nie wybrano', 'the line is gone')
    },
  },
  {
    name: 'every dish sold out: cards stay, their buttons disabled',
    seed: 'soldOut',
    run: async ({ disabled, text, assert }) => {
      assert.includes(await text(), 'Carbonara', 'the cards stay')
      assert.equal(await disabled('Dodaj', 'Carbonara'), true, 'Dodaj disabled')
    },
  },
  {
    name: 'a table with an order at the pass is unavailable in the select',
    seed: 'rushHour',
    run: async ({ ev, assert }) => {
      assert.equal(await ev(`[...document.querySelectorAll('md-select-option')].filter(o => o.disabled).map(o => o.value)`), ['1', '2', '3', '4', '5'], 'tables 1–5 have orders in progress')
    },
  },
]
