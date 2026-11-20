import { fits } from '../fits.mjs'

// Magazyn: stock edited by typing or by −/+.
export const tests = [
  {
    name: '− is disabled at zero; + and − change the count',
    seed: 'soldOut',
    run: async ({ click, disabled, ev, assert }) => {
      const counts = () => ev(`[...document.querySelectorAll('[name="Porcje"]')].map(f => f.value)`)
      assert.equal(await disabled('Ubyła porcja', 'Risotto'), true, '− at zero')
      await click('Przybyła porcja', 'Risotto')
      assert.equal((await counts())[2], '1', 'one more')
      assert.equal(await disabled('Ubyła porcja', 'Risotto'), false, '− at one')
      await click('Ubyła porcja', 'Risotto')
      assert.equal((await counts())[2], '0', 'one less')
      assert.equal(await disabled('Ubyła porcja', 'Risotto'), true, '− at zero again')
    },
  },
  {
    name: 'unreadable stock is left as typed by −/+',
    seed: 'dinnerService',
    run: async ({ fill, click, ev, assert }) => {
      await fill('Porcje', '1o', 'Tiramisu')
      await click('Przybyła porcja', 'Tiramisu')
      assert.equal(await ev(`[...document.querySelectorAll('[name="Porcje"]')][3].value`), '1o', 'kept as typed')
    },
  },
  {
    name: 'Wyzeruj zeroes its row once confirmed, then is disabled',
    seed: 'dinnerService',
    run: async ({ click, disabled, ev, text, assert }) => {
      const counts = () => ev(`[...document.querySelectorAll('[name="Porcje"]')].map(f => f.value).join(' ')`)
      await click('Wyzeruj', 'Risotto')
      assert.includes(await text(), 'Wyzerować porcje dania Risotto?', 'the dialog asks')
      await click('Nie, nie zeruj')
      assert.equal(await counts(), '6 8 3 2', 'the stock is kept')
      await click('Wyzeruj', 'Risotto')
      await click('Wyzeruj', 'Wyzerować porcje dania Risotto?')
      assert.equal(await counts(), '6 8 0 2', 'only Risotto is zeroed')
      assert.equal(await disabled('Wyzeruj', 'Risotto'), true, 'nothing left to zero')
      assert.equal(await disabled('Wyzeruj', 'Carbonara'), false, 'the other rows still can')
    },
  },
  {
    name: 'stock edited on a later page is kept where the dish is stored',
    seed: 'busyStockroom',
    run: async ({ click, fill, ev, text, assert }) => {
      assert.includes(await text(), '1–10 z 300', 'ten of three hundred')
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].map(r => r.cells[0].innerText).slice(0, 3)`), ['Affogato', 'Affogato z gorgonzolą', 'Affogato z grzybami'], 'by name')
      await click('Następna strona')
      assert.includes(await text(), '11–20 z 300', 'the second page')
      await fill('Porcje', '41', 'Arancini')
      await click('Przybyła porcja', 'Arancini')
      await click('Poprzednia strona')
      assert.excludes(await text(), 'Arancini', 'the first page again')
      await click('Następna strona')
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].find(r => r.cells[0].innerText === 'Arancini').querySelector('[name="Porcje"]').value`), '42', 'typed, then one more')
    },
  },
  {
    name: 'the search and the group narrow the stock to the dishes they name',
    seed: 'busyStockroom',
    run: async ({ click, fill, ev, text, assert }) => {
      await fill('Szukaj', 'tiramisu')
      assert.includes(await text(), '1–10 z 10', 'ten kinds of tiramisu')
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].every(r => r.cells[0].innerText.includes('Tiramisu'))`), true, 'only they are listed')
      await fill('Szukaj', 'zzz')
      assert.includes(await text(), 'Żadne danie nie pasuje', 'no dish answers')
      await fill('Szukaj', '')
      await click('Zupy')
      assert.includes(await text(), '1–10 z 20', 'two soups in ten variations')
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].every(r => /Minestrone|Ribollita/.test(r.cells[0].innerText))`), true, 'only they are listed')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop',
    seed: 'busyStockroom',
    run: fits,
  },
]
