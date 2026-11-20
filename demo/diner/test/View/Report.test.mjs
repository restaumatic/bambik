import { fits } from '../fits.mjs'

// Raport: paid orders, newest first, under their total.
export const tests = [
  {
    name: 'paid orders newest first, the total on top',
    seed: 'dinnerService',
    run: async ({ ev, assert }) => {
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].map(r => r.cells[0].innerText.split('\\n')[0])`),
        ['Łącznie', '#3 · stolik 1 · 8:41', '#2 · stolik 5 · 8:20', '#1 · stolik 2 · 8:05'], 'rows')
      assert.equal(await ev(`document.querySelector('tbody tr').cells[1].innerText`), '77,50\u00a0zł', 'the total')
    },
  },
  {
    name: 'nothing paid yet',
    seed: 'emptyDiner',
    run: async ({ text, assert }) => assert.includes(await text(), 'Nic jeszcze nie sprzedano', 'the note'),
  },
  {
    name: 'a hundred receipts, ten to a page, the oldest placed yesterday',
    seed: 'busyReport',
    run: async ({ click, disabled, ev, text, assert }) => {
      const numbers = () => ev(`[...document.querySelectorAll('tbody tr')].slice(1).map(r => r.cells[0].innerText.split(' ')[0])`)
      assert.includes(await text(), '1–10 z 100', 'ten of a hundred')
      assert.equal(await ev(`[...document.querySelectorAll('tr')].every(r => getComputedStyle(r.cells[1]).textAlign === 'right')`), true, 'the amounts against the right edge')
      assert.equal(await numbers(), ['#100', '#99', '#98', '#97', '#96', '#95', '#94', '#93', '#92', '#91'], 'newest first')
      for (let i = 0; i < 9; i++) await click('Następna strona')
      assert.includes(await text(), '91–100 z 100', 'the last page')
      assert.equal((await numbers())[9], '#1', 'the first order last')
      assert.includes(await text(), 'wczoraj', 'placed the day before')
      assert.equal(await disabled('Następna strona'), true, 'nothing after it')
      await click('Poprzednia strona')
      assert.includes(await text(), '81–90 z 100', 'and back')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop',
    seed: 'busyReport',
    run: fits,
  },
]
