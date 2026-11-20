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
    name: 'two hundred and nineteen receipts, ten to a page, the oldest placed yesterday',
    seed: 'busyReport',
    run: async ({ click, disabled, ev, text, assert }) => {
      const numbers = () => ev(`[...document.querySelectorAll('tbody tr')].slice(1).map(r => r.cells[0].innerText.split(' ')[0])`)
      assert.includes(await text(), '1–10 z 219', 'ten of two hundred and nineteen')
      assert.equal(await ev(`document.querySelector('tbody tr').cells[1].innerText`), '18 268,50 zł', 'the takings, their thousands set apart')
      assert.equal(await ev(`[...document.querySelectorAll('tr')].every(r => getComputedStyle(r.cells[1]).textAlign === 'right')`), true, 'the amounts against the right edge')
      assert.equal(await numbers(), ['#219', '#218', '#217', '#216', '#215', '#214', '#213', '#212', '#211', '#210'], 'newest first')
      for (let i = 0; i < 21; i++) await click('Następna strona')
      assert.includes(await text(), '211–219 z 219', 'the last page')
      assert.equal((await numbers())[8], '#1', 'the first order last')
      assert.includes(await text(), 'wczoraj', 'placed the day before')
      assert.equal(await disabled('Następna strona'), true, 'nothing after it')
      await click('Poprzednia strona')
      assert.includes(await text(), '201–210 z 219', 'and back')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop',
    seed: 'busyReport',
    run: fits,
  },
]
