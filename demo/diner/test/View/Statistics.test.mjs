import { fits } from '../fits.mjs'

// Statystyki: dishes sold in paid orders, the best selling first, under
// their total.
export const tests = [
  {
    name: 'the dish that sold for the most first, the totals on top',
    seed: 'dinnerService',
    run: async ({ ev, assert }) => {
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].map(r => [...r.cells].map(c => c.innerText).join(' | '))`),
        ['Łącznie | 8 | 77,50 zł', 'Margherita | 4 | 38,00 zł', 'Carbonara | 2 | 21,00 zł', 'Risotto | 1 | 12,00 zł', 'Tiramisu | 1 | 6,50 zł'], 'rows')
      assert.equal(await ev(`[...document.querySelectorAll('tr')].every(r => getComputedStyle(r.cells[2]).textAlign === 'right')`), true, 'the amounts against the right edge, heading and total included')
    },
  },
  {
    name: 'nothing sold yet',
    seed: 'emptyDiner',
    run: async ({ text, assert }) => assert.includes(await text(), 'Nic jeszcze nie sprzedano', 'the note'),
  },
  {
    name: 'ten dishes to a page, the totals over every page',
    seed: 'busyStatistics',
    run: async ({ click, ev, assert }) => {
      const amounts = () => ev(`[...document.querySelectorAll('tbody tr')].slice(1).map(r => parseFloat(r.cells[2].innerText.replace(',', '.')))`)
      const total = () => ev(`document.querySelector('tbody tr').innerText`)
      const before = await total()
      const first = await amounts()
      assert.equal(first.length, 10, 'ten dishes under the totals')
      assert.equal(first.every((a, i) => i === 0 || a <= first[i - 1]), true, 'the best selling first')
      await click('Następna strona')
      assert.equal((await amounts())[0] <= first[9], true, 'and on down the next page')
      assert.equal(await total(), before, 'the totals do not turn')
    },
  },
  {
    name: 'the search and the group narrow the sales, and the totals with them',
    seed: 'busyStatistics',
    run: async ({ click, fill, ev, text, assert }) => {
      const dishes = () => ev(`[...document.querySelectorAll('tbody tr')].slice(1).map(r => r.cells[0].innerText)`)
      const total = () => ev(`document.querySelector('tbody tr').innerText`)
      const all = await total()
      await fill('Szukaj w statystykach', 'truf')
      assert.includes(await text(), '1–10 z 26', 'the dishes with truffles that sold')
      assert.equal((await dishes()).every(d => d.includes('z truflami')), true, 'only they are listed')
      assert.equal((await total()) !== all, true, 'the totals are theirs')
      await fill('Szukaj w statystykach', '')
      assert.equal(await total(), all, 'the search cleared, every sale again')
      await click('Desery')
      assert.includes(await text(), '1–10 z 44', 'the desserts that sold')
      assert.equal((await dishes()).every(d => /^(Tiramisu|Panna cotta|Cannoli|Affogato|Zabaglione)/.test(d)), true, 'only desserts are listed')
      await fill('Szukaj w statystykach', 'zzz')
      assert.includes(await text(), 'Nie ma takich dań', 'no dish answers')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop',
    seed: 'busyStatistics',
    run: fits,
  },
]
