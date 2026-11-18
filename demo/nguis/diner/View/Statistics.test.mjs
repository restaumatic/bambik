// Statystyki: dishes sold in paid orders, alphabetically, under their total.
export const tests = [
  {
    name: 'dishes alphabetically, the totals on top',
    seed: 'dinnerService',
    run: async ({ ev, assert }) => {
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].map(r => [...r.cells].map(c => c.innerText).join(' | '))`),
        ['Łącznie | 8 | 77,50 zł', 'Carbonara | 2 | 21,00 zł', 'Margherita | 4 | 38,00 zł', 'Risotto | 1 | 12,00 zł', 'Tiramisu | 1 | 6,50 zł'], 'rows')
    },
  },
  {
    name: 'nothing sold yet',
    seed: 'emptyDiner',
    run: async ({ text, assert }) => assert.includes(await text(), 'Nic jeszcze nie sprzedano', 'the note'),
  },
]
