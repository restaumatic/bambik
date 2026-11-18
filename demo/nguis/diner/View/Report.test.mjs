// Raport: paid orders, newest first, under their total.
export const tests = [
  {
    name: 'paid orders newest first, the total on top',
    seed: 'dinnerService',
    run: async ({ ev, assert }) => {
      assert.equal(await ev(`[...document.querySelectorAll('tbody tr')].map(r => r.cells[0].innerText)`), ['Łącznie', '3', '2', '1'], 'rows')
      assert.equal(await ev(`document.querySelector('tbody tr').cells[4].innerText`), '77,50 zł', 'the total')
    },
  },
  {
    name: 'nothing paid yet',
    seed: 'emptyDiner',
    run: async ({ text, assert }) => assert.includes(await text(), 'Nic jeszcze nie sprzedano', 'the note'),
  },
]
