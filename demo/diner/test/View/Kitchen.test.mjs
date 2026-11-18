// Kuchnia: tickets in the order they were sent; a ticket is ready once
// every line is plated.
export const tests = [
  {
    name: 'tickets in the order they were sent',
    seed: 'rushHour',
    run: async ({ ev, assert }) => {
      assert.equal(await ev(`[...document.querySelectorAll('.md3-card')].map(c => c.innerText.split(' · ')[0])`), ['#11', '#12', '#13', '#14'], 'oldest first')
    },
  },
  {
    name: 'plating the last line offers "Zamówienie gotowe", which clears the ticket',
    seed: 'rushHour',
    run: async ({ click, text, assert }) => {
      assert.excludes(await text(), 'Zamówienie gotowe', 'not before every line is plated')
      await click('2 × Tiramisu', '#11')
      await click('Zamówienie gotowe', '#11')
      assert.excludes(await text(), '#11', 'the ticket left the kitchen')
    },
  },
  {
    name: 'an empty kitchen says so',
    seed: 'emptyDiner',
    run: async ({ text, assert }) => assert.includes(await text(), 'Wszystko ugotowane', 'the note'),
  },
]
