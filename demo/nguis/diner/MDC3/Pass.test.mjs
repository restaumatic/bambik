// Wydawka: ready orders wait for payment.
export const tests = [
  {
    name: 'a paid bill leaves the pass',
    seed: 'rushHour',
    run: async ({ click, text, assert }) => {
      assert.includes(await text(), '11:50', 'the ready order with its time')
      await click('Zapłacono')
      assert.includes(await text(), 'Wszystko wydane', 'nothing left to pay')
    },
  },
]
