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
    name: 'Wyzeruj zeroes every dish once confirmed, then is disabled',
    seed: 'dinnerService',
    run: async ({ click, disabled, ev, text, assert }) => {
      const counts = () => ev(`[...document.querySelectorAll('[name="Porcje"]')].map(f => f.value).join(' ')`)
      await click('Wyzeruj')
      assert.includes(await text(), 'Wyzerować porcje wszystkich dań?', 'the dialog asks')
      await click('Nie, nie zeruj')
      assert.equal(await counts(), '6 8 3 2', 'the stock is kept')
      await click('Wyzeruj')
      await click('Wyzeruj', 'Wyzerować')
      assert.equal(await counts(), '0 0 0 0', 'the stock is zeroed')
      assert.equal(await disabled('Wyzeruj'), true, 'nothing left to zero')
    },
  },
]
