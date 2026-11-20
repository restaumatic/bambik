import { fits } from '../fits.mjs'

// Wydawka: ready orders wait for payment.
export const tests = [
  {
    name: 'a bill is paid once the payment is confirmed, and leaves the pass',
    seed: 'rushHour',
    run: async ({ click, text, assert }) => {
      assert.includes(await text(), '#9 · stolik 5', 'the ready order')
      assert.includes(await text(), '30 min', 'with the minutes since it was placed')
      await click('Zapłacono')
      assert.includes(await text(), 'Przyjąć 24,00 zł za zamówienie #9 ze stolika 5?', 'the payment is asked about first')
      await click('Nie, jeszcze nie zapłacono')
      assert.includes(await text(), '#9 · stolik 5', 'declined, the bill stays')
      await click('Zapłacono')
      await click('Zapłacono', 'Przyjąć')
      assert.includes(await text(), 'Wszystko wydane', 'confirmed, nothing is left to pay')
    },
  },
  {
    name: 'a bill paid on a full page: the page follows, the count goes down',
    seed: 'busyPass',
    run: async ({ click, ev, text, assert }) => {
      assert.includes(await text(), '1–10 z 10', 'ten bills')
      const waits = await ev(`[...document.querySelectorAll('tbody tr')].map(r => parseInt(r.cells[1].innerText))`)
      assert.equal(waits.every((w, i) => i === 0 || w <= waits[i - 1]), true, 'the longest waiting first')
      assert.equal(await ev(`[...document.querySelectorAll('tr')].every(r => getComputedStyle(r.cells[2]).textAlign === 'right')`), true, 'the amounts against the right edge')
      await click('Zapłacono', '#220')
      await click('Zapłacono', 'Przyjąć')
      assert.excludes(await text(), '#220', 'the paid bill left')
      assert.includes(await text(), '1–9 z 9', 'nine left')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop',
    seed: 'busyPass',
    run: fits,
  },
]
