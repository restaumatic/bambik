import { fits } from '../fits.mjs'

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
  {
    name: 'ten tickets to a page, each with the minutes it has waited; the oldest is overdue',
    seed: 'busyKitchen',
    run: async ({ click, ev, text, assert }) => {
      const waits = () => ev(`[...document.querySelectorAll('.md3-card .md-typescale-label-large')].map(w => parseInt(w.innerText))`)
      assert.includes(await text(), '1–10 z 20', 'ten of twenty')
      const first = await waits()
      assert.equal(first.length, 10, 'ten tickets')
      assert.equal(first.every((w, i) => i === 0 || w <= first[i - 1]), true, 'oldest first')
      assert.equal(await ev(`getComputedStyle(document.querySelector('.md3-card .md-typescale-label-large')).fontWeight`), '700', 'the oldest wait is marked')
      await click('Następna strona')
      assert.includes(await text(), '11–20 z 20', 'the rest')
      assert.equal((await waits())[0] <= first[9], true, 'younger than the first page')
    },
  },
  {
    name: 'fits a phone, a tablet and a desktop',
    seed: 'busyKitchen',
    run: fits,
  },
]
