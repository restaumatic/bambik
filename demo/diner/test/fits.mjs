// A part fits its viewport at phone, tablet and desktop widths: the page
// does not scroll sideways and no data table scrolls inside its frame.
export const fits = async ({ resize, ev, assert }) => {
  for (const width of [360, 768, 1280]) {
    await resize(width)
    assert.equal(await ev(`document.documentElement.scrollWidth <= innerWidth`), true, `the page fits ${width}px`)
    assert.equal(await ev(`[...document.querySelectorAll('.md3-data-table')].filter(t => t.scrollWidth > t.clientWidth + 1).length`), 0, `every table fits ${width}px`)
  }
}
