// What the scenarios share: the rows to a page the application ships with,
// when a station counts as drawn, and the gestures every station needs.
export const perPage = { list: 10, tiles: 24 }

// a page expression true once the part has drawn its day
export const drawn = {
  Stock: `document.querySelector('tbody tr') !== null`,
  Hall: `document.querySelector('.md3-tile') !== null`,
  Kitchen: `document.querySelector('.md3-card') !== null`,
  Pass: `document.querySelector('tbody tr') !== null`,
  Report: `document.querySelector('tbody tr') !== null`,
  Statistics: `document.querySelector('tbody tr') !== null`,
  App: `document.querySelector('.md3-tile') !== null`,
  Service: `document.querySelector('.md3-tile') !== null`,
}

export const stationMount = (part, scale = 1, extra = {}) => ({ name: `${part}x${scale}${extra.suffix ?? ''}`, kind: 'station', part, scale, perPage: extra.perPage ?? perPage, clock: extra.clock })

// page expressions for the things clicked
export const dialogButton = label => `([...(document.querySelector('md-dialog[open]')?.querySelectorAll('[slot=actions] *') ?? [])].find(b => b.textContent.trim() === ${JSON.stringify(label)}) ?? null)`
export const tableTile = number => `([...document.querySelectorAll('.md3-tile')].find(t => (t.getAttribute('aria-label') || '').startsWith('Stolik ${number} ·')) ?? null)`
export const dishTile = name => `([...document.querySelectorAll('.md3-tile')].find(t => t.querySelector('.md-typescale-label-large')?.innerText === ${JSON.stringify(name)}) ?? null)`
export const rowOf = (table, starts) => `([...document.querySelectorAll('table[aria-label="${table}"] tbody tr')].find(r => r.cells[0].innerText.trim().startsWith(${JSON.stringify(starts)})) ?? null)`
export const inRow = (table, starts, label) => `([...(${rowOf(table, starts)}?.querySelectorAll('md-icon-button, md-filled-button') ?? [])].find(b => (b.getAttribute('aria-label') || b.innerText.trim()) === ${JSON.stringify(label)}) ?? null)`
export const fieldIn = (table, starts, name) => `(${rowOf(table, starts)}?.querySelector('[name="${name}"]') ?? null)`
export const field = name => `window.__bench.field(${JSON.stringify(name)})`
export const tab = label => `([...document.querySelectorAll('md-primary-tab')].find(t => window.__bench.visible(t) && t.innerText.trim() === ${JSON.stringify(label)}) ?? null)`
export const pagerButton = label => `([...document.querySelectorAll('md-icon-button')].find(b => window.__bench.visible(b) && b.getAttribute('aria-label') === ${JSON.stringify(label)}) ?? null)`

// answer the dialog that is open with the button captioned `label`
export const answer = async (p, label) => {
  await p.waitFor(`document.querySelector('md-dialog[open]') !== null && (${dialogButton(label)}) !== null`, 3000)
  await p.sleep(120)
  await p.clickOn(dialogButton(label))
  await p.waitFor(`document.querySelector('md-dialog[open]') === null`, 3000)
}

// replace what a field holds: focus it, select everything, type
export const retype = async (p, expr, text) => {
  await p.clickOn(expr)
  await p.send('Input.dispatchKeyEvent', { type: 'keyDown', key: 'a', code: 'KeyA', modifiers: 2, windowsVirtualKeyCode: 65 })
  await p.send('Input.dispatchKeyEvent', { type: 'keyUp', key: 'a', code: 'KeyA', modifiers: 2, windowsVirtualKeyCode: 65 })
  if (text === '') await p.key('Backspace', 'Backspace', 8)
  else await p.send('Input.insertText', { text })
}

export const letter = (p, ch) => p.key(ch, `Key${ch.toUpperCase()}`, ch.toUpperCase().charCodeAt(0))
export const digit = (p, d) => p.key(d, `Digit${d}`, 48 + +d)
export const backspace = p => p.key('Backspace', 'Backspace', 8)
