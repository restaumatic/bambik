// The container action (Data.Profunctor.Acting `acted`) over per-guest dish
// `segmentedButtonUnpicked @"Dish" @"chosen"` selectors: every guest's
// choice is honest knowledge from the seed on, so the page says who is still
// choosing (the `waiting` case of `menuState`) and prints the menu once the
// table is complete (`complete`), a re-choice re-rendering it whole; row
// instances follow their keys.
export const demos = ['demo/nguis/potluck-mdc2']
export const url = '/demo/nguis/potluck-mdc2/'

const pick = (row, label) =>
  `(() => { const sb = document.querySelectorAll('.mdc-segmented-button')[${row}]; if (!sb) return false; const seg = [...sb.querySelectorAll('.mdc-segmented-button__segment')].find(s => s.textContent.includes('${label}')); if (!seg) return false; seg.click(); return true })()`

const menuText = `(document.querySelector('h6') || { textContent: '' }).textContent`
const waitingText = `([...document.querySelectorAll('*')].find(e => e.children.length === 0 && e.textContent.startsWith('Still choosing:')) || { textContent: '' }).textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('.mdc-segmented-button').length`), 4, 'four guest rows built')
  assertEq(await ev(menuText), '', 'no menu before anyone chose')
  assertEq(await ev(waitingText), 'Still choosing: Ada, Grace, Edsger, Barbara', 'the waiting pane names every guest')

  await ev(`(() => { window.__rows = [...document.querySelectorAll('.mdc-segmented-button')]; return true })()`)

  assertEq(await ev(pick(0, 'Salad')), true, 'Ada picks')
  assertEq(await ev(pick(1, 'Lasagna')), true, 'Grace picks')
  assertEq(await ev(pick(2, 'Pavlova')), true, 'Edsger picks')
  await sleep(100)
  assertEq(await ev(menuText), '', 'no menu while one guest is undecided')
  assertEq(await ev(waitingText), 'Still choosing: Barbara', 'the waiting pane names the one guest left')

  assertEq(await ev(pick(3, 'Salad')), true, 'Barbara picks')
  await sleep(100)
  const menu = await ev(menuText)
  assertEq(
    menu.includes('Ada’s Salad') && menu.includes('Grace’s Lasagna') && menu.includes('Edsger’s Pavlova') && menu.includes('Barbara’s Salad'),
    true,
    'menu completes on the last voice (' + menu + ')'
  )
  assertEq(await ev(waitingText), '', 'the waiting pane is gone once the table is complete')

  assertEq(await ev(pick(0, 'Pavlova')), true, 'Ada re-picks')
  await sleep(100)
  const menu2 = await ev(menuText)
  assertEq(menu2.includes('Ada’s Pavlova'), true, 'a re-choice re-renders the whole menu (' + menu2 + ')')

  assertEq(
    await ev(`[...document.querySelectorAll('.mdc-segmented-button')].every((el, i) => el === window.__rows[i])`),
    true,
    'identity follows key: row nodes survive every re-feed'
  )
}
