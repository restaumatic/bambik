// The container action's gather gate (Data.Profunctor.Acting `acted`): the
// menu is withheld until every guest row has emitted a choice, re-emits whole
// on any re-choice (retain-last), and row instances follow their keys.
export const demos = ['demo/nguis/potluck-mdc2']
export const url = '/demo/nguis/potluck-mdc2/'

const pick = (row, label) =>
  `(() => { const sb = document.querySelectorAll('.mdc-segmented-button')[${row}]; if (!sb) return false; const seg = [...sb.querySelectorAll('.mdc-segmented-button__segment')].find(s => s.textContent.includes('${label}')); if (!seg) return false; seg.click(); return true })()`

const menuText = `(document.querySelector('h6') || { textContent: '' }).textContent`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(`document.querySelectorAll('.mdc-segmented-button').length`), 4, 'four guest rows built')
  assertEq(await ev(menuText), 'On the table: ', 'gather gate: only the static prefix before anyone chose')

  await ev(`(() => { window.__rows = [...document.querySelectorAll('.mdc-segmented-button')]; return true })()`)

  assertEq(await ev(pick(0, 'Salad')), true, 'Ada picks')
  assertEq(await ev(pick(1, 'Lasagna')), true, 'Grace picks')
  assertEq(await ev(pick(2, 'Pavlova')), true, 'Edsger picks')
  await sleep(100)
  assertEq(await ev(menuText), 'On the table: ', 'gather gate: withheld while one guest is undecided')

  assertEq(await ev(pick(3, 'Salad')), true, 'Barbara picks')
  await sleep(100)
  const menu = await ev(menuText)
  assertEq(
    menu.includes('Ada’s Salad') && menu.includes('Grace’s Lasagna') && menu.includes('Edsger’s Pavlova') && menu.includes('Barbara’s Salad'),
    true,
    'menu completes on the last voice (' + menu + ')'
  )

  await ev(`(() => { window.__frags = [...document.querySelectorAll('h6 span')]; return true })()`)

  assertEq(await ev(pick(0, 'Pavlova')), true, 'Ada re-picks')
  await sleep(100)
  const menu2 = await ev(menuText)
  assertEq(menu2.includes('Ada’s Pavlova'), true, 'retain-last: a re-choice re-emits the whole menu (' + menu2 + ')')
  assertEq(
    await ev(`(() => { const now = [...document.querySelectorAll('h6 span')]; return now.length === window.__frags.length && now.every((n, i) => n === window.__frags[i]) })()`),
    true,
    'partial update: the menu is keyed fragments — spans survive a re-choice'
  )

  assertEq(
    await ev(`[...document.querySelectorAll('.mdc-segmented-button')].every((el, i) => el === window.__rows[i])`),
    true,
    'identity follows key: row nodes survive every re-feed'
  )
}
