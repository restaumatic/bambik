// The modal protocol: the dialog opens on feed (behind `provided` off a
// model flag) and closes on its deciding emission. Inbox's business rule
// confirms only the LAST message's deletion ("Delete the last message?"),
// so the test deletes down to one message first — exercising the immediate
// path — then triggers the dialog and Keeps.
export const demos = ['demo/nguis/inbox']
export const url = '/demo/nguis/inbox/'

const mailboxRows = `document.querySelector('.mdc-deprecated-list').querySelectorAll('.mdc-deprecated-list-item').length`
const openFirst = `document.querySelector('.mdc-deprecated-list .mdc-deprecated-list-item').click()`
const clickDelete = `[...document.querySelectorAll('.mdc-icon-button')].find(b => b.getAttribute('aria-label') === 'Delete message').click()`

export const run = async ({ ev, assertEq, sleep }) => {
  assertEq(await ev(mailboxRows), 3, 'mailbox starts with three messages')
  assertEq(await ev(`document.querySelectorAll('.mdc-dialog--open').length`), 0, 'dialog starts closed')

  for (const remaining of [2, 1]) {
    await ev(openFirst); await sleep(300)
    await ev(clickDelete); await sleep(400)
    assertEq(await ev(mailboxRows), remaining, `non-last message deletes immediately (${remaining} left)`)
    assertEq(await ev(`document.querySelectorAll('.mdc-dialog--open').length`), 0, 'no dialog for a non-last message')
  }

  await ev(openFirst); await sleep(300)
  await ev(clickDelete); await sleep(500)
  assertEq(await ev(`document.querySelectorAll('.mdc-dialog--open').length`), 1, 'the last message opens the confirm dialog (open on feed)')

  await ev(`[...document.querySelectorAll('.mdc-dialog--open button')].find(b => b.textContent.includes('Keep')).click()`)
  await sleep(600)
  assertEq(await ev(`document.querySelectorAll('.mdc-dialog--open').length`), 0, 'Keep closes the dialog (close on emission)')
  assertEq(await ev(mailboxRows), 1, 'the kept message survives')
}
