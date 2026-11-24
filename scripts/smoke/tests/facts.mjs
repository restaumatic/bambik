// Facts of the host read beside the model (`geolocated`, `online`,
// `colorScheme`, `reducedMotion`, `pointer`, `viewport`, `language`,
// `timeZone`), on the real DOM (here-and-now, both twins): each line
// follows its fact as the platform changes it, emulated through CDP, with
// no model to turn. And an event that carries where the device was
// (`geolocating`, incident-report): the filed report keeps the place of
// its click.
import { byName } from '../../demos.mjs'

export const demos = ['here-and-now-mdc3', 'here-and-now-html', 'incident-report-mdc3']
export const pages = [
  { url: `/${byName('here-and-now-mdc3').page}`, label: 'here-and-now-mdc3' },
  { url: `/${byName('here-and-now-html').page}`, label: 'here-and-now-html' },
  { url: `/${byName('incident-report-mdc3').page}`, label: 'incident-report' },
]

const column = `document.querySelector('#demo-column')`
const lines = `${column}.innerText.split('\\n').map(l => l.trim()).filter(l => l)`
const line = start => `${lines}.find(l => l.startsWith(${JSON.stringify(start)}))`

// geolocation is refused until granted, and a refused watch ends: grant,
// then reload, so the page's first watch is answered
const locate = async (session, latitude, longitude, ready) => {
  const origin = await session.ev('location.origin')
  await session.send('Browser.grantPermissions', { origin, permissions: ['geolocation'] })
  await session.send('Emulation.setGeolocationOverride', { latitude, longitude, accuracy: 1 })
  await session.send('Page.reload')
  await new Promise(r => setTimeout(r, 500))
  for (let i = 0; i < 25 && !(await session.ev(`!!(${ready})`)); i++) await new Promise(r => setTimeout(r, 200))
}

const hereAndNow = async ({ ev, session, assertEq, sleep, page }) => {
  await locate(session, 52.23, 21.01, line('Your window'))
  await sleep(600)
  assertEq(await ev(line('You are at')), 'You are at 52.23, 21.01.', `[${page}] the position line follows the device`)
  await session.send('Emulation.setGeolocationOverride', { latitude: 50.06, longitude: 19.94, accuracy: 1 })
  await sleep(600)
  assertEq(await ev(line('You are at')), 'You are at 50.06, 19.94.', `[${page}] and keeps following it`)

  await session.send('Network.enable')
  await session.send('Network.emulateNetworkConditions', { offline: true, latency: 0, downloadThroughput: -1, uploadThroughput: -1 })
  await sleep(300)
  assertEq(await ev(line('You are o')), 'You are offline.', `[${page}] the connection line follows the browser offline`)
  await session.send('Network.emulateNetworkConditions', { offline: false, latency: 0, downloadThroughput: -1, uploadThroughput: -1 })
  await sleep(300)
  assertEq(await ev(line('You are o')), 'You are online.', `[${page}] and online again`)

  await session.send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value: 'dark' }, { name: 'prefers-reduced-motion', value: 'reduce' }] })
  await sleep(300)
  assertEq(await ev(line('You prefer the')), 'You prefer the dark side.', `[${page}] the scheme line follows the system setting`)
  assertEq(await ev(line('You prefer less')), 'You prefer less motion.', `[${page}] the motion line follows it too`)
  await session.send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value: 'light' }, { name: 'prefers-reduced-motion', value: 'no-preference' }] })
  await sleep(300)
  assertEq(await ev(line('You prefer the')), 'You prefer the light side.', `[${page}] and back`)
  assertEq(await ev(line('Motion')), 'Motion does not bother you.', `[${page}] both`)

  await session.send('Emulation.setDeviceMetricsOverride', { width: 600, height: 400, deviceScaleFactor: 1, mobile: false })
  await sleep(300)
  assertEq(await ev(line('Your window')), 'Your window is 600 × 400 pixels.', `[${page}] the window line follows a resize`)
  await session.send('Emulation.clearDeviceMetricsOverride')

  await session.send('Emulation.setTimezoneOverride', { timezoneId: 'Asia/Tokyo' })
  await sleep(1300)
  assertEq((await ev(line('It is'))).endsWith(' in Asia/Tokyo.'), true, `[${page}] the time zone is read with the time`)

  assertEq(await ev(`${lines}.some(l => /^You (point|have nothing to point)/.test(l)) && ${line('Your browser speaks')} !== undefined`), true, `[${page}] the pointer and language lines read their facts`)
}

const incidentReport = async ({ ev, session, assertEq, sleep, page }) => {
  const button = `[...${column}.querySelectorAll('md-filled-button')].find(b => b.textContent.includes('File the report'))`
  await locate(session, 52.23, 21.01, button)
  await sleep(600)
  assertEq((await ev(line('Reporting'))).includes('from 52.23, 21.01'), true, `[${page}] the report line reads where the device is`)
  await ev(`${button}.click()`)
  await session.send('Emulation.setGeolocationOverride', { latitude: 50.06, longitude: 19.94, accuracy: 1 })
  await sleep(1500)
  const filed = await ev(`[...document.querySelectorAll('*')].map(e => e.textContent).find(t => t && t.startsWith('Your report, filed'))`)
  assertEq(filed?.includes('filed from 52.23, 21.01'), true, `[${page}] the filed report keeps where its click was, not where the device went (${filed})`)
  assertEq((await ev(line('Reporting'))).includes('from 50.06, 19.94'), true, `[${page}] while the line follows the device`)
}

export const run = ctx => (ctx.page === 'incident-report' ? incidentReport(ctx) : hereAndNow(ctx))
