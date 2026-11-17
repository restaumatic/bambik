// Headless Chrome for the harnesses (scripts/smoke/run.mjs, scripts/part-test.mjs):
// discovered from $BAMBIK_CHROME, google-chrome, chromium or
// chromium-browser, launched headless on an ephemeral debug port with an
// isolated throwaway profile — never the user's own browser.
import { spawn, spawnSync } from 'node:child_process'
import { existsSync, readFileSync } from 'node:fs'
import path from 'node:path'

const findChrome = () => {
  const candidates = [process.env.BAMBIK_CHROME, 'google-chrome', 'chromium', 'chromium-browser'].filter(Boolean)
  for (const c of candidates) {
    try {
      const { status } = spawnSyncQuiet(c)
      if (status === 0) return c
    } catch { /* keep looking */ }
  }
  console.error('No Chrome found — install google-chrome/chromium or set $BAMBIK_CHROME')
  process.exit(1)
}

const spawnSyncQuiet = c => spawnSync('which', [c], { stdio: 'ignore' })

export const launchChrome = async profileDir => {
  const chrome = findChrome()
  const proc = spawn(chrome, [
    '--headless=new', '--remote-debugging-port=0', `--user-data-dir=${profileDir}`,
    '--no-first-run', '--no-default-browser-check', 'about:blank',
  ], { stdio: 'ignore' })
  const portFile = path.join(profileDir, 'DevToolsActivePort')
  for (let i = 0; i < 150; i++) {
    await new Promise(r => setTimeout(r, 100))
    if (existsSync(portFile)) {
      const port = readFileSync(portFile, 'utf8').split('\n')[0].trim()
      if (port) return { proc, cdpBase: `http://127.0.0.1:${port}` }
    }
  }
  proc.kill()
  throw new Error('Chrome did not open its DevTools port within 15s')
}

