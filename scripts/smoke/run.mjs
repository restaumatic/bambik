#!/usr/bin/env node
// Bambik smoke harness: serves the repo over HTTP, drives demos in headless
// Chrome via CDP, and runs the assertions in tests/*.mjs. This is where the
// carrier-only laws get their tests — `Sequencing`'s keyed reconciliation
// (identity follows the key), the empty/singleton collection laws, the
// quiescence-driven event flow — alongside per-demo smokes; the value-level
// laws live in test/Main.purs (`spago test`).
//
//   npm run smoke              # all tests (bundle the demos first:
//                              #   npm run bundle-demo-nguis && npm run bundle-demo-7guis)
//   npm run smoke -- reorder   # only test files whose name matches
//
// Chrome is discovered from $BAMBIK_CHROME, google-chrome, chromium, or
// chromium-browser, and always launched headless on an ephemeral debug port
// with an isolated throwaway profile — never the user's own browser.
import { spawn, spawnSync } from 'node:child_process'
import { createServer } from 'node:http'
import { existsSync, mkdtempSync, readFileSync, readdirSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'
import { openSession } from './cdp.mjs'

const here = path.dirname(fileURLToPath(import.meta.url))
const root = path.resolve(here, '../..')
const filter = process.argv[2]

const contentTypes = {
  '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css',
  '.json': 'application/json', '.svg': 'image/svg+xml', '.purs': 'text/plain',
}

const serve = () => new Promise(resolve => {
  const server = createServer((req, res) => {
    let file = path.join(root, decodeURIComponent(new URL(req.url, 'http://x').pathname))
    if (file.endsWith(path.sep) || (existsSync(file) && readdirSyncSafe(file))) file = path.join(file, 'index.html')
    try {
      const body = readFileSync(file)
      res.writeHead(200, { 'content-type': contentTypes[path.extname(file)] || 'application/octet-stream' })
      res.end(body)
    } catch {
      res.writeHead(404); res.end('not found')
    }
  })
  server.listen(0, '127.0.0.1', () => resolve(server))
})

const readdirSyncSafe = dir => {
  try { return readdirSync(dir).length >= 0 } catch { return false }
}

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

const launchChrome = async profileDir => {
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

const main = async () => {
  const testsDir = path.join(here, 'tests')
  const testFiles = readdirSync(testsDir).filter(f => f.endsWith('.mjs'))
    .filter(f => !filter || f.includes(filter)).sort()
  if (testFiles.length === 0) {
    console.error(filter ? `no test matches "${filter}"` : 'no tests found')
    process.exit(1)
  }

  // every demo a selected test needs must be bundled
  const missing = new Set()
  const mods = []
  for (const f of testFiles) {
    const mod = await import(pathToFileURL(path.join(testsDir, f)))
    mods.push({ name: f.replace(/\.mjs$/, ''), mod })
    for (const demo of mod.demos) {
      if (!existsSync(path.join(root, demo, 'bundle.js'))) missing.add(demo)
    }
  }
  if (missing.size) {
    console.error('missing bundles:\n' + [...missing].map(d => `  ${d}/bundle.js`).join('\n'))
    console.error('bundle first: npm run bundle-demo-nguis && npm run bundle-demo-7guis')
    process.exit(1)
  }

  const server = await serve()
  const base = `http://127.0.0.1:${server.address().port}`
  const profileDir = mkdtempSync(path.join(tmpdir(), 'bambik-smoke-'))
  const { proc, cdpBase } = await launchChrome(profileDir)

  let failures = 0
  try {
    for (const { name, mod } of mods) {
      console.log(`\n▶ ${name} (${mod.url})`)
      const session = await openSession(cdpBase, base + mod.url)
      await new Promise(r => setTimeout(r, 1200)) // page + MDC init
      const assertEq = (actual, expected, label) => {
        const ok = JSON.stringify(actual) === JSON.stringify(expected)
        console.log(`  ${ok ? 'PASS' : 'FAIL'} ${label}${ok ? '' : ` — expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`}`)
        if (!ok) failures++
      }
      const sleep = ms => new Promise(r => setTimeout(r, ms))
      try {
        await mod.run({ ev: session.ev, assertEq, sleep })
      } catch (e) {
        console.log(`  FAIL ${name} crashed: ${e.message}`)
        failures++
      }
      await session.close()
    }
  } finally {
    const gone = new Promise(r => proc.once('exit', r))
    proc.kill()
    await Promise.race([gone, new Promise(r => setTimeout(r, 3000))])
    server.close()
    rmSync(profileDir, { recursive: true, force: true, maxRetries: 5, retryDelay: 200 })
  }

  console.log(failures ? `\n✖ ${failures} failure(s)` : '\n✓ all smoke tests passed')
  process.exit(failures ? 1 : 0)
}

main()
