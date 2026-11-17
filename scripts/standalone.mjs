#!/usr/bin/env node
// A demo as a bare standalone application: its bundle and a page with only
// its design system's stylesheets — no demo chrome, no source listing —
// written to dist/<app>/ (gitignored), ready to copy to a web root.
//
//   node scripts/standalone.mjs diner      # bundle the demos first
//   npm run deploy-diner                   # this, then copied to xyz:/var/www/html/diner/
import { copyFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import path from 'node:path'
import { all } from './demos.mjs'

const name = process.argv[2]
const demo = all.find(d => d.name === name) ?? all.find(d => d.family === name && d.variant)
if (!demo) { console.error(`no demo "${name}" in scripts/demos.mjs`); process.exit(1) }
if (!existsSync(demo.bundle)) { console.error(`no ${demo.bundle}: npm run bundle-demos first`); process.exit(1) }

const page = readFileSync(path.join(demo.dir, 'index.html'), 'utf8')
const head = page.match(new RegExp(`<template data-variant="${demo.variant}"[^>]*>([\\s\\S]*?)</template>`))?.[1].trim() ?? ''
const title = page.match(/<span id="page-title">([^<]*)<\/span>/)?.[1] ?? demo.family
const out = `dist/${demo.family}`
mkdirSync(out, { recursive: true })
copyFileSync(demo.bundle, `${out}/bundle.js`)
writeFileSync(`${out}/index.html`, `<!doctype html>
<html lang="pl">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>${title}</title>
    ${head}
    <style>
      body { margin: 0 auto; max-width: 960px; padding: 16px; box-sizing: border-box; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; font-family: Roboto, sans-serif; background: var(--md-sys-color-surface, #fef7ff); color: var(--md-sys-color-on-surface, #1d1b20); }
    </style>
    <script type="module" src="bundle.js"></script>
  </head>
  <body></body>
</html>
`)
console.log(`${out}/index.html and bundle.js (${demo.mod}.${demo.fn})`)
