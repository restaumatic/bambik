// Measures the 7GUIs corpus for doc/7guis-comparison.md.
//
//   node scripts/count-7guis.mjs demo/7guis/*-mdc2/*.purs
//
// Counting rules, identical to every corpus in that document: blank and
// comment lines stripped, every per-task source file counted (business
// modules included), CSS excluded, cl100k BPE via gpt-tokenizer.
//
// gpt-tokenizer is not a repo dependency (it is only needed to redo this
// measurement). Install it anywhere and point GPT_TOKENIZER at that
// directory, or `npm i gpt-tokenizer` here and it resolves by itself:
//
//   npm i --no-save gpt-tokenizer && node scripts/count-7guis.mjs demo/7guis/*-mdc2/*.purs
import { readFileSync } from 'node:fs'
import { createRequire } from 'node:module'

const req = createRequire(process.env.GPT_TOKENIZER
  ? `${process.env.GPT_TOKENIZER}/`
  : import.meta.url)
const { encode } = req('gpt-tokenizer')

const stripPurs = src => src.split('\n')
  .filter(l => l.trim() !== '' && !l.trim().startsWith('--'))
  .join('\n')

export function measure(path) {
  const stripped = stripPurs(readFileSync(path, 'utf8'))
  return { lines: stripped.split('\n').length, tokens: encode(stripped).length, chars: stripped.length }
}

const files = process.argv.slice(2)
let tt = 0, tc = 0, tl = 0
for (const f of files) {
  const m = measure(f)
  tt += m.tokens; tc += m.chars; tl += m.lines
  console.log(`${m.tokens}\t${m.chars}\t${m.lines}\t${f}`)
}
console.log(`${tt}\t${tc}\t${tl}\tTOTAL`)
