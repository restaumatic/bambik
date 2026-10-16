// The view-model rule (writing.md, Types and values): a view-model row is
// records, variants, primitives and Array — no Maybe, and no Boolean unless a
// Boolean editor edits it. This scans the demos' view-model modules for `:: Maybe`
// and `:: Boolean` field types and reports every one not on the allow-list.
import { readFileSync, readdirSync, statSync } from "node:fs";
import { join } from "node:path";
import { viewModelFiles } from "./demos.mjs";

const isViewModel = (file) => viewModelFiles.has(file);

const walk = (dir) => readdirSync(dir).flatMap((n) => {
  const p = join(dir, n);
  return statSync(p).isDirectory() ? walk(p) : p.endsWith(".purs") ? [p] : [];
});

// Boolean fields edited by a Boolean editor.
const allow = new Set([
  ...['"Favorite"', '"Payment protection insurance"', '"Extra shot"', '"Decaf"', '"Uppercase letters"',
      '"Symbols"', '"Lowercase letters"', '"Include a Teams link"', '"Digits"', '"Takeaway cup"',
      "\"I'd recommend it to a friend\"", '"Oscar"', '"Mark as favorite"', '"Cult"', '"Classic"']
    .map((l) => `${l} :: Boolean`),
]);

const fieldRe = /((?:"[^"]+"|[A-Za-z_][A-Za-z0-9_']*) :: (?:Maybe|Boolean))\b/g;
const hits = [];
for (const file of walk("demo")) {
  if (!isViewModel(file)) continue;
  const src = readFileSync(file, "utf8");
  src.split("\n").forEach((line, i) => {
    // a top-level `name :: Maybe …` is a function signature, not a row field —
    // Maybe below the UI is what the rule permits
    if (/^[A-Za-z_][A-Za-z0-9_']* ::/.test(line)) return;
    for (const m of line.matchAll(fieldRe)) {
      if (!allow.has(m[1].replace(/\\'/g, "'"))) hits.push(`${file}:${i + 1}: ${m[1]}`);
    }
  });
}
if (hits.length) {
  console.error("view-model rule violations (Maybe/Boolean field off the allow-list):");
  for (const h of hits) console.error("  " + h);
  process.exit(1);
}

// Copy is a function, not a field (doc/research-copy-is-a-function.md): a
// display's copy is a named logic function read at the leaf, so the old
// view-side read adopters `projection`/`projected` must not appear anywhere in
// demo code.
const bannedRe = /\b(projection|projected)\b/;
const banned = [];
for (const file of walk("demo")) {
  const src = readFileSync(file, "utf8");
  src.split("\n").forEach((line, i) => {
    const m = line.match(bannedRe);
    if (m) banned.push(`${file}:${i + 1}: ${m[1]}`);
  });
}
if (banned.length) {
  console.error("presentation-model rule violations (view-side read adopter in demo code):");
  for (const b of banned) console.error("  " + b);
  process.exit(1);
}
// Copy is a function, not a field: the read `text` takes is a NAMED function
// living in the logic module, never a lambda composing copy at the view
// site — that is the whole point of the rule.
const lambdaRe = /\btext \(?\\/;
const lambdas = [];
for (const file of walk("demo")) {
  if (isViewModel(file)) continue;
  const src = readFileSync(file, "utf8");
  src.split("\n").forEach((line, i) => {
    if (lambdaRe.test(line)) lambdas.push(`${file}:${i + 1}: ${line.trim()}`);
  });
}
if (lambdas.length) {
  console.error("copy-is-a-function violations (lambda in a `text` read — name it in the logic module):");
  for (const l of lambdas) console.error("  " + l);
  process.exit(1);
}
// A field is named by its label, never picked by a function: a view reads a
// stored field through the word's own label slot (`textAt @"title"`,
// `clicked @l @"key"`, `# providedAt @"halted" @"phase"`, `actedAt`, `anyOfAt`,
// `listOfAt`, `attrAt`), so an accessor section in view code is a field read
// that skipped its label — and anything more than one field is a named
// function in the view model module.
const accessorRe = /(?<![\w)\]])_\.(?:"[^"]+"|[A-Za-z][\w']*)/;
const accessors = [];
for (const file of walk("demo")) {
  if (isViewModel(file)) continue;
  const src = readFileSync(file, "utf8");
  src.split("\n").forEach((line, i) => {
    if (accessorRe.test(line)) accessors.push(`${file}:${i + 1}: ${line.trim()}`);
  });
}
if (accessors.length) {
  console.error("field-label violations (accessor section in view code — name the field in the word's label slot):");
  for (const a of accessors) console.error("  " + a);
  process.exit(1);
}
// No silent information loss (guardrails L13): a `foreach` element and a
// `clicked` face release into nothing — the list passes the fed row on, the
// click reports a field — so an editor inside either loses its edits. The
// type cannot say so (a display and an editor are both `{ | r } → { | r }`),
// so the view is checked: an editor word must not appear in the balanced
// expression a `# foreach` trails or a `clicked @l @k` applies to. A list
// of editors over a stored array is `actedAt`; one over a derived array has
// no array to write back to.
const editorWords = ["input", "textArea", "rangeInput", "select", "selectUnpicked", "selectOptional",
  "filledTextField", "outlinedTextField", "filledTextArea", "outlinedTextArea", "textField",
  "slider", "sliderLive", "checkbox", "toggleSwitch", "radioButton", "radioGroup",
  "segmentedButton", "segmentedButtonUnpicked", "segmentedButtonOptional",
  "dropdown", "dropdownUnpicked", "dropdownOptional", "tabBar", "rating", "iconToggle", "filterChip", "group"];
const editorRe = new RegExp(`(?<![\\w.'])(?:${editorWords.join("|")})(?= @)`);
// the parenthesized expression ending at `end` (exclusive), or the line's
// head when the element is not parenthesized
const balancedBefore = (src, end) => {
  let i = end - 1;
  while (i >= 0 && /\s/.test(src[i])) i--;
  if (src[i] !== ")") return src.slice(src.lastIndexOf("\n", i) + 1, i + 1);
  let depth = 0;
  for (; i >= 0; i--) {
    if (src[i] === ")") depth++;
    else if (src[i] === "(" && --depth === 0) return src.slice(i, end);
  }
  return src.slice(0, end);
};
const balancedAfter = (src, start) => {
  let i = start, depth = 0;
  for (; i < src.length; i++) {
    if (src[i] === "(") depth++;
    else if (src[i] === ")" && --depth === 0) return src.slice(start, i + 1);
    else if (depth === 0 && src[i] === "\n") return src.slice(start, i);
  }
  return src.slice(start);
};
const lost = [];
for (const file of walk("demo")) {
  if (isViewModel(file)) continue;
  const src = readFileSync(file, "utf8");
  for (const m of src.matchAll(/# foreach @/g)) {
    const el = balancedBefore(src, m.index);
    const e = el.match(editorRe);
    if (e) lost.push(`${file}:${src.slice(0, m.index).split("\n").length}: ${e[0]} inside a foreach element`);
  }
  for (const m of src.matchAll(/\bclicked @"[^"]+" @"[^"]+"\s*\$?\s*/g)) {
    const el = balancedAfter(src, m.index + m[0].length);
    const e = el.match(editorRe);
    if (e) lost.push(`${file}:${src.slice(0, m.index).split("\n").length}: ${e[0]} inside a clicked face`);
  }
}
if (lost.length) {
  console.error("silent-loss violations (an editor where its edits go nowhere — a stored array is `actedAt`):");
  for (const l of lost) console.error("  " + l);
  process.exit(1);
}
// The view restates no model row (guardrails L18): a row the view model's
// signatures state — the model's, a classifier's cases, a projection's
// element row, a payload — is read from them, never declared on a view line.
// A view's type arguments are labels, and nothing else: an action's outcomes
// are its function's signature, closed by what tells them.
const restated = [];
for (const file of walk("demo")) {
  if (isViewModel(file)) continue;
  const src = readFileSync(file, "utf8");
  for (const m of src.matchAll(/@(?=[({A-Z_])/g)) {
    const line = src.slice(0, m.index).split("\n").length;
    const row = balancedAfter(src, m.index + 1);
    restated.push(`${file}:${line}: @${row.split("\n")[0]}`);
  }
}
if (restated.length) {
  console.error("restated-row violations (a row the view model's signatures state, declared in the view):");
  for (const r of restated) console.error("  " + r);
  process.exit(1);
}
console.log("view-model rule: clean");
