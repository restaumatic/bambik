// The view-model rule (writing.md, Types and values): a view-model row is
// records, variants, primitives and Array — no Maybe, and no Boolean unless a
// Boolean editor edits it. This scans the demos' logic modules for `:: Maybe`
// and `:: Boolean` field types and reports every one not on the allow-list.
import { readFileSync, readdirSync, statSync } from "node:fs";
import { join } from "node:path";

const walk = (dir) => readdirSync(dir).flatMap((n) => {
  const p = join(dir, n);
  return statSync(p).isDirectory() ? walk(p) : p.endsWith(".purs") ? [p] : [];
});

// Boolean fields edited by a Boolean editor, plus the one leaf-protocol
// exception — each carries its reason.
const allow = new Set([
  // potluck: the type-changing selector's *input protocol* (`Cons l (Maybe a)`)
  // used bare inside `acted` — the gather gate must wait for a genuine pick,
  // and `# optional`'s `unchosen` echo would open it early
  '"Dish" :: Maybe',
  ...['"Favorite"', '"Payment protection insurance"', '"Extra shot"', '"Decaf"', '"Uppercase letters"',
      '"Symbols"', '"Lowercase letters"', '"Include a Teams link"', '"Digits"', '"Takeaway cup"',
      "\"I'd recommend it to a friend\"", '"Oscar"', '"Mark as favorite"', '"Cult"', '"Classic"']
    .map((l) => `${l} :: Boolean`),
]);

const fieldRe = /((?:"[^"]+"|[A-Za-z_][A-Za-z0-9_']*) :: (?:Maybe|Boolean))\b/g;
const hits = [];
for (const file of walk("demo")) {
  if (!file.endsWith("Logic.purs")) continue;
  const src = readFileSync(file, "utf8");
  src.split("\n").forEach((line, i) => {
    // a top-level `name :: Maybe …` is a function signature, not a row field —
    // Maybe below the UI is what the rule permits
    if (/^[A-Za-z_][A-Za-z0-9_']* ::/.test(line)) return;
    for (const m of line.matchAll(fieldRe)) {
      if (!allow.has(m[1])) hits.push(`${file}:${i + 1}: ${m[1]}`);
    }
  });
}
if (hits.length) {
  console.error("view-model rule violations (Maybe/Boolean field off the allow-list):");
  for (const h of hits) console.error("  " + h);
  process.exit(1);
}
console.log("view-model rule: clean");
