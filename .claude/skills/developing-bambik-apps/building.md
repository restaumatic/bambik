# Building, running and verifying a bambik application

Everything runs from the app directory with the scaffold's npm scripts
([bootstrap.md](bootstrap.md)). Commands assume
`export PATH=$PWD/node_modules/.bin:$PATH` and the scaffold's port 8000.

## Run

1. **Start dev mode** — two background processes, left running for the
   whole session:

   ```sh
   tail -f /dev/null | npm run watch > watch.log 2>&1 &
   tail -f /dev/null | npm run dev   > dev.log   2>&1 &
   ```

   `watch` (`spago build -w`) recompiles `src/` into `output/` on every
   save (under a second); `dev` (esbuild) serves `public/` at
   `http://127.0.0.1:8000/` and rebundles `bundle.js` from `output/` on
   each request. Both exit when their stdin closes, so the `tail -f
   /dev/null |` is required — never `</dev/null`. Run one watcher per
   directory: two processes writing `output/` corrupt each other's
   builds, so do not also run `npm run build` while `watch` is up.

2. **After each edit, read the watcher log** instead of building again:

   ```sh
   sleep 1; tail -n 30 watch.log
   ```

   A good build prints `Build succeeded.`; a bad one prints the
   compiler's error (file, line, message) followed by
   `[error] Failed to build.`. Either way the watcher then waits for the
   next save. An unused-dependencies warning is harmless. `dev.log`
   shows each request and any bundling error.

3. **Refresh the page** (or re-run *Verify*) to see the change.

## Verify

The compiler proves the wiring, not that the app shows data. The
check, used after bootstrapping and after every change:

> Page and bundle answer 200, the app is rendered inside `<body>`, and
> the console shows no errors or warnings, checked at least 3 s after
> load.

The 3 s matter: a merge waiting for a field with no value warns in the
console after 3 s ([writing.md](writing.md), *When it does not
propagate*).

```sh
curl -s -o /dev/null -w 'page %{http_code}\n'   http://127.0.0.1:8000/
curl -s -o /dev/null -w 'bundle %{http_code}\n' http://127.0.0.1:8000/bundle.js

google-chrome --headless=new --disable-gpu --no-first-run \
  --user-data-dir="$(mktemp -d)" --enable-logging=stderr --v=0 \
  --virtual-time-budget=5000 \
  --dump-dom http://127.0.0.1:8000/ > dom.html 2> chrome.log
sed -n '/<body/,/<\/body>/p' dom.html   # the app's elements, with their data
grep CONSOLE chrome.log                  # must print nothing
```

- Use whichever of `google-chrome`, `chromium`, `chromium-browser` is
  installed.
- `--virtual-time-budget=5000` runs the page's clock 5 s past load
  (instantly), so the check covers the 3 s warning.
- `dom.html`'s body must hold the app's elements and the seeded values
  (for the starter: an `<h4>` showing `0` and a `Count` button). An
  empty `<body>` means the bundle failed — read `dev.log` and the
  `CONSOLE` lines.
- Every `CONSOLE` line is a message the page logged (errors, warnings,
  and the emission trace if you turned it on — turn it off for the
  check).
- When the developer has a browser, give them the URL as well; an
  interaction the check cannot perform (a click, typing) is theirs to
  try, or yours in a scripted browser.

Report `http://127.0.0.1:8000/` to the developer with the servers left
running.

## Bundle for deploy

```sh
npm run bundle
```

Writes the minified `public/bundle.js` (about 0.5 MB for the starter).
`public/` is then the whole deployable site: static files, no server.
This is a deploy step, not part of the dev loop, and it does not end a
task.

## API reference

The module headers are the reference: `npm run docs` (`spago docs
--open`) generates and opens them as browsable HTML under
`generated-docs/html/`. The sources and the demos that use them are
under `.spago/bambik/<tag>/` (`src/`, `demo/7guis/`, `demo/nguis/`).
