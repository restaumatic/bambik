# Building, running and verifying a bambik application

The workflow is the scaffold's npm scripts, set up by
[bootstrap.md](bootstrap.md) — which also describes the scaffold's files
themselves, including the page the app mounts into.

**Dev mode is where every piece of work ends.** Bootstrapping an app,
adding a stage, fixing a business function — none of it is finished at a
green compile. Leave the app running at its URL, verified in a browser,
and say so to the developer.

## Build and run

1. **Agent loop: use watch mode.** Keep `npm run watch` (`spago build -w`)
   running in the background and read its output after each edit
   (~0.7s incremental) instead of one-shot `npm run build`s. Two
   caveats: spago -w reads stdin and dies on EOF, so keep stdin open
   (never `</dev/null`), and only one watcher may own `output/` at a
   time.

2. **Serve.** `npm run dev` serves the app at `http://127.0.0.1:8000/`,
   with esbuild rebundling from `output/` on request — refresh the
   browser after an edit. It dies on stdin EOF like the watcher. Change
   the port in package.json if 8000 is busy.

   The two together are dev mode: the watcher turns edits into `output/`,
   the server turns `output/` into the page. Start both in the
   background, keep them up for the whole session, and verify the running
   page as below.

3. **Bundle.** `npm run bundle` writes the minified
   `public/bundle.js`. The whole of `public/` is then the deployable
   artifact: static files, no server. This is a deploy step — it is not
   part of the dev loop, and it is not what concludes a task.

## Verify

The compiler proves the wiring; it does not prove the app works. bambik
apps are DOM-driven, so verify in a browser: HTTP 200 on the page and on
`/bundle.js`, the app rendered inside `<body>`, no console errors. The
app mounts asynchronously — poll for a rendered element rather than
sampling once after a fixed delay.

A headless check is worth writing once the app has more than one stage.
The library's own demos are covered by a Chrome CDP harness at
`.spago/bambik/<tag>/scripts/smoke/`, whose `cdp.mjs` session helper is a usable
model; its `tests/*.mjs` files show how a walk through a demo is
written.

When the page loads but the data does not arrive, the problem is in the
app module, not the build: see **When it does not propagate** in
[writing.md](writing.md#when-it-does-not-propagate) for the starvation
watchdog and the emission trace.
