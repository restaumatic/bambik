---
name: developing-bambik-apps
description: How to devlop Profunctor User Interfaces Web applications featuring HTML/MDC2/MDC3/Shoelace/Fluent/Bootstrap with Bambik library: Bootstraping, writing, reviewing, building, bundling, running. With idiomatic code style, refering to Bambik demo applications. This skill is standalone — copy its directory and use it anywhere.
---

# Developing bambik applications

A bambik application is one profunctor pipeline: every UI component is a
profunctor over its carrier, displaying an input and emitting an
output, and the app composes them with qualified-do pipelines (editors
are whole-row pipeline stages) and the
four row merges. It is a pair of modules with a one-way dependency: a
**view module** exporting a single entry function named after the
application (never `main`), mounted at the document body, importing the
design system and the logic module; and a **logic module** of pure
business functions and values, depending only on the domain. The rows
the pipeline operates over are a **presentation model**: displays are
verbatim (no leaf takes a formatter), so everything the user reads —
formatted readouts, composed sentence lines — is a model field the
logic module writes and unit tests (writing.md, *displays are
verbatim*). The
smallest complete example is the helloworld demo (all view, so a single
module); the counter demo is the smallest one with a model and a logic
module.

Work through the three procedures in order. Each is a file in this
skill's directory:

1. **[bootstrap.md](bootstrap.md)** — scaffold the application. It
   creates, from nothing but node + git + network, a directory that
   builds, bundles and runs locally, with bambik as an ordinary tagged
   spago package (no repo to clone, nothing vendored). Covers the forked
   compiler, updating and pinning, and troubleshooting. Its **first step
   is the design system** — MDC2, MDC3, Shoelace, Fluent, Bootstrap or
   plain HTML — which the developer chooses; ask if they have not said.

2. **[writing.md](writing.md)** — write the app modules. The pipeline and
   the four merges, component citizenship and the adopters, pass-through
   stages, app shapes with the demo that shows each, conditional
   visibility, modals, collections, separation of concerns, the type
   inference gotchas, and what to do when data does not propagate (the
   starvation watchdog and the emission trace) — closing with **Code
   style**, the definitive contract for bambik application code (layout,
   types and values, business functions, wiring). That section is
   normative: the library's own documentation points at it rather than
   restating it, and the demos are its executable form.

   Two companions ride with it: **[walkthrough.md](walkthrough.md)**,
   one mid-size demo (flight-booker) read line by line — the first thing
   to read after the counter — and **[vocabulary.md](vocabulary.md)**, the
   lookup index from what the screen needs to the word for it and the
   place its rule is stated. Neither states a rule of its own.

3. **[building.md](building.md)** — build, run and verify. The npm
   scripts, the watch-mode agent loop, and browser verification.

All three end at the same place: the application **running in dev mode**
— `npm run watch` and `npm run dev` in the background, the page verified
in a browser, its URL reported to the developer. A green build is not a
finished task; bambik apps are DOM-driven and the knowledge gates that
silence a pane are invisible to the compiler.

Spago clones the library whole, so after the first build
**`.spago/bambik/<tag>/`** holds its sources, demos and docs as worked
examples: `demo/7guis/`, `demo/nguis/`, and the module headers under
`src/` that document the API. Paths below are
written relative to that directory.
