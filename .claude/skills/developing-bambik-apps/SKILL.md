---
name: developing-bambik-apps
description: How to devlop Profunctor User Interfaces Web applications featuring HTML/MDC2/MDC3/Shoelace/Fluent/Bootstrap with Bambik library: Bootstraping, writing, reviewing, building, bundling, running. With idiomatic code style, refering to Bambik demo applications. This skill is standalone — copy its directory and use it anywhere.
---

# Developing bambik applications

A bambik application is one profunctor pipeline: every widget is a
profunctor over its carrier, displaying an input and emitting an
output, and the app composes them with qualified-do pipelines and the
four row merges. It is a standalone module exporting a single entry
function named after the application, never `main`, mounted at the
document body. The smallest complete example is the helloworld demo;
the counter demo is the smallest one with a model.

Work through the three procedures in order. Each is a file in this
skill's directory:

1. **[bootstrap.md](bootstrap.md)** — scaffold the application. It
   creates, from nothing but node + git + network, a directory that
   builds, bundles and runs locally, with bambik as an ordinary tagged
   spago package (no repo to clone, nothing vendored). Covers the forked
   compiler, updating and pinning, and troubleshooting. Its **first step
   is the design system** — MDC2, MDC3, Shoelace, Fluent, Bootstrap or
   plain HTML — which the developer chooses; ask if they have not said.

2. **[writing.md](writing.md)** — write the app module. The pipeline and
   the four merges, component citizenship and the adopters, pass-through
   stages, app shapes with the demo that shows each, conditional
   visibility, modals, collections, separation of concerns, the type
   inference gotchas, and what to do when data does not propagate (the
   starvation watchdog and the emission trace) — closing with **Code
   style**, the definitive contract for bambik application code (layout,
   types and values, business functions, wiring). That section is
   normative: the library's own documentation points at it rather than
   restating it, and the demos are its executable form.

3. **[building.md](building.md)** — build, run and verify. The npm
   scripts, the watch-mode agent loop, and browser verification.

Spago clones the library whole, so after the first build
**`.spago/bambik/<tag>/`** holds its sources, demos and docs as worked
examples: `demo/7guis/`, `demo/nguis/`, `doc/type-errors.md`, and the
module headers under `src/` that document the API. Paths below are
written relative to that directory.
