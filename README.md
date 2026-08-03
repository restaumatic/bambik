
# Profunctor User Interfaces

This is a prototype of the idea of *profunctor user interfaces* for Web/Material Design Component-based UIs written in [PureScript](https://www.purescript.org/).

[Why Bambik? — a long-form introduction](/doc/why-bambik.md)

[1000 characters-long description of the idea](/doc/description-1000characters.md)

[20 minutes-long presentation of the idea](/doc/presentation-20min.md)

[Row profunctors over records and variants (design note)](/doc/row-profunctors.md)

[Variant syntax sugar — the forked PureScript compiler this repo builds on](/doc/variant-sugar.md)

# Using it in your own application

bambik is consumed as a spago git package pinned to a release tag — no clone,
nothing vendored. The [v0.1.4 release](https://github.com/restaumatic/bambik/releases/tag/v0.1.4)
ships **`developing-bambik-apps`**, the authoring skill: how to bootstrap a
project, the vocabulary and its four row directions, separation of concerns, the
definitive code-style contract, and the build/verify workflow — with a scaffold
that produces a working application, left running in dev mode.

Its `bootstrap.md` gives the `packages.dhall` entry and the two toolchain pins
the library requires (the variant-sugar compiler and the matching
`purescript-variant` fork; stock `purs` cannot build bambik).

```bash
mkdir -p .claude/skills
curl -sL https://github.com/restaumatic/bambik/releases/download/v0.1.4/developing-bambik-apps-v0.1.4.tar.gz | tar xz -C .claude/skills
```

Its source of truth is [.claude/skills/developing-bambik-apps/](/.claude/skills/developing-bambik-apps/);
[bootstrap.md](/.claude/skills/developing-bambik-apps/bootstrap.md) is readable on
its own if you would rather follow the procedure by hand.

# Demo

In order to run the demo:

```bash
$ npm install
```

Demo of [Material Design Component-based UI](demo/nguis/order-form/OrderForm.purs):

```bash
$ npm run dev order-form
```

Demo of [plain HTML-based UI](demo/nguis/restaurant-menu/RestaurantMenu.purs) — a fancy static page with no Material components, part of the nGUIs set:

```bash
$ npm run dev restaurant-menu
```
