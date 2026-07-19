
# Profunctor User Interfaces

This is a prototype of the idea of *profunctor user interfaces* for Web/Material Design Component-based UIs written in [PureScript](https://www.purescript.org/).

[Why Bambik? — a long-form introduction](/doc/why-bambik.md)

[1000 characters-long description of the idea](/doc/description-1000characters.md)

[20 minutes-long presentation of the idea](/doc/presentation-20min.md)

[Row profunctors over records and variants (design note)](/doc/row-profunctors.md)

[Variant syntax sugar — the forked PureScript compiler this repo builds on](/doc/variant-sugar.md)

# Demo

In order to run the demo:

```bash
$ npm install
```

Demo of [Material Design Component-based UI](demo/1/Main.purs):

```bash
$ npm run demo-1
```

Demo of [plain HTML-based UI](demo/nguis/restaurant-menu/RestaurantMenu.purs) — a fancy static page with no Material components, part of the nGUIs set:

```bash
$ npm run bundle-demo-nguis
```
