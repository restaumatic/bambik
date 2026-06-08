# Showcase: a checkout form as pure optics

A reactive **checkout form** — a shopper's fields (`email`, `cardNumber`, `amount`),
each editing → firing a change → validating → showing status — expressed **only as
optics**: no UI, no effects, no carrier. `p` stays abstract, so the logic is
carrier-independent. It type-checks (`spago build`); it is not meant to be *run*.

- [Logic.purs](./Logic.purs) — the whole showcase: a `textInput` field widget and
  `checkoutFlow`, the form that merges three of them. No type annotations: the widget's
  closed row (`Cons l a () r`) pins every step to its field, and the field types unify
  from `checkoutFlow`'s endpoints.

## The unit is the field, not the operation

A form is a **merge of field widgets**; each field widget runs its own lifecycle. That's
why there's one merge (the form) and the four optic families live *inside* each widget —
not four separate do-blocks all spanning the same three fields.

`textInput @l` is a `Semigroupoid.do` (`>>>`) flow over the single field `l`, one optic
family per step:

```
Record ──Lens──▶ Record ──Shutter──▶ Variant ──Prism──▶ Variant ──Reel──▶ Record
  (edit l)         (l changed)        (validate l)        (status l)
```

| step | optic | direction | DDD role | built on |
|---|---|---|---|---|
| edit | `Lens` | × → × | Value Object accessor ("has-a") | `editProperty` |
| change | `Shutter` | × → + | **Process / Saga** | `shutterE` |
| validate | `Prism` | + → + | Value Object discriminator ("is-a") | `editCase` |
| status | `Reel` | + → × | **Entity / Aggregate** | `reelE` |

## The form and submit

`form` is one `RecordToRecord.do` merge of the field widgets — each field listed once,
its whole lifecycle encapsulated. `checkout` then *flows* the form into a submit
`button` that fires the completed form as one `submit` event (× → +):

```purescript
form = RecordToRecord.do
  textInput @"email"
  textInput @"cardNumber"
  textInput @"amount"

checkout = Semigroupoid.do      -- p (Record …) (Variant ( submit :: Record … ))
  form
  button @"submit"
```

Three axes of composition: **flow** inside each field widget (`Semigroupoid.do`, the
four optic families end-to-end), **merge** across the form (`RecordToRecord.do`,
combining the fields), and **flow** again at the top (form `>>>` submit). No annotations
— the closed rows pin each step; the field types unify from the endpoints.

### Why a closed row (and not inline annotations)

`editProperty`/`editCase`/… are *open-rest* (`Cons l a r s`, `r` free) so they compose
onto any record — but that openness leaves a leaf's row ambiguous in a merge. `textInput`
fixes it once, at the widget, with a **closed tail `()`** (`Cons l a () r`): every step is
pinned to the single field/case `l`, so no call-site annotation is needed.

The full optic vocabulary (the focus and wrap combinators — `focusRecord`,
`focusVariant`, `resolveProperty`, `retainCase`, `shutterWrap`, `reelWrap`, `lensE`,
`prismE`, …) is documented in [`doc/row-profunctors.md`](../doc/row-profunctors.md).
