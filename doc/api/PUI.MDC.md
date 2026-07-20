## Module PUI.MDC

#### `OptLabelIcon`

``` purescript
data OptLabelIcon
  = OptLabelIcon
```

##### Instances
``` purescript
ConvertOption OptLabelIcon "label" String (Maybe String)
ConvertOption OptLabelIcon "icon" String (Maybe String)
ConvertOption OptLabelIcon sym a a
```

#### `OptLabel`

``` purescript
data OptLabel
  = OptLabel
```

##### Instances
``` purescript
ConvertOption OptLabel "label" String (Maybe String)
ConvertOption OptLabel "caption" String (Maybe String)
ConvertOption OptLabel sym a a
```

#### `OptIcon`

``` purescript
data OptIcon
  = OptIcon
```

##### Instances
``` purescript
ConvertOption OptIcon "icon" String (Maybe String)
ConvertOption OptIcon sym a a
```

#### `OptSelected`

``` purescript
data OptSelected
  = OptSelected
```

##### Instances
``` purescript
ConvertOption OptSelected sym a a
```

#### `OptStep`

``` purescript
data OptStep
  = OptStep
```

##### Instances
``` purescript
ConvertOption OptStep "step" Number (Maybe Number)
ConvertOption OptStep sym a a
```

#### `banner`

``` purescript
banner :: PUI Web [ event :: String ] (Record ())
```

The `+→×` status receiver in banner clothing: shows message case `l`
in an MDC banner, contributing no fields. Unlike the auto-dismissing
snackbar it stays until its own Dismiss action (foundation-handled).

#### `body1`

``` purescript
body1 :: Ocular (PUI Web)
```

#### `body2`

``` purescript
body2 :: Ocular (PUI Web)
```

#### `button`

``` purescript
button :: forall provided r. ConvertOptionsWithDefaults OptLabelIcon { icon :: Maybe String, label :: Maybe String } (Record provided) { icon :: Maybe String, label :: Maybe String } => Record provided -> PUI Web (Record r) [ clicked :: Record r ]
```

The `×→+` event button: reads the whole record it is shown and fires it
as event case `l` on click (`recordToCase` over the raw button). Both
fields are optional and default to `Nothing`: `button {}` is bare,
`button { label: "Count" }` labels it, `icon: "add"` adds an icon.

#### `caption`

``` purescript
caption :: Ocular (PUI Web)
```

#### `card`

``` purescript
card :: forall provided. ConvertOptionsWithDefaults OptLabel { caption :: Maybe String } (Record provided) { caption :: Maybe String } => Record provided -> Ocular (PUI Web)
```

A card with an optional caption — the caption is design-system config
(like `filledTextField`'s `floatingLabel`). The card is content-agnostic
(any polarity), so its caption chrome is hand-fused, not merged. The
caption defaults to none: `card {}` is captionless, `card { caption:
"Title" }` labels it.

#### `cardActions`

``` purescript
cardActions :: Ocular (PUI Web)
```

The MD2 card button-row area: a flex row for a group of buttons, so they
sit inline at their natural width instead of stretching down the card's
flex column. Wrap a button group: `cardActions $ RecordToVariant.do …`.

#### `checkbox`

``` purescript
checkbox :: forall a. Default a => PUI Web (Record ()) (Record ()) -> PUI Web { value :: Maybe a } { value :: Maybe a }
```

Label content is chrome (`{} → {}`, announcing).

#### `chipSet`

``` purescript
chipSet :: Ocular (PUI Web)
```

Chrome for a group of `filterChip @l`s.

#### `dataCell`

``` purescript
dataCell :: Ocular (PUI Web)
```

#### `dataRow`

``` purescript
dataRow :: Ocular (PUI Web)
```

#### `dataTable`

``` purescript
dataTable :: { columns :: Array String, label :: String } -> Ocular (PUI Web)
```

Table chrome with a static header from config; rows are `dataRow`s of
`dataCell`s.

#### `dialog`

``` purescript
dialog :: { title :: String } -> Ocular (PUI Web)
```

Modal ocular with the open-on-feed/close-on-emission protocol: the
dialog opens (via the MDC foundation — animation, scrim, Esc) whenever
a value is fed, and closes when its content emits, so feed it
selectively (behind an event case), put the deciding emitters inside,
and the emission both closes the dialog and flows on. The content's
final stage must emit only on decision (buttons, `clicked`) — an
echoing display there would close the dialog the moment it opens.

#### `divider`

``` purescript
divider :: PUI Web (Record ()) (Record ())
```

#### `drawer`

``` purescript
drawer :: forall i o. { subtitle :: String, title :: String } -> PUI Web i o -> PUI Web i o -> PUI Web i o
```

Permanent navigation drawer beside the content; the drawer's own nav
is chrome (`{} → {}`, e.g. a `list` of `listItem`s).
The permanent drawer with a **live nav slot**: nav and content are
sibling stages over the same types — both see every value fed, and
either side's emissions exit the drawer, so a selectable nav (a
`listOf` of sections folded via `updates`) drives the content beside
it. Static chrome nav embeds via `muted`.

#### `elevation1`

``` purescript
elevation1 :: Ocular (PUI Web)
```

#### `elevation10`

``` purescript
elevation10 :: Ocular (PUI Web)
```

#### `elevation20`

``` purescript
elevation20 :: Ocular (PUI Web)
```

#### `fab`

``` purescript
fab :: forall provided r. ConvertOptionsWithDefaults OptLabel { label :: Maybe String } (Record provided) { icon :: String, label :: Maybe String } => Record provided -> PUI Web (Record r) [ clicked :: Record r ]
```

The `×→+` event FAB: like `button @l`, reads the whole record it is
shown and fires it as event case `l` on click. `icon` is required; a
`label` (bare string) makes it the extended FAB.

#### `filledTextArea`

``` purescript
filledTextArea :: { columns :: Int, rows :: Int } -> PUI Web { value :: String } { value :: String }
```

#### `filledTextField`

``` purescript
filledTextField :: { floatingLabel :: String } -> PUI Web { value :: String } { value :: String }
```

#### `debouncedTextField`

``` purescript
debouncedTextField :: { floatingLabel :: String, millis :: Milliseconds } -> PUI Web { value :: String } { value :: String }
```

`filledTextField` over the debounced input leaf: keystrokes coalesce
at the DOM boundary (`Web.inputDebounced`), so the field is loop-safe
to debounce — the wire itself stays synchronous.

#### `filterChip`

``` purescript
filterChip :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
```

The MD2 filter chip, a `×→×` `Boolean` editor. Selection styling is
CSS-class-driven. Group chips in the `chipSet` ocular.

#### `headline1`

``` purescript
headline1 :: Ocular (PUI Web)
```

#### `headline2`

``` purescript
headline2 :: Ocular (PUI Web)
```

#### `headline3`

``` purescript
headline3 :: Ocular (PUI Web)
```

#### `headline4`

``` purescript
headline4 :: Ocular (PUI Web)
```

#### `headline5`

``` purescript
headline5 :: Ocular (PUI Web)
```

#### `headline6`

``` purescript
headline6 :: Ocular (PUI Web)
```

#### `iconButton`

``` purescript
iconButton :: forall r. { icon :: String, label :: String } -> PUI Web (Record r) [ clicked :: Record r ]
```

The `×→+` event icon button (the MD2 icon button; for the toggling
variant see the `×→×` editor `iconToggle @l`).

#### `iconToggle`

``` purescript
iconToggle :: { label :: String, offIcon :: String, onIcon :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
```

The MD2 icon button (toggle variant), a `×→×` `Boolean` editor —
`onIcon` shows while `true`, `offIcon` while `false`.

#### `imageList`

``` purescript
imageList :: { columns :: Int } -> Ocular (PUI Web)
```

Masonry image list; the prebuilt MDC CSS leaves column layout to a
SCSS mixin, so it rides in an inline style here.

#### `imageListItem`

``` purescript
imageListItem :: { label :: String, src :: String } -> PUI Web (Record ()) (Record ())
```

#### `indeterminateCircularProgress`

``` purescript
indeterminateCircularProgress :: PUI Web { busy :: Boolean } (Record ())
```

`indeterminateLinearProgress`'s circular sibling — the same
`{ busy } → {}` display citizen.

#### `indeterminateLinearProgress`

``` purescript
indeterminateLinearProgress :: PUI Web { busy :: Boolean } (Record ())
```

The `×→×` display citizen for async progress: `{ busy } → {}`, the
shape `PUI.action`'s progress slot expects.

#### `linearProgress`

``` purescript
linearProgress :: PUI Web { value :: Number } (Record ())
```

The **determinate** linear progress display, a `{ value :: Number } → {}`
display citizen: `value` is the filled fraction (0.0–1.0). The gauge
shape: `linearProgress # projection fraction # forValue`.

#### `layoutCell`

``` purescript
layoutCell :: { span :: Int } -> Ocular (PUI Web)
```

#### `layoutGrid`

``` purescript
layoutGrid :: Ocular (PUI Web)
```

#### `list`

``` purescript
list :: Ocular (PUI Web)
```

#### `listItem`

``` purescript
listItem :: Ocular (PUI Web)
```

#### `listOf`

``` purescript
listOf :: forall provided a o. ConvertOptionsWithDefaults OptSelected { selected :: a -> Boolean } (Record provided) { selected :: a -> Boolean } => Record provided -> PUI Web a o -> PUI Web (Array a) a
```

The MD2 list as a **dynamic collection component**: one item widget per
array element, rebuilt per value fed; items satisfying `selected` get
the MD2 selected styling (optional — `listOf {}` selects nothing);
every item is a click emitter replaying its own value, so the
component's output is the clicked item.

#### `menu`

``` purescript
menu :: { label :: String } -> Ocular (PUI Web)
```

Anchor button plus menu surface around a merge of `menuItem @l`s; the
menu closes itself on item selection.

#### `menuItem`

``` purescript
menuItem :: forall r. { label :: String } -> PUI Web (Record r) [ clicked :: Record r ]
```

The `×→+` event list item for the `menu` ocular: fires the record it
is shown as event case `l` on click (the menu closes itself).

#### `overline`

``` purescript
overline :: Ocular (PUI Web)
```

#### `radioButton`

``` purescript
radioButton :: forall a. Eq a => Array { label :: String, value :: a } -> PUI Web { value :: Maybe a } { value :: a }
```

The MD2 radio group, a `×→×` editor. Type-changing like `select @l`:
the input field holds the selection state (`Maybe a`), the output
field the bare selection (`a`). One radio per option; the shared
native `name` gives browser-level exclusivity and the CSS keys off
`:checked`, so each option's emission is its statically known value.

#### `segmentedButton`

``` purescript
segmentedButton :: forall a. Eq a => Array { label :: String, value :: a } -> PUI Web { value :: Maybe a } { value :: a }
```

The MD2 single-select segmented button, a `×→×` editor. Type-changing
like `select @l`; selection styling is CSS-class-driven, so the
wiring is hand-rolled per segment.

#### `select`

``` purescript
select :: forall a. Eq a => { floatingLabel :: String } -> Array { label :: String, value :: a } -> PUI Web { value :: Maybe a } { value :: a }
```

The MD2 exposed dropdown menu, a `×→×` editor. Type-changing like
`radioButton @l`: the input field holds the selection state
(`Maybe a`), the output field the bare selection (`a`). Options are
design-system config.

#### `simpleDialog`

``` purescript
simpleDialog :: { confirm :: String, title :: String } -> Ocular (PUI Web)
```

`dialog` with a built-in confirm action: same open-on-feed protocol,
and the confirm button is a `clicked` pass-through — clicking it
emits the content's last output (so give displays a `# tapped`),
which closes the dialog and flows on.

#### `slider`

``` purescript
slider :: forall provided. ConvertOptionsWithDefaults OptStep { label :: String, step :: Maybe Number } (Record provided) { label :: String, max :: Number, min :: Number, step :: Maybe Number } => Record provided -> PUI Web { value :: Number } { value :: Number }
```

The `×→×` `Number` editor. An optional `step` makes it the discrete slider.
Emits on **commit** only (thumb release): one emission per adjustment,
so an `updates` fold sees each drag as a single transaction. For
continuous mid-drag emissions (live readouts), use `sliderLive`.

#### `sliderLive`

``` purescript
sliderLive :: forall provided. ConvertOptionsWithDefaults OptStep { label :: String, step :: Maybe Number } (Record provided) { label :: String, max :: Number, min :: Number, step :: Maybe Number } => Record provided -> PUI Web { value :: Number } { value :: Number }
```

`slider` emitting continuously mid-drag (like mid-typing text); a
consumer that doesn't want the burst wraps its stage in `debounced`.

#### `snackbar`

``` purescript
snackbar :: PUI Web [ event :: String ] (Record ())
```

The `+→×` status receiver: shows message case `l` in a snackbar,
contributing no fields (`text` echoes its `{}`, so it announces).

#### `subtitle1`

``` purescript
subtitle1 :: Ocular (PUI Web)
```

#### `subtitle2`

``` purescript
subtitle2 :: Ocular (PUI Web)
```

#### `tabBar`

``` purescript
tabBar :: forall provided a. Eq a => ConvertOptionsWithDefaults OptIcon { icon :: Maybe String } (Record provided) { icon :: Maybe String, label :: String, value :: a } => Array (Record provided) -> PUI Web { value :: a } { value :: a }
```

The MD2 tab bar, a `×→×` editor like `segmentedButton @l` but
**same-type** (`Cons l a () s`): the selection is always known from the
input, so it echoes unconditionally and sits happily inside `looped`
ensembles (selection field + `provided` payload panes). One tab per option;
`MDCTabBar` drives activation — indicator transitions, `aria-selected`,
and arrow-key navigation come from the foundation.

#### `toggleSwitch`

``` purescript
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
```

The MD2 Switch, a `×→×` `Boolean` editor (the name `switch` was
already taken by the `+→+` case selector).

#### `tooltip`

``` purescript
tooltip :: { text :: String } -> Ocular (PUI Web)
```

Attach a hover/focus tooltip to the wrapped element (single-element
content: the anchor is the content's root node).

#### `topAppBar`

``` purescript
topAppBar :: { title :: String } -> Ocular (PUI Web)
```


