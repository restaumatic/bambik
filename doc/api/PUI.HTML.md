## Module PUI.HTML

The HTML vocabulary — a 1-1 correspondence with HTML: element oculars
(`div`, `p`, `ul`, `li`, `a`, ...), attribute/class decorators
(`attr`/`:=`, `cl`), the live leaves (`text`, `input`, `textArea`,
`button`, ...), announcing statics (`staticText`, `staticHTML`, the void
`hr` leaf), the `body` entry, and — for **structure computed from data at
runtime** without a markup DSL — `dynamic` (build a whole widget from the
fed value, rebuilt per feed; the single-value `foreach`). Grid cells emit
their own identity by wrapping each built cell in `clicked` over a
`lcmap (const key)` seed (`# clicked # lcmap (const key)`) — a per-cell
listener, no `data-*` attribute — with `onClickedXY` the pointer-coordinate
sibling for canvases. The carrier they are built over lives in `PUI.Web`.

#### `(:=)`

``` purescript
infixr 10 attr as :=
```

#### `(:=>)`

``` purescript
infixr 10 attrDyn as :=>
```

#### `a`

``` purescript
a :: Ocular (PUI Web)
```

#### `article`

``` purescript
article :: Ocular (PUI Web)
```

#### `aside`

``` purescript
aside :: Ocular (PUI Web)
```

#### `attr`

``` purescript
attr :: String -> String -> Ocular (PUI Web)
```

#### `attrDyn`

``` purescript
attrDyn :: String -> (Maybe Unit -> Maybe String) -> Ocular (PUI Web)
```

#### `attrWith`

``` purescript
attrWith :: forall i o. String -> (i -> String) -> PUI Web i o -> PUI Web i o
```

Value-computed attribute for the last-built element: the attribute is set
to `valueOf i` on every value fed. The channel-fed counterpart of the
static `attr`/`:=` — it lets **structure-from-data stay retaining**: a cell
whose style, SVG coordinate, or colour depends on data is built once and
updated in place through its channel (`circle >>> attrWith "cx" (show <<<
_.x)`, `div >>> attrWith "style" cellStyle`), rather than rebuilt wholesale
by a `dynamic`/`foreachWith` closure. Pair with `foreach` for a collection
that never tears its elements down.

#### `body`

``` purescript
body :: forall i o. PUI Web i o -> Effect Unit
```

The app entry: builds the widget in `<body>` and registers its
wiring — and feeds **nothing**. All initial data enters as seeds
(`with initial`, `announce`, `seeded`) inside the widget itself, so
the standalone app reads `body $ with initial $ ...`; emissions are
simply dropped.

#### `button`

``` purescript
button :: forall a. PUI Web (Record ()) (Record ()) -> PUI Web a a
```

Content is chrome (`{} → {}`, announcing): a button contains decoration
only; its wiring is the click emitter, replaying the last value fed.

#### `checkboxInput`

``` purescript
checkboxInput :: forall a. Default a => PUI Web (Maybe a) (Maybe a)
```

#### `cl`

``` purescript
cl :: String -> Ocular (PUI Web)
```

#### `clWhen`

``` purescript
clWhen :: forall i o. (i -> Boolean) -> String -> PUI Web i o -> PUI Web i o
```

Value-dependent class for the last-built element: the class is present
exactly while the predicate holds for the value fed — styling, not
visibility, so it stays a predicate (deliberately last-element-only:
a class spread over several siblings is rarely what is meant).

#### `clicked`

``` purescript
clicked :: forall i o. PUI Web i o -> PUI Web i i
```

Make the last-built element a click emitter: content is display, the
element replays the last value fed on click — `button`'s protocol for
any element (a click before any value arrived is withheld).

#### `clDyn`

``` purescript
clDyn :: String -> (Maybe Unit -> Boolean) -> Ocular (PUI Web)
```

#### `blockquote`

``` purescript
blockquote :: Ocular (PUI Web)
```

#### `code`

``` purescript
code :: Ocular (PUI Web)
```

#### `div`

``` purescript
div :: Ocular (PUI Web)
```

#### `dynamic`

``` purescript
dynamic :: forall a o. (a -> PUI Web (Record ()) o) -> PUI Web a o
```

The single-value case of `foreachWith`: rebuild one widget from the fed
value (a `foreachWith` over the one-element array). Owns its container:
`svg [...] $ dynamic renderScene`, `div $ dynamic renderSwatch`.

#### `each`

``` purescript
each :: forall a o. Array a -> (a -> PUI Web (Record ()) o) -> PUI Web (Record ()) o
```

Build a **fixed** (closure-known) list into the container now — a
`foreachWith` fed a constant array, input pinned to `{}` so it drops into a
`{} → {}` chrome merge without an annotation: `ul $ each rows renderRow`,
`tr $ each cells cellWidget`.

#### `el`

``` purescript
el :: String -> Ocular (PUI Web)
```

#### `em`

``` purescript
em :: Ocular (PUI Web)
```

#### `footer`

``` purescript
footer :: Ocular (PUI Web)
```

#### `foreach`

``` purescript
foreach :: forall a o. (a -> String) -> PUI Web a o -> PUI Web (Array a) o
```

The dynamic collection — the **runtime-sized homogeneous sequence merge**,
and the single collection combinator. **Keyed and retaining**: each element
is identified by `key a`, and on every fed array the collection reconciles
*by key* — matched elements are re-fed in place (their DOM kept), new keys
are built, absent keys removed, and the DOM reordered only when the key
sequence actually changed. So a fixed-key grid never rebuilds (values update
through the channel), a growing list only appends, and a reordered list
**moves each element's DOM node with it** — so browser-local state (focus,
scroll, selection) follows the item, not the position. Keys must be unique.

Written trailing, wrapped in a container ocular: `ul $ item # foreach _.key`.
It collapses every element's emission onto one shared channel `o` (the
homogeneous analogue of a variant-output merge), so as a terminal display it
cannot announce on an empty array by itself (parametricity: no `o` to
fabricate) — pass the carrier through with `# lcmap proj # displayed`, whose
unconditional echo *is* the sequence's announcing unit. This retention is the
row-merge gate lifted to a runtime, key-indexed vector of element instances
(`Retaining`/`Costrong` at collection granularity). See
doc/collections-sequence-merge.md.

#### `foreachWith`

``` purescript
foreachWith :: forall a o. (a -> PUI Web (Record ()) o) -> PUI Web (Array a) o
```

The **structure-from-value builder collection**: build a whole widget per
array element from the builder closure (tags/attributes as computed
strings — `el ("h" <> show level)`, `circle >>> "cx" := show c.x`). The
enclosing element is rebuilt wholesale on every value fed. Reach for it
(and its single-value case `dynamic`) only when an element's *structure*
genuinely varies with the data; when only *values* change over a fixed
structure, feed the structure through the keyed retaining `foreach` and
compute per-element attributes with `attrWith` (built once, updated in
place, no rebuild). Owns its container like `foreach`.

#### `h1`

``` purescript
h1 :: Ocular (PUI Web)
```

#### `h2`

``` purescript
h2 :: Ocular (PUI Web)
```

#### `h3`

``` purescript
h3 :: Ocular (PUI Web)
```

#### `h4`

``` purescript
h4 :: Ocular (PUI Web)
```

#### `h5`

``` purescript
h5 :: Ocular (PUI Web)
```

#### `h6`

``` purescript
h6 :: Ocular (PUI Web)
```

#### `header`

``` purescript
header :: Ocular (PUI Web)
```

#### `hr`

``` purescript
hr :: PUI Web (Record ()) (Record ())
```

The void `hr` element as announcing chrome (`{} → {}`): a self-closing
rule, no content ocular needed.

#### `i`

``` purescript
i :: Ocular (PUI Web)
```

#### `img`

``` purescript
img :: Ocular (PUI Web)
```

#### `init`

``` purescript
init :: forall a. (Node -> Effect a) -> (a -> Effect Unit) -> (a -> Effect Unit) -> Ocular (PUI Web)
```

#### `input`

``` purescript
input :: String -> PUI Web String String
```

Model updates never clobber the field the user is typing in: `toUser`
skips `setValue` while the node is focused (but still echoes, so merge
gates and downstream stages keep flowing). The channel stays live for
the field's whole life — an edited field resumes showing model updates
the moment it loses focus.

#### `inputDebounced`

``` purescript
inputDebounced :: Milliseconds -> String -> PUI Web String String
```

`input` with the DOM events debounced **at the leaf**: keystrokes are
coalesced before they enter the wire, so everything downstream of an
emission stays synchronous and `looped`'s re-entrancy guard still
terminates loop cycles. (Wire-level debouncing inside a loop turns
refeeds into a standing async ping-pong — the delay must sit in front
of the wire, not on it.)

#### `label`

``` purescript
label :: Ocular (PUI Web)
```

#### `li`

``` purescript
li :: Ocular (PUI Web)
```

#### `ol`

``` purescript
ol :: Ocular (PUI Web)
```

#### `onClickedXY`

``` purescript
onClickedXY :: forall i o. PUI Web i o -> PUI Web i { x :: Number, y :: Number }
```

Pointer-coordinate click emitter: emits the local/viewBox `{ x, y }` of a
click on the container (an `<svg>` gives viewBox coords). A container-level
emitter for canvases, where the coordinate is the payload: `svg [...] $
onClickedXY $ dynamic renderScene`.

#### `p`

``` purescript
p :: Ocular (PUI Web)
```

#### `radioButton`

``` purescript
radioButton :: forall a. Default a => PUI Web (Maybe a) a
```

#### `runWidgetInNode`

``` purescript
runWidgetInNode :: forall a b. Node -> a -> (b -> Effect Unit) -> PUI Web a b -> Effect Unit
```

#### `runWidgetInSelectedNode`

``` purescript
runWidgetInSelectedNode :: forall a b. String -> a -> (b -> Effect Unit) -> PUI Web a b -> Effect Unit
```

#### `section`

``` purescript
section :: Ocular (PUI Web)
```

#### `span`

``` purescript
span :: Ocular (PUI Web)
```

#### `staticHTML`

``` purescript
staticHTML :: String -> PUI Web (Record ()) (Record ())
```

See `staticText` — same announcing chrome typing.

#### `staticText`

``` purescript
staticText :: String -> PUI Web (Record ()) (Record ())
```

Static text as the announcing record unit with a face (`{} → {}`):
fixed DOM and, like `RecordToRecord.pempty`, it announces its
informationless `{}` on registration — so chrome composes as a gated
record-merge operand without starving anything.

#### `strong`

``` purescript
strong :: Ocular (PUI Web)
```

#### `table`

``` purescript
table :: Ocular (PUI Web)
```

#### `tbody`

``` purescript
tbody :: Ocular (PUI Web)
```

#### `td`

``` purescript
td :: Ocular (PUI Web)
```

#### `text`

``` purescript
text :: PUI Web { value :: String } (Record ())
```

#### `textArea`

``` purescript
textArea :: PUI Web String String
```

See `input` — same focus-guarded protocol.

#### `th`

``` purescript
th :: Ocular (PUI Web)
```

#### `thead`

``` purescript
thead :: Ocular (PUI Web)
```

#### `tr`

``` purescript
tr :: Ocular (PUI Web)
```

#### `transient`

``` purescript
transient :: Ocular (PUI Web)
```

#### `ul`

``` purescript
ul :: Ocular (PUI Web)
```

#### `provided`

``` purescript
provided :: forall a b. PUI Web a b -> PUI Web (Maybe a) b
```

The view-model conditional: visibility is the **presence of data**, not a
predicate. Feed `Just a` and the content is attached and fed `a`; feed
`Nothing` and it is detached. Pair with a named `Maybe`-valued business
projection — `pane # provided # lcmap currentQuestion` reads "shown,
provided there is a current question" — so the pane consumes the payload,
never the whole model, and the visibility logic lives in testable business
code. Detachment means no echoes while absent: a pipeline-stage combinator,
not a gated-merge operand.


