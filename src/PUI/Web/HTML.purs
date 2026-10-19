-- | The HTML vocabulary — one name per HTML element, plus HTML's native
-- | controls.
-- |
-- | **Elements** (`div`, `p`, `ul`, `li`, `a`, `table`, `h1`–`h6`, ...) wrap
-- | content. **Native controls** are the places a plain-HTML screen takes a
-- | value: `input` and `textArea` edit a string, `select`/`rangeInput` a
-- | choice and a bounded quantity, `progress` shows a fraction, `output`
-- | narrates an event, `button` reports a click. `hr` is fixed decoration,
-- | and `body` mounts the finished screen.
-- |
-- | Everything that names no element — the decorators (`attr`/`:=`, `cl`,
-- | `attrWith`, `clWhen`), the text leaves (`text`, `staticText`), the
-- | occurrence sources (`clicked`, `onClickedXY`),
-- | the panes (`provided`, `inCase`), the sources (`geolocated`, `clock`, …),
-- | and the structure builders (`dynamic`, `each`, `el`) — lives in the
-- | parent module, `PUI.Web`, shared with SVG and the design systems.
module PUI.Web.HTML
  ( acknowledged
  , confirmed
  , a
  , article
  , aside
  , body
  , button
  , blockquote
  , code
  , dd
  , div
  , dl
  , dt
  , em
  , footer
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , header
  , hr
  , i
  , img
  , input
  , inPlaceInput
  , filePicked
  , label
  , li
  , ol
  , option
  , indeterminateLinearProgress
  , output
  , p
  , progress
  , rangeInput
  , runComponentInNode
  , section
  , select
  , selectUnpicked
  , selectOptional
  , span
  , strong
  , table
  , tbody
  , td
  , textArea
  , th
  , thead
  , tr
  , ul
  )
  where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Number (fromString) as Number
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (inj, match)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Data.Profunctor.Row.VariantToVariant (focusCase)
import PUI (fromHandles, handles, Ocular, PUI, recordToRecordWiring, variantToRecordWiring, variantToVariantWiring, optionWiring, replayed, source, departures, statusOver, confirmedOver, acknowledgedOver)
import PUI.Web (chrome, fieldLeaf, Node, Web, adoptHostDiagnostics, onFilePicked, appendChild, attribute, createElementNS, documentBody, el, element, getValue, htmlNS, isFocused, listenNode, runDomInNode, setAttribute, setValue, staticText, statusText, separating, invokeMethod, showModalOnce, (:=), (:=>))

-- UIs

-- | A single-line input of the given `type` ("text", "number",
-- | "email", ...), label-indexed at the `String` field it edits (L3):
-- | shows the string it is given, reports every keystroke, and stamps its
-- | label as the host `name`. The floor has no caption chrome of its own,
-- | so a caption stays a sibling `label`+`staticText` merge — and the
-- | `focusField @l` lift is fused in here as in every vocabulary's editors, so
-- | application code never lifts a scalar leaf itself.
-- |
-- | Typing is never interrupted — while the field has focus, values
-- | arriving from elsewhere are not written into it, so an update can't
-- | swallow a half-typed word; the field picks the model up again the
-- | moment it loses focus.
input :: forall @l r b. IsSymbol l => Cons l String b r => String -> PUI Web { | r } { | r }
input type_ = fieldLeaf @l $ "type" := type_ $ fromHandles do
  element "input" (pure unit)
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "input"
    , read: \_ -> getValue node
    }

-- | The **in-place text input** of a grid or a list — a sheet's cell
-- | while it is edited: `input @k` over field `k`, its edits kept until
-- | the user leaves it (a click elsewhere, Tab, Enter), when the row with
-- | field `k` as typed leaves as case `l`. Shown by a pane while its
-- | element is being edited, it takes the focus when shown
-- | (`inPlaceInput @"Formula entered" @"Formula" # providedAt @"editing"
-- | @"mode"`, cells). `× → +`: a leaving is the commit.
inPlaceInput
  :: forall @l @k r b1 b2 v
   . IsSymbol l
  => IsSymbol k
  => Cons k String b1 r
  => Cons l { | r } b2 v
  => PUI Web { | r } [ | v ]
inPlaceInput = span $ input @k "text" >>> replayed @l identity departures

-- | A **file picker** — a capture, not an editor: the user supplies the
-- | file and the platform cannot be shown one, so it owns its field like a
-- | source (`PUI.source`): `none` until a file is chosen, then `picked`
-- | with its name, size and MIME type. Label-indexed, `name` stamped.
filePicked
  :: forall @l r r1
   . IsSymbol l
  => Cons l [ picked :: { name :: String, size :: Number, mime :: String }, none :: {} ] () r1
  => PUI Web { | r } { | r1 }
filePicked = "name" := reflectSymbol (Proxy @l) $ "type" := "file" $ fromHandles do
  element "input" (pure unit)
  node <- gets _.sibling
  handles (source @l (pure (inj (Proxy @"none") {})) \tell -> onFilePicked node (tell <<< inj (Proxy @"picked")))

-- | The multi-line `input` — same citizenship (label-indexed, `name`
-- | stamped) and same guarantee: typing is never interrupted by values
-- | arriving from elsewhere.
textArea :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textArea = fieldLeaf @l $ fromHandles do
  element "textArea" (pure unit)
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "input"
    , read: \_ -> getValue node
    }

-- | One choice out of a fixed list — the native `<select>` of `<option>`s,
-- | with no chrome and no label of its own.
-- | The field holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `selectUnpicked @l @c` for a
-- | choice owed but not yet made, `selectOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options belong to the control, not to the model: they are `option`
-- | leaves merged `VariantToVariant.do`, the block closing the field's row
-- | in the order written.
select :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
select options = fieldLeaf @l $ selectGroupOf false options

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectUnpicked options = fieldLeaf @l $ selectGroupOf false (focusCase @c options)

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | an empty first option clears it, storing `n` again.
selectOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectOptional options = fieldLeaf @l $ selectGroupOf true (blankEmits @n (focusCase @c options))

-- | One option of a `select`: a native `<option>` showing its case.
option :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
option = fromHandles do
  let tag = reflectSymbol (Proxy @c)
  selectNode <- gets _.parent
  element "option" (chrome (staticText tag))
  optionNode <- gets _.sibling
  liftEffect $ setAttribute optionNode "value" tag
  liftEffect $ optionWiring @c
    { show: setValue selectNode tag
    , reports: listenNode selectNode "change"
    , picked: (_ == tag) <$> getValue selectNode
    }

-- The native `<select>` around the options, cleared before every feed so
-- a case no option owns shows nothing picked; `blank` leads with an empty
-- option, the one a cleared optional select shows.
selectGroupOf :: forall a. Boolean -> PUI Web a a -> PUI Web a a
selectGroupOf blank options = el "select" $ fromHandles do
  selectNode <- gets _.parent
  when blank do
    element "option" (pure unit)
    noneNode <- gets _.sibling
    liftEffect $ setAttribute noneNode "value" ""
  inner <- handles options
  pure
    { toUser: \fed -> setValue selectNode "" *> inner.toUser fed
    , fromUser: inner.fromUser
    }

-- The clearing of an optional select: picking the empty option emits `n`.
blankEmits :: forall @n b v. IsSymbol n => Cons n {} b v => PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
blankEmits options = fromHandles do
  selectNode <- gets _.parent
  inner <- handles options
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> do
        inner.fromUser prop
        listenNode selectNode "change" do
          value <- getValue selectNode
          when (value == "") $ prop (inj (Proxy @n) {})
    }

-- | The native range slider (`<input type="range">`), with no chrome and no
-- | readout of its own, reporting while the user drags.
-- |
-- | The range is part of the quantity, not part of the screen:
-- | `{ current, min, max, step }` travels together as one business datum, so
-- | limits come from the data and can change while the app runs — a slider
-- | is never silently out of range, and a range nobody supplied is a
-- | compile error rather than a wrong screen. `step` is `.discrete n`
-- | or `.continuous {}`, named like every other two-state field.
rangeInput :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
rangeInput = fieldLeaf @l $ "type" := "range" $ fromHandles do
  element "input" (pure unit)
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: \q -> do
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        setAttribute node "step" (match { discrete: show, continuous: \_ -> "any" } q.step)
        setValue node (show q.current)
    , held: pure false
    , reports: listenNode node "input"
    , read: \q -> getValue node <#> \value -> q { current = fromMaybe q.current (Number.fromString value) }
    }

-- | The native `<progress>` gauge, `value` running 0 to 1. As much a gauge
-- | as a progress indicator — a quota, a share, a fraction elapsed —
-- | `progress progressFraction`: the value is a **read
-- | function** like every display's, since a fraction is derived (a ratio
-- | of source fields), not state.
progress
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progress f = fromHandles do
  element "progress" (pure unit)
  attribute "max" "1"
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring { show: \r -> setAttribute node "value" (show (f r)), held: pure false, reports: \_ -> pure unit, read: pure }

-- | What just happened, told in place — the native `<output>`, HTML's
-- | element for the result of a user action. It shows the latest event's
-- | caption and keeps it on the page (plain HTML has nothing that dismisses
-- | itself).
-- |
-- | A status **tells its case**: its caption is the case label verbatim,
-- | so the outcome is named as what it tells (`output @"Card charged"
-- | kept`) and the payload is read by nothing but the fold. A `+→×` leaf
-- | like `fold @l f`, it takes its fold of the payload.
output
  :: forall @l a v r
   . IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI Web [ | v ] { | r }
output = statusOver @l outputFace

outputFace :: forall a r. String -> PUI Web a { | r }
outputFace caseLabel = el "output" $ statusText caseLabel

-- TODO disable button after click?
-- | The **witness rung** — see `PUI.Web.MDC2.confirmed`: `focusCase @l`
-- | over the modal, the `+→+` stage of case `l`, its confirm button
-- | captioned by the case and re-emitting it with the row its content
-- | answered, a cancel consuming it, every other case passing. Over the native `<dialog>`, opened with `showModal`.
confirmed
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
confirmed = confirmedOver @l modal

-- | The **dismissed rung** — see `PUI.Web.MDC2.acknowledged`: `onCase @l`
-- | over the modal, the `+→+` stage consuming case `l`, telling it as its
-- | title and showing its payload until the user closes it, dismissed
-- | by its "OK"; the row less `l` leaves.
acknowledged
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | b ]
acknowledged = acknowledgedOver @l modal

-- the modal both dialog leaves are built over: `variantToVariantWiring`
-- over the native `<dialog>`, opened modal, an optional heading, the
-- content stage, an optional cancel button and the deciding button, named
-- by the heading or else by the decision; Escape closes the element by
-- itself, releasing nothing
modal
  :: forall r
   . { title :: Maybe String, decide :: String, cancel :: Maybe String }
  -> PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
modal { title, decide, cancel } content = fromHandles do
  decideRef <- liftEffect $ Ref.new Nothing
  cancelRef <- liftEffect $ Ref.new Nothing
  shown <- handles $ el "dialog" >>> "aria-label" := fromMaybe decide title $ fromHandles do
    for_ title \t -> handles (h2 $ staticText t)
    w <- handles (div $ content)
    for_ cancel \c -> do
      chrome (el "button" >>> "type" := "button" $ staticText c)
      node <- gets _.sibling
      liftEffect $ Ref.write (Just node) cancelRef
    chrome (el "button" >>> "type" := "button" $ staticText decide)
    node <- gets _.sibling
    liftEffect $ Ref.write (Just node) decideRef
    pure w
  dialogNode <- gets _.sibling
  decideNode <- liftEffect $ Ref.read decideRef
  cancelNode <- liftEffect $ Ref.read cancelRef
  liftEffect $ variantToVariantWiring
    { open: showModalOnce dialogNode
    , close: invokeMethod "close" dialogNode
    , content: shown
    , reports: \decided -> for_ decideNode \node -> listenNode node "click" decided
    , cancels: \cancelled -> for_ cancelNode \node -> listenNode node "click" cancelled
    }

-- | A bare `<button>`, captioned by its label verbatim (`label:` overrides
-- | with real copy the label cannot be). It reports, as case `l`, that the
-- | user asked for something, carrying whatever row it was being shown at
-- | the time, so the request arrives with its subject attached:
-- | `button @"Count"`. An event source, `× → +`.
-- |
-- | It is disabled until it has been shown something: a click before any
-- | value arrived has nothing to report. Like every vocabulary's button it
-- | stays live after a click — a request a screen must not send twice is
-- | its action's concern, not the button's.
button :: forall @l r b v. IsSymbol l => Cons l { | r } b v => PUI Web { | r } [ | v ]
button = fromHandles do
  w' <- handles (el "button" >>> "disabled" :=> (\x -> if isNothing x then Just "true" else Nothing) $ staticText config.label)
  -- a click before any value arrived has nothing valid to emit — withheld
  mARef <- liftEffect $ Ref.new Nothing
  node <- gets _.sibling
  pure
    { toUser: \occur -> do
        status <- w'.toUser {}
        Ref.write (Just occur) mARef
        pure status
    , fromUser: \prop -> listenNode node "click" do
        mA <- Ref.read mARef
        for_ mA \fed -> prop (inj (Proxy @l) fed)
    }
  where
  config = { label: reflectSymbol (Proxy @l) } :: { label :: String }

-- | A horizontal rule separating sections — fixed decoration, and the one
-- | element with nothing inside it, so it is written as a leaf rather than
-- | wrapped around content.
hrExact :: PUI Web {} {}
hrExact = fromHandles do
  parent <- gets _.parent
  newNode <- liftEffect $ do
    node <- createElementNS htmlNS "hr"
    appendChild node parent
    pure node
  modify_ _ { sibling = newNode }
  pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
    }

hr :: Ocular (PUI Web)
hr = separating hrExact

-- UIOculars

-- | The all-purpose box: grouping and layout where no other element carries
-- | meaning.
div :: Ocular (PUI Web)
div = el "div"

-- | The all-purpose inline wrapper: a run of text singled out for styling,
-- | inside a line rather than around it.
span :: Ocular (PUI Web)
span = el "span"

-- | Content beside the main content — a sidebar, a pull quote, a nav panel.
aside :: Ocular (PUI Web)
aside = el "aside"

-- | A self-contained piece of content — a post, a card's subject, an entry
-- | that would still make sense on its own.
article :: Ocular (PUI Web)
article = el "article"

-- | The introductory band of a page or a section: title, subtitle, the
-- | controls that belong to what follows.
header :: Ocular (PUI Web)
header = el "header"

-- | The closing band of a page or a section: fine print, attribution,
-- | secondary links.
footer :: Ocular (PUI Web)
footer = el "footer"

-- | A thematic section of a page, the unit a heading introduces.
section :: Ocular (PUI Web)
section = el "section"

-- | The caption that belongs to a control. Wrapping the control makes the
-- | words part of its hit area, so clicking the text works the control.
label :: Ocular (PUI Web)
label = el "label"

-- | A picture. The source and alternative text are attributes:
-- | `"src" := url $ "alt" := "Cover" $ img …`.
img :: Ocular (PUI Web)
img = el "img"

-- | Strong importance — the words a reader must not miss, rendered bold.
strong :: Ocular (PUI Web)
strong = el "strong"

-- | Emphasis — a stress in the reading, rendered italic.
em :: Ocular (PUI Web)
em = el "em"

-- | Text that is code: a literal value, a command, an identifier, in a
-- | monospaced face.
code :: Ocular (PUI Web)
code = el "code"

-- | A quotation set apart from the surrounding text.
blockquote :: Ocular (PUI Web)
blockquote = el "blockquote"

-- | A paragraph — the default block of running text.
p :: Ocular (PUI Web)
p = el "p"

-- | An alternate voice or mood set off from the surrounding text — and, by
-- | long convention, the element icon fonts hang their glyphs on.
i :: Ocular (PUI Web)
i = el "i"

-- | A link. The destination is an attribute: `"href" := url $ a …`.
a :: Ocular (PUI Web)
a = el "a"

-- | An unordered list of `li` items — a collection whose order carries no
-- | meaning.
ul :: Ocular (PUI Web)
ul = el "ul"

-- | An ordered list of `li` items — a collection where the numbering is
-- | part of the content (steps, a ranking).
ol :: Ocular (PUI Web)
ol = el "ol"

-- | One item of a `ul` or `ol`.
li :: Ocular (PUI Web)
li = el "li"

-- | A description list: terms, each followed by its description, as bare
-- | `dt`/`dd` siblings — the one HTML list whose item is two elements
-- | and nothing wrapping them (a keyed `foreach` or `acted` over such an
-- | item moves and detaches both nodes with the key; reorder-html).
dl :: Ocular (PUI Web)
dl = el "dl"

-- | A term of a `dl`.
dt :: Ocular (PUI Web)
dt = el "dt"

-- | The description of the `dt` before it.
dd :: Ocular (PUI Web)
dd = el "dd"

-- table elements get real oculars (not `staticHTML`): the raw-HTML parser
-- drops `tr`/`td`/`thead` fragments outside a table context

-- | A table: data in rows and columns, where the position of a value in the
-- | grid is what gives it meaning. Not for page layout.
table :: Ocular (PUI Web)
table = el "table"

-- | The table's header band, holding the row of `th` column headings.
thead :: Ocular (PUI Web)
thead = el "thead"

-- | The table's body — the rows of data.
tbody :: Ocular (PUI Web)
tbody = el "tbody"

-- | One row of a table.
tr :: Ocular (PUI Web)
tr = el "tr"

-- | A heading cell: the name of a column (or of a row), not a value.
th :: Ocular (PUI Web)
th = el "th"

-- | A data cell — one value in a table row.
td :: Ocular (PUI Web)
td = el "td"

-- | The page's top-level heading: what the screen *is*. One per screen.
h1 :: Ocular (PUI Web)
h1 = el "h1"

-- | A second-level heading — a major section of the screen.
h2 :: Ocular (PUI Web)
h2 = el "h2"

-- | A third-level heading — a subsection of an `h2`.
h3 :: Ocular (PUI Web)
h3 = el "h3"

-- | A fourth-level heading. Headings step down one level at a time; the
-- | rank is the outline, not the size (size is the design system's).
h4 :: Ocular (PUI Web)
h4 = el "h4"

-- | A fifth-level heading.
h5 :: Ocular (PUI Web)
h5 = el "h5"

-- | A sixth-level heading, the deepest rank.
h6 :: Ocular (PUI Web)
h6 = el "h6"

-- | Mount the app in the page's `<body>` — the one call an application
-- | makes: `body $ with initialOrder $ …` or `body $ … $ screen # looped
-- | # with initialGame`. This is the plain-HTML floor's entry; every design-system
-- | module exports a `body` of the same signature that first dresses the
-- | page for its catalogue (MDC2's typography baseline, MDC3's typescale
-- | stylesheet, Fluent's theme, Shoelace's icon base path) and then mounts
-- | here, so an app imports its entry from its vocabulary like every other
-- | word and no vocabulary acts on the page at import time.
-- |
-- | The app has to be **complete**: everything on screen must have a value
-- | from the first frame, and `with` is where that starting state is
-- | supplied. Anything left unsupplied is reported here as a compile error
-- | naming the missing pieces — a screen can't reach a user half-filled.
-- |
-- | Mounting registers the wiring and then feeds `{}` **once** — the
-- | terminal record's one value — so "closed to `{}`" is literal: the app
-- | is fed exactly what its type says it needs, which is nothing. A
-- | `{}`-input display (`text (const "…")`) renders on that feed; a point
-- | (`announce`, `with`) has already answered at registration and,
-- | by Repetition at `{}`, has nothing more to say (`PUI.announce`).
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptHostDiagnostics
  node <- documentBody
  runDomInNode node do
    { toUser, fromUser } <- handles ui
    liftEffect do
      fromUser \_ -> pure unit
      toUser {}

-- | Mount a UI component into an existing element rather than taking over the
-- | page — for embedding into a page bambik does not own. The starting value
-- | is given here, and the callback receives what the UI component reports.
runComponentInNode :: forall a b. Node -> a -> (b -> Effect Unit) -> PUI Web a b -> Effect Unit
runComponentInNode node initial callback ui = do
  adoptHostDiagnostics
  runDomInNode node do
    { toUser, fromUser } <- handles ui
    liftEffect $ fromUser callback
    void $ liftEffect $ toUser initial

-- | The **indeterminate progress bar**: a status fed a run's `started` and
-- | `ended`, shown between them — what `PUI.action`'s slot dispatches
-- | (`indeterminateLinearProgress # action submit`). No label: it names no
-- | field and no case.
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = fromHandles do
  element "progress" (pure unit)
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ variantToRecordWiring $ match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
