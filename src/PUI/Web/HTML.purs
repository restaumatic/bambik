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
-- | occurrence sources (`clicked`, `onClickedXY`), visibility (`provided`)
-- | and the gated displays (`shown`, `shownWhen`, `inCase`, `shownEach`),
-- | and the structure builders (`dynamic`, `each`, `el`) — lives in the
-- | parent module, `PUI.Web`, shared with SVG and the design systems.
module PUI.Web.HTML
  ( a
  , article
  , aside
  , body
  , button
  , blockquote
  , code
  , div
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
  , label
  , li
  , ol
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
import Data.Array ((!!), findIndex)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Number (fromString) as Number
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (forCase)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (case_, inj, match, on)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import PUI (Ocular, PUI)
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import PUI.Web (OptCaption(..), selectedAt, selectedOptionalAt, selectedUnpickedAt, Node, Web, addEventListener, adoptHostDiagnostics, appendChild, attribute, createElementNS, documentBody, el, element, getValue, htmlNS, isFocused, runDomInNode, setAttribute, setValue, staticText, textOf, (:=), (:=>))

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
input :: forall @l r rest. IsSymbol l => Cons l String rest r => String -> PUI Web { | r } { | r }
input type_ = focusField @l $ "name" := reflectSymbol (Proxy @l) $ "type" := type_ $ wrap do
  -- focus guard: skip the write while the user is in the field, but still
  -- echo — an editor owes every feed its answer (record-echo totality), and
  -- its field is one the gates wait for
  element "input" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new $ Nothing
  pure
    { toUser: \newa -> do
      focused <- isFocused node
      unless focused $ setValue node newa
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      void $ addEventListener "input" node $ const do
        value <- getValue node
        prop value
    }

-- | The multi-line `input` — same citizenship (label-indexed, `name`
-- | stamped) and same guarantee: typing is never interrupted by values
-- | arriving from elsewhere.
textArea :: forall @l r rest. IsSymbol l => Cons l String rest r => PUI Web { | r } { | r }
textArea = focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "textArea" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \newa -> do
      focused <- isFocused node
      unless focused $ setValue node newa
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      void $ addEventListener "input" node $ const do
        value <- getValue node
        prop value
    }

-- | One choice out of a fixed list — the native `<select>` of `<option>`s,
-- | with no chrome and no label of its own.
-- | The field holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `selectUnpicked @l @c` for a
-- | choice owed but not yet made, `selectOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options belong to the control, not to the model.
select :: forall @l a rest r. IsSymbol l => Cons l a rest r => Eq a => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
select options = selectWith @l false (selectedAt @l) options

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c a b s rest r. IsSymbol l => IsSymbol c => Cons c a b s => Cons l [ | s ] rest r => Eq a => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectUnpicked options = selectWith @l false (selectedUnpickedAt @l @c) options

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | an empty first option clears it, storing `n` again.
selectOptional :: forall @l @c @n a b t s rest r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b s => Cons n {} t s => Cons l [ | s ] rest r => Eq a => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectOptional options = selectWith @l true (selectedOptionalAt @l @c @n) options

selectWith :: forall @l a i o. IsSymbol l => Eq a => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> Array { value :: a, label :: String } -> PUI Web i o
selectWith clearable lift options = lift $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "select" (void $ unwrap optionLeaves)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ void $ addEventListener "change" node $ const do
    picked <- getValue node
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop (_.value <$> (Int.fromString picked >>= (options !! _)))
  pure
    { toUser: \ma -> do
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setValue node (show idx)
          Nothing -> setValue node ""
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  optionLeaves :: PUI Web {} {}
  optionLeaves = wrap do
    when clearable do
      element "option" (pure unit)
      noneNode <- gets _.sibling
      liftEffect $ setAttribute noneNode "value" ""
    forWithIndex_ options \idx o -> do
      element "option" (void $ unwrap (staticText o.label))
      optionNode <- gets _.sibling
      liftEffect $ setAttribute optionNode "value" (show idx)
    pure { toUser: mempty, fromUser: \prop -> prop {} }

-- | The native range slider (`<input type="range">`), with no chrome and no
-- | readout of its own, reporting while the user drags.
-- |
-- | The range is part of the quantity, not part of the screen:
-- | `{ current, min, max, step }` travels together as one business datum, so
-- | limits come from the data and can change while the app runs — a slider
-- | is never silently out of range, and a range nobody supplied is a
-- | compile error rather than a wrong screen. `step` is `.discrete n`
-- | or `.continuous {}`, named like every other two-state field.
rangeInput :: forall @l r rest. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } rest r => PUI Web { | r } { | r }
rangeInput = focusField @l $ "name" := reflectSymbol (Proxy @l) $ "type" := "range" $ wrap do
  element "input" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  qRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \q -> do
        Ref.write (Just q) qRef
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        setAttribute node "step" (match { discrete: show, continuous: \_ -> "any" } q.step)
        setValue node (show q.current)
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "input" node $ const do
          value <- getValue node
          mq <- Ref.read qRef
          for_ mq \q -> for_ (Number.fromString value) \v -> prop (q { current = v })
    }

-- | The native `<progress>` gauge, `value` running 0 to 1. As much a gauge
-- | as a progress indicator — a quota, a share, a fraction elapsed —
-- | `progress @"Progress" progressFraction`: the value is a **read
-- | function** like every display's, since a fraction is derived (a ratio
-- | of source fields), not state. The label is the accessible name only —
-- | a bar showing 42% must announce *what* is 42% — so it is copy, never a
-- | field reference.
progress
  :: forall @l reads
   . IsSymbol l
  => ({ | reads } -> Number) -> PUI Web { | reads } {}
progress f = wrap do
  element "progress" (pure unit)
  attribute "max" "1"
  attribute "aria-label" (reflectSymbol (Proxy @l))
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute node "value" (show (f r))
        -- display echo (like `text`): the feed's answer, inert to gates
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | What just happened, told in place — the native `<output>`, HTML's
-- | element for the result of a user action. It shows the latest event's
-- | line and keeps it on the page (plain HTML has nothing that dismisses
-- | itself).
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the output is built — `output @"booked" bookedLine` — and
-- | let the event carry the bare facts.
output
  :: forall @l a s
   . IsSymbol l
  => Cons l a () s
  => (a -> String)
  -> PUI Web [ | s ] {}
output copy = outputFace # forCase @l copy

outputFace :: PUI Web [ event :: String ] {}
outputFace = el "output" $ textOf eventText

-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = on (Proxy @"event") identity case_

-- TODO disable button after click?
-- | A bare `<button>`, captioned by its label verbatim (`label:` overrides
-- | with real copy the label cannot be). It reports, as case `l`, that the
-- | user asked for something, carrying whatever row it was being shown at
-- | the time, so the request arrives with its subject attached:
-- | `button @"Count" {}`. An event source, `× → +`.
-- |
-- | It is disabled until it has been shown something, and **disables itself
-- | on click** until the next value reaches it — so a double tap cannot
-- | send a request twice, and a button that stays dead is a screen whose
-- | model never came back.
button :: forall @l provided r s. IsSymbol l => Cons l { | r } () s => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } [ | s ]
button provided = wrap do
  w' <- unwrap (el "button" >>> "disabled" :=> (\x -> if isNothing x then Just "true" else Nothing) $ staticText config.label)
  -- a click before any value arrived has nothing valid to emit — withheld
  mARef <- liftEffect $ Ref.new Nothing
  node <- gets _.sibling
  pure
    { toUser: \occur -> do
        status <- w'.toUser {}
        Ref.write (Just occur) mARef
        pure status
    , fromUser: \prop -> void $ addEventListener "click" node $ const do
        mA <- Ref.read mARef
        for_ mA \fed -> do
          setAttribute node "disabled" "true" -- TODO re-think
          prop (inj (Proxy @l) fed)
    }
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String }

-- | A horizontal rule separating sections — fixed decoration, and the one
-- | element with nothing inside it, so it is written as a leaf rather than
-- | wrapped around content.
hr :: PUI Web {} {}
hr = wrap do
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
-- | makes: `body $ with initialOrder $ …` or `body $ … $ screen # mvu
-- | initialGame`. This is the plain-HTML floor's entry; every design-system
-- | module exports a `body` of the same signature that first dresses the
-- | page for its catalogue (MDC2's typography baseline, MDC3's typescale
-- | stylesheet, Fluent's theme, Shoelace's icon base path) and then mounts
-- | here, so an app imports its entry from its vocabulary like every other
-- | word and no vocabulary acts on the page at import time.
-- |
-- | The app has to be **complete**: everything on screen must have a value
-- | from the first frame, and `with`/`mvu` are where that starting state is
-- | supplied. Anything left unsupplied is reported here as a compile error
-- | naming the missing pieces — a screen can't reach a user half-filled.
-- |
-- | Mounting registers the wiring and then feeds `{}` **once** — the
-- | terminal record's one value — so "closed to `{}`" is literal: the app
-- | is fed exactly what its type says it needs, which is nothing. A
-- | `{}`-input display (`text (const "…")`) renders on that feed; a point
-- | (`announce`, `with`, `mvu`) has already answered at registration and,
-- | by Repetition at `{}`, has nothing more to say (`Data.Profunctor.Seeding`).
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptHostDiagnostics
  node <- documentBody
  runDomInNode node do
    { toUser, fromUser } <- unwrap ui
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
    { toUser, fromUser } <- unwrap ui
    liftEffect $ fromUser callback
    void $ liftEffect $ toUser initial
