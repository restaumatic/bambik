-- | The HTML vocabulary — one name per HTML element, plus the handful of
-- | leaves that carry data.
-- |
-- | **Elements** (`div`, `p`, `ul`, `li`, `a`, `table`, `h1`–`h6`, ...) wrap
-- | content and take decorators (`attr`/`:=`, `cl`). **Leaves** are the places
-- | a screen shows or takes a value: `text` shows a string, `input` and
-- | `textArea` edit one, `button`/`clicked`/`onClickedXY` report what the user
-- | did, and `staticText`/`staticHTML`/`hr` are fixed decoration. `body` mounts
-- | the finished screen.
-- |
-- | There are two ways to draw a screen from data, and the choice is visible to
-- | the user. When the **shape is fixed and only the values move** — a
-- | spreadsheet grid, an SVG canvas, a table of orders — build the shape once
-- | and let data flow through it (`foreach` from `PUI` for the repeated part,
-- | `text` for contents, `attrWith`/`clWhen` for anything computed): elements are updated
-- | in place, so nothing loses focus, scroll position or a half-finished
-- | gesture when a value changes. When the **shape itself depends on the data**
-- | — a rendered markdown document, where one block is a heading and the next a
-- | list — `dynamic` and `each` build it from a function and
-- | redraw when it changes.
-- |
-- | The plumbing they are built over lives in the parent module, `PUI.Web`;
-- | the design-system vocabularies are its other children.
module PUI.Web.HTML
  ( (:=)
  , (:=>)
  , a
  , article
  , aside
  , attr
  , attrDyn
  , attrWith
  , body
  , button
  , cl
  , clWhen
  , clicked
  , blockquote
  , code
  , div
  , dynamic
  , each
  , el
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
  , init
  , input
  , label
  , li
  , ol
  , onClickedXY
  , output
  , p
  , progress
  , radioButton
  , rangeInput
  , runComponentInNode
  , section
  , select
  , span
  , shown
  , shownWhen
  , inCase
  , shownEach
  , staticText
  , strong
  , table
  , tbody
  , td
  , text
  , textArea
  , th
  , thead
  , tr
  , ul
  , provided
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
import Data.Profunctor.Row.RecordToRecord (field, recordToRecord, projected)
import Data.Profunctor.Row (class OwnedRecordOutputs, class SharedRecordInputs, widenRecordInput)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (case_, match, on, prj)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons, class Union)
import Prim.RowList (Nil) as RL
import Record (get) as Record
import Type.Proxy (Proxy(..))
import PUI (Ocular, PUI, diagnosticsOn, foreach, muted)
import Unsafe.Coerce (unsafeCoerce)
import PUI.Web (Node, Web, adoptHostDiagnostics, addClass, addEventListener, appendChild, attachable, attribute, clazz, createCommentNode, createElementNS, createTextNode, documentBody, element, getValue, htmlNS, isFocused, onClickXY, removeAllChildren, removeAttribute, removeClass, runDomInNode, setAttribute, setChecked, setTextNodeValue, setValue)

-- UIs

-- | The **ambient rung** — content that is
-- | always there: registered at build (its chrome exists before any
-- | feed), fed the row on every feed, the fed row released always. The
-- | content reads its own *closed* narrow row by subsumption
-- | (`Union read extra row`), so a chrome merge states exactly
-- | the fields it shows — verbatim: a formatted read is a derived field a
-- | `settled` normalization maintains (the presentation-model rule).
-- | The sibling of `shownWhen`/`shownEach` whose policy is
-- | no policy; the rung trails its content like every data concern:
-- | `(headline6 $ …) # shown`.
shown
  :: forall read extra row
   . Union read extra row
  => PUI Web { | read } {} -> PUI Web { | row } { | row }
shown content = wrap do
  content' <- unwrap content
  -- complete the content's wiring: its only possible emission is the
  -- informationless {}, discarded lawfully (the content type says so)
  liftEffect $ content'.fromUser \_ -> pure unit
  propRef <- liftEffect $ Ref.new Nothing
  -- the content registers at build (its chrome exists before any feed, like
  -- every component's); feeding renders the narrow row it reads, then the
  -- fed row is released — the ambient rung's gate opens instantly
  pure
    { toUser: \row -> do
        content'.toUser (unsafeCoerce row)
        mProp <- Ref.read propRef
        for_ mProp \prop -> prop row
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | The **case-pane rung** — `provided` merged with the wire: content
-- | attached and fed on case `l` of the classified variant, detached on
-- | any other case, the fed row released always. A hidden pane must never
-- | block the pipe, so this rung's fulfillment is best-effort by
-- | construction. Trails its content: `(…) # shownWhen @l classifier`.
shownWhen
  :: forall @l read extra row a b s i12 i1x i2x rowL
   . IsSymbol l => Cons l a b s
  => Union read extra row
  => SharedRecordInputs row row row i12 i1x i2x
  => OwnedRecordOutputs () row row RL.Nil rowL
  => ({ | read } -> [ | s ]) -> PUI Web a {} -> PUI Web { | row } { | row }
shownWhen f content = recordToRecord (provided @l (\(r :: { | row }) -> f (unsafeCoerce r)) content) identity

-- | The **editor pane** — `shownWhen`'s
-- | editor sibling. A whole-row citizen (an editor, or a pipeline of them)
-- | that *exists* only while the classifier yields case `l`: attached and
-- | fed the whole row on that case, detached on any other, the fed row
-- | released always. Where `shownWhen` is the pane owned-merged with the
-- | wire (its content emits `{}`), this pane's content emits the **row**,
-- | which the owned merge's disjointness rejects — so the rung is a
-- | carrier primitive: the pane's channel and the wire's, side by side
-- | over one input and one output.
-- |
-- | It dissolves the identity fold: a field that exists only in one mode
-- | is *not* a payload to fold back into the row by hand
-- | (`# provided @l paneOf # updated setField` with `setField`
-- | the identity) — it is a whole-row editor whose existence is gated, and
-- | its `field @l` lift already re-attaches the rest of the row. The
-- | classifier reads a closed narrow row (the row-stating exception:
-- | `fulfillment :: { selected :: [ … ] } -> [ … ]`), exactly as
-- | `shownWhen`'s does. Two releases per feed while attached — the wire's
-- | and the editor's own echo — idempotent under the loop, which swallows
-- | the re-fed one. What the edit does to the rest of the row is a `settled`
-- | normalization on the same stage when it is a state invariant
-- | (meeting-booker's `seatsInRoom`, circle-drawer's `resizeSelected`).
inCase
  :: forall @l read extra row a b s
   . IsSymbol l => Cons l a b s
  => Union read extra row
  => ({ | read } -> [ | s ]) -> PUI Web { | row } { | row } -> PUI Web { | row } { | row }
inCase f w = wrap do
  { result: pane, ensureAttached, ensureDetached } <- attachable $ unwrap w
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \row -> do
        case prj (Proxy @l) (f (unsafeCoerce row :: { | read })) of
          Nothing -> ensureDetached
          -- attach before feeding, as `provided` does
          Just _ -> ensureAttached *> pane.toUser row
        mProp <- Ref.read propRef
        for_ mProp \prop -> prop row
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        pane.fromUser prop
    }

-- | The **collection rung** — render the keyed,
-- | retained list from the projection, release the fed row per feed.
-- | Derived: the collection, muted, merged with the wire. Trails its
-- | item: `(li $ …) # shownEach @l proj`.
shownEach
  :: forall @l read extra row k r a o i12 i1x i2x rowL
   . IsSymbol l => Cons l k r a => Ord k
  => Union read extra row
  => SharedRecordInputs row row row i12 i1x i2x
  => OwnedRecordOutputs () row row RL.Nil rowL
  => ({ | read } -> Array { | a }) -> PUI Web { | a } o -> PUI Web { | row } { | row }
shownEach proj item = recordToRecord (muted (foreach @l (\(r :: { | row }) -> proj (unsafeCoerce r)) item)) identity

-- | Show a string that changes — a readout, a total, a name in a list row.
-- | (Wording that doesn't change is `staticText`.)
-- |
-- | A line glued together from several values is several `text` leaves side
-- | by side, with `staticText` for the literal words between them: each
-- | value is its own text node and updates on its own, and the sentence is
-- | assembled where it is read rather than in the business code behind it.
text :: forall @l r. IsSymbol l => Cons l String () r => PUI Web { | r } {}
text = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    -- a text node cannot carry the label-stamp attribute the element-hosted
    -- leaves get, so under host diagnostics a comment marker names it instead
    diag <- diagnosticsOn
    when diag do
      marker <- createCommentNode ("text @" <> show (reflectSymbol (Proxy @l)))
      appendChild marker parentNode
    node <- createTextNode ""
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  node <- gets (_.sibling)
  propRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  pure
    { toUser: \s -> do
        setTextNodeValue node (Record.get (Proxy @l) s)
        prop <- Ref.read propRef
        prop {}
    , fromUser: \prop -> Ref.write prop propRef
    }

-- | A bare single-line input of the given `type` ("text", "number",
-- | "email", ...): shows the string it is given and reports every
-- | keystroke.
-- |
-- | Typing is never interrupted — while the field has focus, values
-- | arriving from elsewhere are not written into it, so an update can't
-- | swallow a half-typed word; the field picks the model up again the
-- | moment it loses focus.
input :: String -> PUI Web String String
input type_ = "type" := type_ $ wrap do
  -- focus guard: skip the write while the user is in the field, but still
  -- echo, so downstream stages keep flowing
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

-- | The multi-line `input` — same guarantee: typing is never interrupted by
-- | values arriving from elsewhere.
textArea :: PUI Web String String
textArea = wrap do
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

-- | A bare radio button, with no chrome and no label of its own: filled
-- | while it is the current choice, and reporting that choice when picked.
-- | One per option; the design-system vocabularies package the whole group
-- | as a single labelled control.
-- |
-- | `picked` is the choice this button stands for until the model supplies
-- | one — stated by the caller, never conjured from the type.
radioButton :: forall a. { picked :: a } -> PUI Web (Maybe a) a
radioButton { picked } = "type" := "radio" $ wrap do
  aRef <- liftEffect $ Ref.new picked
  mPropRef <- liftEffect $ Ref.new Nothing
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: \ma -> do
        case ma of
          Nothing -> setChecked node false
          Just newa -> do
            setChecked node true
            Ref.write newa aRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \newa -> prop newa
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "change" node $ const do
          held <- Ref.read aRef
          prop held
    }

-- | One choice out of a fixed list — the native `<select>` of `<option>`s,
-- | with no chrome and no label of its own. Until the user picks there is
-- | nothing to show, so the field arrives as "maybe a choice" and leaves as
-- | the choice itself — say which with `# optional @"chosen" @"unchosen"` or
-- | `# required`. The
-- | options belong to the control, not to the model.
select :: forall @l a ri ro. IsSymbol l => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
select options = field @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "select" (void $ unwrap optionLeaves)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ void $ addEventListener "change" node $ const do
    picked <- getValue node
    for_ (Int.fromString picked >>= (options !! _)) \o -> do
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop o.value
  pure
    { toUser: \ma -> do
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setValue node (show idx)
          Nothing -> setValue node ""
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  optionLeaves :: PUI Web {} {}
  optionLeaves = wrap do
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
rangeInput = field @l $ "name" := reflectSymbol (Proxy @l) $ "type" := "range" $ wrap do
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
-- | fed a model fraction maintained by `present<App>` (`progress @"fraction"`),
-- | with the business function
-- | deciding what the fraction means.
progress :: forall @l r. IsSymbol l => Cons l Number () r => PUI Web { | r } {}
progress = wrap do
  element "progress" (pure unit)
  attribute "max" "1"
  attribute "aria-label" (reflectSymbol (Proxy @l))
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute node "value" (show (Record.get (Proxy @l) r))
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | What just happened, told in place — the native `<output>`, HTML's
-- | element for the result of a user action. It shows the latest event's
-- | line and keeps it on the page (plain HTML has nothing that dismisses
-- | itself).
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the output is built — `output # forCase @"booked" bookedLine` — and
-- | let the event carry the bare facts.
output :: PUI Web [ event :: String ] {}
output = el "output" $ text @"line" # projected eventText

-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = on (Proxy @"event") identity case_

-- TODO disable button after click?
-- | A bare `<button>` around fixed content — a label, an icon, both. Its
-- | content is decoration; the button reports that the user asked for
-- | something, carrying whatever it was being shown at the time, so the
-- | request arrives with its subject attached.
-- |
-- | It is disabled until it has been shown something, and **disables itself
-- | on click** until the next value reaches it — so a double tap cannot
-- | send a request twice, and a button that stays dead is a screen whose
-- | model never came back.
button :: forall a. PUI Web {} {} -> PUI Web a a
button w = wrap do
  w' <- unwrap (el "button" >>> "disabled" :=> (\x -> if isNothing x then Just "true" else Nothing) $ w)
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
          prop fed
    }

-- | Fixed text: a caption, a unit, the literal words between two values on
-- | a line. It never changes and carries no data, which is what makes it
-- | the piece to reach for when a sentence is assembled in the UI from
-- | model values and wording — the wording is `staticText`, each value its
-- | own `text`.
staticText :: String -> PUI Web {} {}
staticText content = wrap do
  -- decoration contributes nothing: the `{}` it announces is ignored by
  -- the gates (a zero-field side is pre-known and inert), so this is the
  -- chrome's own completeness, not a merge requirement
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode content
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
    }


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

-- | Set a fixed attribute on the element being decorated, written infix as
-- | `:=`: `"placeholder" := "you@example.com" $ input "email" $ …`. For an
-- | attribute that follows the data (a colour, a coordinate, a width), use
-- | `attrWith`.
attr :: String -> String -> Ocular (PUI Web)
attr name value w = wrap do
  w' <- unwrap w
  attribute name value
  pure w'

-- | `attr` written infix: `"src" := url $ img $ …`.
infixr 10 attr as :=

-- | Add a fixed class to the element being decorated — how a design
-- | system's stylesheet is applied. For a class that comes and goes with the
-- | data, use `clWhen`.
cl :: String -> Ocular (PUI Web)
cl name w = wrap do
  w' <- unwrap w
  clazz name
  pure
    { toUser: w'.toUser
    , fromUser: w'.fromUser
    }

-- | Hand the element just built to a third-party component library and run
-- | its hooks around the traffic: the first function receives the element
-- | once and returns whatever handle the library gives back, the second runs
-- | before every value is shown, the third after every report. This is how a
-- | design-system vocabulary attaches an off-the-shelf component — a
-- | dialog's show and close, a ripple. Application code has no use for it.
init :: forall a. (Node -> Effect a) -> (a -> Effect Unit) -> (a -> Effect Unit) -> Ocular (PUI Web)
init nodeInitializer pre post w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  ctx <- liftEffect $ nodeInitializer node
  pure
    { toUser: \new -> do
        pre ctx
        w'.toUser new
    , fromUser: \prop -> do
      w'.fromUser \change -> do
        prop change
        post ctx
    }

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

-- | An attribute that marks the "nothing to show yet" state. The function
-- | is told whether the element has been given a value yet, and its answer
-- | sets the attribute or removes it — this is how a control stays disabled
-- | until its data arrives. Written infix as `:=>`. For an attribute
-- | computed from the data itself, use `attrWith`.
attrDyn :: String -> (Maybe {} -> Maybe String) -> Ocular (PUI Web)
attrDyn name valueFunction w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ updateAttribute node Nothing
  pure
    { toUser: \mch -> do
      updateAttribute node $ Just mch
      w'.toUser mch
    , fromUser: w'.fromUser
    }
    where
      updateAttribute node mnewa = case valueFunction (mnewa $> {}) of
        Just value -> setAttribute node name value
        Nothing -> removeAttribute node name

-- | `attrDyn` written infix, and read the same way as `:=` — the value is
-- | computed rather than given.
infixr 10 attrDyn as :=>

-- | Show the pane while the model is in state `l`, fed that state's own
-- | data — visibility is **case adoption**: the argument is a business
-- | function classifying the situation into a variant, and the pane is
-- | attached and fed the payload of case `l`, detached on every other
-- | case. A ticket counter whose `display` is either `waiting` or `serving`
-- | shows its number pane as `pane # provided @"serving" displayOf`;
-- | checkout's wizard shows each step's pane off one `checkoutStep`
-- | classifier whose cases carry what their panes review.
-- |
-- | This is the one visibility primitive. A `Maybe`-gated pane is the same
-- | thing with its two cases unnamed, so there is no `Maybe` form: a state a
-- | pane depends on is a variant with **named** cases — `estimated`/`unknown`
-- | for a distance, `reading`/`browsing` for an inbox, `chosen`/`unchosen`
-- | for a selection — which is what makes the view line say which state it
-- | renders. Where several states are **mutually exclusive**, one classifier
-- | states it (`# provided @"taken" usernameStatus`) and each pane
-- | adopts its own case, so two panes can never both be on screen — which
-- | separate "should this be visible?" tests can always accidentally allow.
-- |
-- | The pane is handed exactly the case payload and never the whole model,
-- | and it is removed from the page while the model sits elsewhere. Two
-- | things follow: the rule for *what is on screen when* lives in business
-- | code where it can be tested, and a pane that is absent contributes
-- | nothing — so anything downstream waiting on it waits, rather than
-- | showing a stale or invented value.
provided :: forall @l i a b s o. IsSymbol l => Cons l a b s => (i -> [ | s ]) -> PUI Web a o -> PUI Web i o
provided f w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ unwrap w
  pure
    { toUser: \fed -> case prj (Proxy @l) (f fed) of
      Nothing -> ensureDetached
      Just y -> do
        -- attach before feeding: a UI component that measures itself on toUser (the
        -- MDC slider positions its thumb from the track width) needs to be in
        -- the document first, or it lays out against a zero-width detached node
        ensureAttached
        toUser y
    , fromUser
    }

-- | Style by data: the class is on exactly while the test holds for what is
-- | being shown — the strike-through on a done todo, the error colour on an
-- | overdrawn amount, the highlight on the selected row.
-- |
-- | Styling only. To make something *appear and disappear*, use
-- | `provided`, which takes the content away with the pane instead of
-- | leaving it on the page in a different colour. Applies to the last
-- | element built, not to a group of siblings.
clWhen :: forall i o. (i -> Boolean) -> String -> PUI Web i o -> PUI Web i o
clWhen pred name w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { toUser: \fed -> do
        (if pred fed then addClass else removeClass) node name
        w'.toUser fed
    , fromUser: w'.fromUser
    }

-- | An attribute computed from the data being shown — a swatch's colour, a
-- | circle's centre, a bar's width, a cell's inline style
-- | (`circle >>> attrWith "cx" (show <<< _.x)`).
-- |
-- | This is what keeps a drawing or a large grid from being rebuilt: the
-- | element is created once and restyled in place as values arrive, so
-- | selection, focus and scrolling survive every update. Pair it with
-- | `foreach` for a collection whose elements are never torn down.
attrWith :: forall i o. String -> (i -> String) -> PUI Web i o -> PUI Web i o
attrWith name valueOf w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { toUser: \fed -> do
        setAttribute node name (valueOf fed)
        w'.toUser fed
    , fromUser: w'.fromUser
    }

-- | Make any element clickable: it reports whatever it is currently
-- | showing. A grid cell, a list row, a chip, a picture — the content is
-- | the display, the click is the report, so the identity of what was
-- | picked comes from what was on screen and cannot be got wrong. A click
-- | before the element has been shown anything does nothing.
-- | Row-shaped: the click **replays** the last value fed, and replay is
-- | lawful over records only — an entity's value may be re-said, a
-- | one-shot event may not (the `looped`/`observed`/`simpleDialog`
-- | argument). **The content subsumes** (it is a display — the baked-in
-- | reads-narrow rule): it may read a closed sub-row of the replayed row,
-- | and pure chrome states `{}`, so `clicked staticChrome` needs no
-- | adapter.
clicked :: forall @narrow @extra r o. Union narrow extra r => PUI Web { | narrow } o -> PUI Web { | r } { | r }
clicked w = wrap do
  w' <- unwrap (widenRecordInput w)
  node <- gets _.sibling
  iRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \fed -> do
        Ref.write (Just fed) iRef
        w'.toUser fed
    , fromUser: \prop -> do
        -- content is display-only: give its wiring a sink so echoes flow
        w'.fromUser \_ -> pure unit
        void $ addEventListener "click" node $ const do
          mi <- Ref.read iRef
          for_ mi \fed -> prop fed
    }

-- | Report *where* the user clicked, in the container's own coordinates —
-- | inside an `<svg>` those are its drawing coordinates, so a click and the
-- | shapes are in the same units whatever size the drawing is on screen.
-- | The canvas gesture, where the place clicked *is* the interaction:
-- | `svg >>> "viewBox" := "0 0 500 300" $ onClickedXY $ …`.
onClickedXY :: forall i o. PUI Web i o -> PUI Web i { x :: Number, y :: Number }
onClickedXY content = wrap do
  w' <- unwrap content
  node <- gets _.parent
  pure
    { toUser: w'.toUser
    , fromUser: \prop -> do
        w'.fromUser \_ -> pure unit
        onClickXY node \x y -> prop { x, y }
    }

-- | Build a UI component per element from a function — for a list whose elements
-- | differ in *shape*, not just in value: the blocks of a rendered markdown
-- | document, where one is a heading and the next a list
-- | (`el ("h" <> show level)`).
-- |
-- | The container is redrawn whenever the list arrives, so use it only when
-- | the shape really does vary. When the shape is fixed and only the values
-- | move, `foreach` with `text` and `attrWith` updates the same elements in
-- | place instead — no flicker, nothing losing focus. It owns the element it
-- | sits in, so give it its own container rather than a shared one.
foreachWith :: forall a o. (a -> PUI Web {} o) -> PUI Web (Array a) o
foreachWith build = wrap do
  parent <- gets _.parent
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \items -> do
        removeAllChildren parent
        for_ items \item -> do
          w' <- runDomInNode parent (unwrap (build item))
          mProp <- Ref.read propRef
          for_ mProp \prop -> w'.fromUser prop
          void $ w'.toUser {}
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | Draw a UI component from a function for a single value and
-- | redraw when the value changes — the scene whose whole composition
-- | depends on the data. `div $ dynamic renderSwatch`. Owns the element it
-- | sits in.
dynamic :: forall a o. (a -> PUI Web {} o) -> PUI Web a o
dynamic build = wrap $ unwrap (foreachWith build) <#> \w ->
  { toUser: \value -> w.toUser [ value ], fromUser: w.fromUser }

-- | Lay out a list that is known up front and never changes — the courses
-- | on a menu, the keys of a keypad, a row of preset swatches:
-- | `ul $ each courses renderCourse`, `tr $ each keys keyComponent`. Nothing
-- | about it comes from the model, so it sits among fixed decoration.
each :: forall a o. Array a -> (a -> PUI Web {} o) -> PUI Web {} o
each items build = wrap $ unwrap (foreachWith build) <#> \w ->
  { toUser: \_ -> w.toUser items, fromUser: w.fromUser }

-- Entry point

-- | Mount the app in the page's `<body>` — the one call an application
-- | makes: `body $ with initialOrder $ …` or `body $ … $ screen # mvu
-- | initialGame`.
-- |
-- | The app has to be **complete**: everything on screen must have a value
-- | from the first frame, and `with`/`mvu` are where that starting state is
-- | supplied. Anything left unsupplied is reported here as a compile error
-- | naming the missing pieces — a screen can't reach a user half-filled.
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptHostDiagnostics
  node <- documentBody
  runDomInNode node do
    { fromUser } <- unwrap ui
    liftEffect $ fromUser \_ -> pure unit

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

-- | Any element by name — for a tag this vocabulary has no name for, and
-- | for a tag computed at runtime (`el ("h" <> show level)`). The named
-- | oculars above are all `el` at a fixed tag.
el :: String -> Ocular (PUI Web)
el tagName = wrap <<< element tagName <<< unwrap

