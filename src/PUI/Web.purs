-- | The browser carrier, and the root of everything web-specific: the `Web`
-- | monad (`StateT DOM Effect`) the algebra is instantiated at for the
-- | browser, the DOM building blocks and FFI, and the **element-neutral
-- | vocabulary** every vocabulary beneath this module is written with.
-- |
-- | No *element* lives here — elements are the submodules' to name: the
-- | element vocabularies `PUI.Web.HTML` and `PUI.Web.SVG`, and one module
-- | per design system — `PUI.Web.MDC2`, `PUI.Web.MDC3`, `PUI.Web.Shoelace`,
-- | `PUI.Web.Fluent`, `PUI.Web.Bootstrap`. What lives here instead works on
-- | whatever element was built last, or on no element at all, so SVG and
-- | every design system use it as readily as HTML does:
-- |
-- | - **decorators** — `attr`/`:=` and `attrDyn`/`:=>` (static and
-- |   effect-computed attributes), `cl`, and the channel-fed `attrWith` and
-- |   `clWhen`; `init` for per-element setup;
-- | - **text leaves** — `text` (copy from a read function), `textOf` (a
-- |   status's payload), `staticString`, and `staticHTML`, kept off the public
-- |   vocabularies (L10);
-- | - **occurrence sources** — `clicked @l f`, `onClickedXY @l`;
-- | - **visibility and the gated displays** — `provided`, and the rungs
-- |   `shown`, `shownWhen`, `inCase`, `shownEach`;
-- | - **structure from data** — `dynamic`, `each`, and `el` for a computed
-- |   tag.
-- |
-- | There are two ways to draw a screen from data, and the choice is visible
-- | to the user. When the **shape is fixed and only the values move** — a
-- | spreadsheet grid, an SVG canvas, a table of orders — build the shape
-- | once and let data flow through it (`foreach` from `PUI` for the repeated
-- | part, `text` for contents, `attrWith`/`clWhen` for anything computed):
-- | elements are updated in place, so nothing loses focus, scroll position
-- | or a half-finished gesture when a value changes. When the **shape itself
-- | depends on the data** — a rendered markdown document, where one block is
-- | a heading and the next a list — `dynamic` and `each` build it from a
-- | function and redraw when it changes.
module PUI.Web
  ( DOM
  , Event
  , Node
  , OptCaption(..)
  , choice
  , selectedAt
  , clearedOnRepress
  , selectedOptionalAt
  , selectedUnpickedAt
  , Web
  , addClass
  , addEventListener
  , appendChild
  , attachable
  , attribute
  , clazz
  , createElementNS
  , createTextNode
  , createCommentNode
  , adoptHostDiagnostics
  , documentBody
  , element
  , htmlNS
  , getChecked
  , getValue
  , isFocused
  , onInputDebounced
  , onClickXY
  , removeAllChildren
  , removeAttribute
  , removeClass
  , runDomInNode
  , setAttribute
  , setChecked
  , staticHTML
  , setTextNodeValue
  , setValue
  , textContent
  , uniqueId
  , shown
  , shownWhen
  , inCase
  , shownEach
  , text
  , textOf
  , staticString
  , staticText
  , attr
  , (:=)
  , cl
  , init
  , attrDyn
  , (:=>)
  , provided
  , clWhen
  , attrWith
  , clicked
  , onClickedXY
  , dynamic
  , each
  , el
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import ConvertableOptions (class ConvertOption)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, inj, prj)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import PUI (class Hosting, Ocular, PUI, Logged, diagnosticsOn, foreach, muted, replaying, setDiagnostics, setSink, setTracing)
import Data.Profunctor.Row (class OwnedRecordOutputs, class SharedRecordInputs)
import Data.Profunctor.Row.RecordToRecord (focusField, recordToRecord)
import Prim.Row (class Cons, class Union)
import Prim.RowList (Nil) as RL
import Unsafe.Coerce (unsafeCoerce)

foreign import data Node :: Type

-- Builds Web Document keeping track of parent/last sibling node
newtype Web a = Web (StateT DOM Effect a) -- TODO rename to DocumentBuilder?

type DOM =
  { parent :: Node
  , sibling :: Node -- last sibling
  }

derive newtype instance Functor Web
derive newtype instance Apply Web
derive newtype instance Applicative Web
derive newtype instance Bind Web
derive newtype instance Monad Web
derive newtype instance MonadEffect Web
derive newtype instance MonadState DOM Web

uniqueId :: Effect String
uniqueId = randomElementId

-- others

attachable :: forall r. Web r -> Web { result :: r, ensureAttached :: Effect Unit, ensureDetached :: Effect Unit }
attachable dom = do
  parent <- gets _.parent
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  { ensureAttached, ensureDetached, initialDocumentFragment } <- liftEffect do
    placeholderBefore <- placeholderBeforeSlot slotNo
    placeholderAfter <- placeholderAfterSlot slotNo

    appendChild placeholderBefore parent
    appendChild placeholderAfter parent

    initialDocumentFragment <- createDocumentFragment
    detachedDocumentFragmentRef <- Ref.new $ Just initialDocumentFragment

    let
      ensureAttached :: Effect Unit
      ensureAttached = do
        detachedDocumentFragment <- Ref.modify' (\documentFragment -> { state: Nothing, value: documentFragment}) detachedDocumentFragmentRef
        for_ detachedDocumentFragment \documentFragment -> do
          removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
          documentFragment `insertBefore` placeholderAfter

      ensureDetached :: Effect Unit
      ensureDetached = do
        detachedDocumentFragment <- Ref.read detachedDocumentFragmentRef
        when (isNothing detachedDocumentFragment) do
          documentFragment <- createDocumentFragment
          moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
          Ref.write (Just documentFragment) detachedDocumentFragmentRef

    pure $ { ensureAttached, ensureDetached, initialDocumentFragment }
  modify_ _ { parent = initialDocumentFragment }
  result <- dom
  newSibling <- liftEffect $ lastChild initialDocumentFragment
  modify_ _ { parent = parent, sibling = newSibling}
  pure { ensureAttached, ensureDetached, result }
placeholderBeforeSlot :: Int -> Effect Node
placeholderBeforeSlot slotNo = createCommentNode $ "begin slot " <> show slotNo

placeholderAfterSlot :: Int -> Effect Node
placeholderAfterSlot slotNo = createCommentNode $ "end slot " <> show slotNo

--- private

-- | The two namespaces the DOM builder distinguishes; SVG needs its elements
-- | created with `createElementNS`, or the browser treats them as unknown HTML.
htmlNS :: String
htmlNS = "http://www.w3.org/1999/xhtml"

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

-- | The namespace rule for `element`: an `svg` tag opens the SVG namespace;
-- | every other element inherits its parent's.
childNS :: String -> String -> String
childNS parentNS tagName = if tagName == "svg" then svgNS else parentNS

element :: forall a. String -> Web a -> Web a
element tagName contents = do
  parentNode <- gets _.parent
  parentNS <- liftEffect $ namespaceURI parentNode
  -- HTML elements go through plain createElement (MDC's component init is
  -- sensitive to how form controls are created); only SVG-namespaced elements
  -- need createElementNS.
  let ns = childNS parentNS tagName
  newNode <- liftEffect $ if ns == svgNS then createElementNS ns tagName else createElement tagName
  liftEffect $ appendChild newNode parentNode
  modify_ _ { parent = newNode}
  result <- contents
  modify_ _ { parent = parentNode, sibling = newNode}
  pure result

attribute :: String -> String -> Web Unit
attribute name value = do
  node <- gets _.sibling
  liftEffect $ setAttribute node name value

-- read: class
clazz :: String -> Web Unit
clazz name = do
  node <- gets _.sibling
  liftEffect $ addClass node name
  pure unit

foreign import data Event :: Type
foreign import isFocused :: Node -> Effect Boolean
foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import textContent :: Node -> Effect String
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import documentBody :: Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElement :: String -> Effect Node
foreign import createElementNS :: String -> String -> Effect Node
foreign import namespaceURI :: Node -> Effect String
foreign import insertBefore :: Node -> Node -> Effect Unit
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import removeAllNodesBetweenSiblings :: Node -> Node -> Effect Unit
foreign import appendRawHtml :: String -> Node -> Effect Node
foreign import moveAllNodesBetweenSiblings :: Node -> Node -> Node -> Effect Unit
foreign import afterTask :: Effect Unit -> Effect Unit
foreign import addEventListener :: String -> Node -> (Event -> Effect Unit) -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import setAttribute :: Node -> String -> String -> Effect Unit
foreign import removeAttribute :: Node -> String -> Effect Unit
foreign import removeAllChildren :: Node -> Effect Unit
foreign import removeChild :: Node -> Node -> Effect Unit
foreign import addClass :: Node -> String -> Effect Unit
foreign import removeClass :: Node -> String -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
foreign import randomElementId :: Effect String
foreign import lastChild :: Node -> Effect Node

-- | Pointer-down emitter with coordinates mapped into the element's local
-- | space (an SVG's viewBox units when present, CSS pixels otherwise) —
-- | works for mouse, touch and pen alike.
foreign import onClickXY :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit
foreign import onInputDebounced :: Node -> Number -> (String -> Effect Unit) -> Effect Unit

foreign import hostTracing :: Effect Boolean
foreign import hostDiagnostics :: Effect Boolean
foreign import traceSink :: String -> Logged -> Effect Unit
foreign import warnSink :: String -> Array String -> Effect Unit

-- | Hand the browser's console and diagnostics switches to `PUI`'s diagnostics,
-- | which take all three as parameters and have no JavaScript of their own:
-- | `window.__bambikTrace = true`
-- | (or `localStorage.setItem("bambik-trace", "true")`) turns the emission
-- | trace on, and `window.__bambikNoWarn = true` silences the starvation
-- | watchdog. Called at the mount entries, so a carrier that never mounts —
-- | the `Effect` probe carrier the law tests run on — leaves both off.
adoptHostDiagnostics :: Effect Unit
adoptHostDiagnostics = do
  setSink { trace: traceSink, warn: warnSink }
  hostTracing >>= setTracing
  hostDiagnostics >>= setDiagnostics

runDomInNode :: forall a. Node -> Web a -> Effect a
runDomInNode node (Web domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node }

-- | The DOM carrier hosts collection children under the enclosing parent
-- | (`PUI`'s container action): the freshly appended child is the instance's
-- | node, detach removes it, restack re-appends in key order (`appendChild`
-- | moves an existing node, so identity — focus, local state — travels with
-- | it).
instance Hosting Web Node where
  hosting w = do
    parent <- gets _.parent
    pure
      { instantiate: do
          inst <- runDomInNode parent (unwrap w)
          node <- lastChild parent
          pure { feed: inst.toUser, subscribe: inst.fromUser, node }
      , detach: \node -> removeChild node parent
      , restack: \nodes -> for_ nodes \node -> appendChild node parent
      }

-- | Fixed decoration given as a raw markup string — for chrome a design
-- | system only documents as markup. Like `staticString` it never changes and
-- | carries no data; unlike it, the string is inserted as markup, so it must
-- | be written in the source and never assembled from model or user text.
-- |
-- | **Internal chrome plumbing** (L10): it lives here, on the carrier, rather
-- | than in the `PUI.Web.HTML` vocabulary, because an HTML-string surface must
-- | not be part of the public vocabulary an application composes from. The
-- | design-system modules reach it; application code does not.
staticHTML :: String -> PUI Web {} {}
staticHTML html = wrap do
  parent <- gets _.parent
  newNode <- liftEffect $ appendRawHtml html parent
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
    }

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0

-- | Marks a leaf's caption field (`label`/`floatingLabel`) as optional —
-- | left out, it defaults to the row label **verbatim**, which is why a
-- | label is written as the copy it draws (`@"First name"`, quoted because
-- | human copy is no identifier). Nothing derives a caption from an
-- | identifier: real copy that the label cannot be — localized wording,
-- | units — belongs in the config. The `ConvertOptionsWithDefaults` tag
-- | the design systems' captioned leaves share.
-- |
-- | The stamp invariant, vocabulary-wide: **every label-indexed leaf
-- | stamps its label on its host element** — `name` where the element is
-- | a form citizen, `aria-label` where it is a display — so inspecting
-- | any element answers which `@l` in the code it is.
-- | `text` is outside the family: copy is a function, not a
-- | field, so it carries no label to stamp — under host diagnostics it
-- | plants a bare `text` comment marker, and in production nothing.
-- |
-- | The invariant's second half is the **accessible name**: it must be the
-- | caption *verbatim*. Browsers compute names from RENDERED text, so a
-- | catalogue's styling would leak into them — MD2's stylesheet uppercases
-- | buttons, tabs and segments, once computing a `@"Count"` button's name
-- | as "COUNT" — therefore every face whose styling transforms its caption
-- | stamps `aria-label` with the caption verbatim (MDC2's button family,
-- | tabs, segments and the menu anchor), keeping name = label exactly, in
-- | every vocabulary and browser alike — and so does every face the platform
-- | cannot name by itself: MD3's checkbox and switch sit inside a wrapping
-- | `<label>` whose association never reaches the input in their shadow
-- | root, so the switch stamps its label and the checkbox the rendered text
-- | of its content. The smoke harness enforces both
-- | halves: role + accessible-name locators match exactly
-- | (scripts/smoke/a11y.mjs), and axe's name-and-reference rules run per
-- | vocabulary (tests/a11y-laws.mjs).
data OptCaption = OptCaption

instance ConvertOption OptCaption sym a a where
  convertOption _ _ = identity

-- | One selector **option**, named by its case: `choice @"Boardroom"` both
-- | injects that case and draws its label, so a choice states its copy once
-- | — exactly as a captioned leaf does:
-- |
-- | ```
-- | dropdown @"Room" {} [ choice @"Focus pod (4 seats)", choice @"Boardroom (12 seats)" ]
-- | ```
-- |
-- | The options stay a plain array, so their order is the order they are
-- | written in. That matters: a selector's option order is a design decision
-- | (rooms by size, durations by length), and it deliberately does **not**
-- | come from the variant row, which the compiler sorts alphabetically.
choice :: forall @l tail r. IsSymbol l => Row.Cons l {} tail r => { value :: Variant r, label :: String }
choice = { value: inj (Proxy :: Proxy l) {}, label: reflectSymbol (Proxy :: Proxy l) }

-- | Lift a bare selection leaf — the option to check in (`Nothing`: none),
-- | the option the user checked out (`Nothing`: cleared) — to the
-- | **selection editor** of field `l`, a whole-row citizen
-- | `{ l :: a | rest } → { l :: a | rest }` like every other editor: the
-- | widget stores a value, so it has an editor's shape and owes an editor's
-- | answer — every feed is answered with the row, and every pick is stored
-- | into field `l`. Here the field is the option itself, so the model has a
-- | choice at all times. Vocabulary plumbing, beside `focusField @l`: every
-- | plain selector in every vocabulary is its leaf lifted with this.
selectedAt :: forall @l a rest r. IsSymbol l => Row.Cons l a rest r => PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
selectedAt = selectedWith @l Just identity

-- | `selectedAt` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case the
-- | application names. Every other case shows nothing checked, a pick
-- | stores case `c` and there is no way back, so the stages demanding the
-- | selection adopt the made case. Every `…Unpicked` selector is its leaf
-- | lifted with this.
selectedUnpickedAt :: forall @l @c a b s rest r. IsSymbol l => IsSymbol c => Row.Cons c a b s => Row.Cons l (Variant s) rest r => PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
selectedUnpickedAt = selectedWith @l (prj (Proxy @c)) (map (inj (Proxy @c)))

-- | `selectedAt` for a choice the user may leave unmade: field `l` is a
-- | variant whose case `c` is the made choice and whose case `n` is none,
-- | and clearing the widget stores case `n`. Every `…Optional` selector is
-- | its leaf lifted with this.
selectedOptionalAt :: forall @l @c @n a b t s rest r. IsSymbol l => IsSymbol c => IsSymbol n => Row.Cons c a b s => Row.Cons n {} t s => Row.Cons l (Variant s) rest r => PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
selectedOptionalAt = selectedWith @l (prj (Proxy @c)) (Just <<< maybe (inj (Proxy @n) {}) (inj (Proxy @c)))

-- | The clearing gesture of an optional radio group: the platform's radios
-- | cannot be unchecked, so pressing the checked member again (pointer or
-- | key, on `press`) and letting its `click` land clears the group. The
-- | press snapshots the selection, so a click the platform synthesizes for
-- | an arrow-key move to another member never clears; the clear runs after
-- | the element's own click handling, which re-checks it. Vocabulary plumbing
-- | for the `…Optional` radio leaves.
clearedOnRepress :: forall a m. Eq a => Ref.Ref (Maybe a) -> Array { press :: Node, click :: Node, value :: a | m } -> Effect Unit -> Effect Unit
clearedOnRepress selRef members clear = do
  pressedRef <- Ref.new Nothing
  for_ members \m -> do
    let
      snapshot = do
        sel <- Ref.read selRef
        Ref.write (if sel == Just m.value then sel else Nothing) pressedRef
    void $ addEventListener "pointerdown" m.press (const snapshot)
    void $ addEventListener "keydown" m.press (const snapshot)
    void $ addEventListener "click" m.click $ const do
      pressed <- Ref.read pressedRef
      Ref.write Nothing pressedRef
      -- after the element's own click handling, which re-checks it
      when (pressed == Just m.value) (afterTask clear)

-- checkedOf: the option the field shows checked; stored: what a pick (`Just`)
-- or a clear (`Nothing`) stores, if anything; `checkedOf <=< stored` must give
-- back the pick, so a pick's echo is the pick
selectedWith :: forall @l f a rest r. IsSymbol l => Row.Cons l f rest r => (f -> Maybe a) -> (Maybe a -> Maybe f) -> PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
selectedWith checkedOf stored w = focusField @l $ wrap do
  w' <- unwrap w
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \v -> do
        w'.toUser (checkedOf v)
        Ref.read mPropRef >>= traverse_ (_ $ v)
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        w'.fromUser (traverse_ prop <<< stored)
    }

-- The element-neutral vocabulary (see the module header).

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
   . IsSymbol l => Cons l { | a } b s
  => Union read extra row
  => SharedRecordInputs row row row i12 i1x i2x
  => OwnedRecordOutputs () row row RL.Nil rowL
  => ({ | read } -> [ | s ]) -> PUI Web { | a } {} -> PUI Web { | row } { | row }
shownWhen f content = recordToRecord (attachedOn @l (\(r :: { | row }) -> f (unsafeCoerce r)) content) identity

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
-- | its `focusField @l` lift already re-attaches the rest of the row. The
-- | classifier reads a closed narrow row (the row-stating exception:
-- | `fulfillment :: { selected :: [ … ] } -> [ … ]`), exactly as
-- | `shownWhen`'s does. One release per feed either way: attached, the
-- | editor's own echo is the release; detached, the wire speaks for the
-- | absent editor. What the edit does to the rest of the row is a `settled`
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
          Nothing -> do
            ensureDetached
            -- the wire speaks only for the absent editor: attached, the
            -- editor's own echo is the release (one feed, one release)
            mProp <- Ref.read propRef
            for_ mProp \prop -> prop row
          -- attach before feeding, as `provided` does
          Just _ -> ensureAttached *> pane.toUser row
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

-- | Show a string that changes — a readout, a total, a sentence, a name in
-- | a list row. (Wording that doesn't change is `staticString`.)
-- |
-- | **Copy is a function, not a field**: the argument is the read — a named
-- | function from the fields it needs to the words on the screen, living in
-- | the logic module where it is one pure function and one unit test
-- | (`text progressLineOf`, `text _.title`). The read function's own
-- | signature states the footprint, and the stage that hosts the display
-- | (`shown`/`shownWhen`/`shownEach`) widens it to the fed row, so no call
-- | site coerces. This is why `text` takes no label:
-- | its content *is* the copy, so there is no field to name and nothing to
-- | caption — a caption is surrounding chrome (`staticString`, a `label`, a
-- | column header). A leaf that renders a *number* keeps its label and
-- | reads its field verbatim (`progressBar @"fraction"`): numbers need no
-- | formatting.
-- |
-- | A whole line is one function, glue included — never several leaves with
-- | `staticString` between them, and never a formatter in the view.
-- | doc/research-copy-is-a-function.md is the rationale.
text :: forall reads. ({ | reads } -> String) -> PUI Web { | reads } {}
text = textLeaf

-- | `text`'s variant-input sibling: a status renders its own payload
-- | (`textOf eventText`), a `+→×` leaf. Vocabulary-internal — application
-- | code shows copy with `text`, whose row-shaped read states its footprint.
textOf :: forall r. ([ | r ] -> String) -> PUI Web [ | r ] {}
textOf = textLeaf

textLeaf :: forall a. (a -> String) -> PUI Web a {}
textLeaf f = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    -- a text node carries no attributes, so under host diagnostics a bare
    -- comment marks it (the leaf has no label to stamp: copy is a function)
    diag <- diagnosticsOn
    when diag do
      marker <- createCommentNode "text"
      appendChild marker parentNode
    node <- createTextNode ""
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  node <- gets (_.sibling)
  propRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  pure
    { toUser: \s -> do
        setTextNodeValue node (f s)
        -- the display's answer to the feed (record-echo totality: the whole
        -- of a `{}` output row is `{}`). Inert to every gate — a zero-field
        -- side is pre-satisfied, so a display never enters one — but real to
        -- sequencing: a stage after the display is fed it (`simpleDialog`'s
        -- confirm replays what its content last answered). Nothing at
        -- registration, since an answer needs a feed. Every `{}`-output
        -- display follows this protocol ("display echo, like `text`").
        prop <- Ref.read propRef
        prop {}
    , fromUser: \prop -> Ref.write prop propRef
    }

-- | **Static copy**: text that is part of the structure — on screen before,
-- | and regardless of, any model value — written as a type, so it is known
-- | to the compiler like every label: `h3 (staticText @"Hours")`,
-- | `label $ staticText @"Name "`. Fixed decoration carrying no data,
-- | rendered at build. Copy that is fixed but shows only through data — a
-- | pane's message, the glue of a sentence, a formatted value — is a
-- | *constant*, and lives in a copy function of the logic module
-- | (`text faultLine`); text that *is* data is `staticString`.
staticText :: forall @s. IsSymbol s => PUI Web {} {}
staticText = staticString (reflectSymbol (Proxy @s))

-- | Fixed text given as a runtime string — for vocabulary code captioning
-- | from its configuration, and for closure-known text that is data (a
-- | parsed markdown run). Application copy that is static is `staticText @s`.
staticString :: String -> PUI Web {} {}
staticString content = wrap do
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

-- | Show the **emitter pane** while the model is in state `l`, fed that
-- | state's own data — visibility is **case adoption**: the argument is a
-- | business function classifying the situation into a variant, and the
-- | pane is attached and fed the payload of case `l`, detached on every
-- | other case. A quiz whose run is either `asking` or `finished` shows its
-- | answer list as `listOf … # provided @"asking" quizPhase`; a stopwatch
-- | shows Start only while `halted`.
-- |
-- | The content is an emitter (`×→+`), and that is what makes the pane
-- | lawful: a feed arms an emitter and fires nothing, so a detached one
-- | firing nothing is the same answer — an emitter removed by `provided`
-- | *is* `silence`. The pane family is indexed by what the pane holds, each
-- | member lawful at its shape: `provided` for emitters, `shownWhen` for
-- | displays (releases the fed row whether attached or not), `inCase` for
-- | editors (the wire while detached).
-- |
-- | A `Maybe`-gated pane is the same
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
provided :: forall @l i a b s o. IsSymbol l => Cons l { | a } b s => ({ | i } -> [ | s ]) -> PUI Web { | a } [ | o ] -> PUI Web { | i } [ | o ]
provided = attachedOn @l

-- The pane mechanism every pane shares, at any content output: attach and
-- feed the payload on case `l`, detach on every other case. Public only
-- through its lawful restrictions — `provided` at emitter content, where a
-- detached pane's silence is `×→+`'s Answer; `shownWhen`, which merges it
-- with the wire so a display pane answers every feed with the row.
attachedOn :: forall @l i a b s o. IsSymbol l => Cons l { | a } b s => ({ | i } -> [ | s ]) -> PUI Web { | a } o -> PUI Web { | i } o
attachedOn f w = wrap do
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
clWhen :: forall i o. ({ | i } -> Boolean) -> String -> PUI Web { | i } o -> PUI Web { | i } o
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
attrWith :: forall i o. String -> ({ | i } -> String) -> PUI Web { | i } o -> PUI Web { | i } o
attrWith name valueOf w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { toUser: \fed -> do
        setAttribute node name (valueOf fed)
        w'.toUser fed
    , fromUser: w'.fromUser
    }

-- | Make any element clickable: it reports, as case `l`, `f` of whatever it
-- | is currently showing. A grid cell, a list row, a chip, a picture — the
-- | content is the display, the click is the report, so the identity of
-- | what was picked comes from what was on screen and cannot be got wrong:
-- | `clicked @"picked" _.key (td $ text _.text)`. A click before the
-- | element has been shown anything does nothing. An **event source**,
-- | `× → +` by shape as by behaviour: the click **replays** the last row
-- | fed — replay is lawful over records only, an entity's value may be
-- | re-said where a one-shot event may not (the `looped`/`observed`/
-- | `simpleDialog` argument) — and leaves as an occurrence of `l`, so
-- | nothing record-shaped ever stands for a click. **The content
-- | subsumes** (it is a display — the baked-in reads-narrow rule): it may
-- | read a closed sub-row of the replayed row, and pure chrome states `{}`,
-- | so `clicked @l f staticChrome` needs no adapter.
-- |
-- | The **replay contract**, a law of this word rather than of the shape
-- | (the shape's two are Data.Profunctor.Row's Repetition and Answer, both
-- | held: a feed rewrites the replay slot, and a feed never fires): a click
-- | emits `f` of the row last fed, as case `l`; before the first feed a
-- | click emits nothing.
clicked :: forall @l @narrow @extra r o k s. IsSymbol l => Cons l k () s => Union narrow extra r => ({ | r } -> k) -> PUI Web { | narrow } o -> PUI Web { | r } [ | s ]
clicked f w = replaying @l f (occurrences w)

-- The click source `clicked` is built from: each click on the last-built
-- element (the content's own node) leaves as an occurrence carrying nothing;
-- the content is fed the row and its output written off. The `× → +`
-- leaf's occurrence half — at the closed empty row, `occurrences chrome ::
-- PUI Web {} [ occurred :: {} ]` is the point's dual, an occurrence out of
-- the terminal record (`ticks` is its timer sibling in `PUI`). Private, with
-- one fixed case: the business label is `clicked @l`'s to state, and
-- `replaying @l` relabels while it attaches the row — replay is `Strong`'s
-- retention, so no source keeps a copy of the row it was shown.
occurrences :: forall i o. PUI Web i o -> PUI Web i [ occurred :: {} ]
occurrences w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { toUser: w'.toUser
    , fromUser: \prop -> do
        -- content is display-only: give its wiring a sink so echoes flow
        w'.fromUser \_ -> pure unit
        void $ addEventListener "click" node $ const $ prop (inj (Proxy @"occurred") {})
    }

-- | Report *where* the user clicked, in the container's own coordinates —
-- | inside an `<svg>` those are its drawing coordinates, so a click and the
-- | shapes are in the same units whatever size the drawing is on screen.
-- | The canvas gesture, where the place clicked *is* the interaction:
-- | `svg >>> "viewBox" := "0 0 500 300" $ onClickedXY @"picked" $ …`. An event
-- | source: the point leaves as an occurrence of case `l`.
onClickedXY :: forall @l i o s. IsSymbol l => Cons l { x :: Number, y :: Number } () s => PUI Web { | i } o -> PUI Web { | i } [ | s ]
onClickedXY content = wrap do
  w' <- unwrap content
  node <- gets _.parent
  pure
    { toUser: w'.toUser
    , fromUser: \prop -> do
        w'.fromUser \_ -> pure unit
        onClickXY node \x y -> prop (inj (Proxy @l) { x, y })
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
dynamic :: forall a o. ({ | a } -> PUI Web {} { | o }) -> PUI Web { | a } { | o }
dynamic build = wrap $ unwrap (foreachWith build) <#> \w ->
  { toUser: \value -> w.toUser [ value ], fromUser: w.fromUser }

-- | Lay out a list that is known up front and never changes — the courses
-- | on a menu, the keys of a keypad, a row of preset swatches:
-- | `ul $ each courses renderCourse`, `tr $ each keys keyComponent`. Nothing
-- | about it comes from the model, so it sits among fixed decoration.
-- | It answers each feed once, with `{}`, for the whole list: the elements'
-- | own `{}` answers are absorbed, so a list of n elements is one answer,
-- | not n (Answer at `×→×`).
each :: forall a. Array a -> (a -> PUI Web {} {}) -> PUI Web {} {}
each items build = wrap do
  w <- unwrap (foreachWith build)
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \_ -> do
        w.toUser items
        Ref.read propRef >>= traverse_ (_ $ {})
    , fromUser: \prop -> do
        w.fromUser \_ -> pure unit
        Ref.write (Just prop) propRef
    }

-- Entry point

-- | Any element by name — for a tag this vocabulary has no name for, and
-- | for a tag computed at runtime (`el ("h" <> show level)`). The named
-- | oculars above are all `el` at a fixed tag.
el :: String -> Ocular (PUI Web)
el tagName = wrap <<< element tagName <<< unwrap
