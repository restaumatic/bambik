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
-- |   effect-computed attributes), `cl`, and the channel-fed `attrWith`,
-- |   `attrAt` and `clWhen`; `init` for per-element setup;
-- | - **text leaves** — `text` (copy from a read function), `textAt` (a
-- |   field shown as stored), `statusText` (a status's caption), `staticText`,
-- |   and `staticHTML`, kept off the public vocabularies (L10);
-- | - **occurrence sources** — `onClickedXY @l`, `onHoveredXY @l`,
-- |   `onContextMenuXY @l`;
-- | - **panes** (re-exported from `PUI`, where they run over any
-- |   `Hosting` carrier) — `provided` (emitters) and `inCase` (displays and
-- |   editors), each fed the whole row; a pane over a variant the model
-- |   stores names its field by label (`providedAt`, `inCaseAt`);
-- | - **sources** — the browser's facts (`geolocated`, `online`, `clock`,
-- |   `colorScheme`) over `PUI`'s `source`, each owning one field the app
-- |   cannot seed;
-- | - **structure from data** — `dynamic`, `each`, and `el` for a computed
-- |   tag;
-- | - **leaf building blocks** — what every vocabulary's leaves are built
-- |   from over `PUI`'s leaf wirings (`recordToRecordWiring`,
-- |   `variantToRecordWiring`, `variantToVariantWiring`, `optionWiring` —
-- |   the leaf protocol, host-neutral, so it lives with the carrier, as
-- |   do the click emitter `eventLeaf`, the status `statusOver` and the
-- |   dialogs `confirmedOver`/`acknowledgedOver`): `escaped` (a runtime
-- |   string made safe for `staticHTML`), `listenNode`/`listenNodeDeferred`/
-- |   `listenCapture`, `ensureStyle`, `autoDismiss`, the selector-group
-- |   helpers (`eachMatching`, `hasClass`, `Members`, `clearedByRepress` —
-- |   an optional group's clearing gesture) and the presentation-option
-- |   tags `OptCaption`, `OptLabelIcon`, `OptSelected`, `OptIcon`.
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
  , OptLabelIcon(..)
  , OptSelected(..)
  , invokeMethod
  , showModalOnce
  , fieldLeaf
  , chrome
  , escaped
  , listenNode
  , listenNodeDeferred
  , afterTask
  , eachMatching
  , hasClass
  , listenCapture
  , Members
  , clearedByRepress
  , ensureStyle
  , autoDismiss
  , Web
  , addClass
  , addEventListener
  , appendChild
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
  , onPointerXY
  , removeAllChildren
  , removeAttribute
  , removeClass
  , runDomInNode
  , setAttribute
  , setChecked
  , staticHTML
  , setTextNodeValue
  , setValue
  , uniqueId
  , hole
  , separating
  , geolocated
  , online
  , clock
  , colorScheme
  , onFilePicked
  , module Panes
  , text
  , textAt
  , statusText
  , staticText
  , attr
  , (:=)
  , cl
  , init
  , attrDyn
  , (:=>)
  , clWhen
  , attrWith
  , attrAt
  , onClickedXY
  , onContextMenuXY
  , onHoveredXY
  , dynamic
  , each
  , el
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import ConvertableOptions (class ConvertOption)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Array (concat)
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Data.Variant (inj)
import Type.Proxy (Proxy(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import PUI (inCase, inCaseAt, provided, providedAt) as Panes
import PUI (class Interacting, Gesture(..), fromHandles, handles, recordToRecordWiring, source, class Hosting, class Scoped, Ocular, PUI, Logged, Scope, ScopeState(..), cancelScope, diagnosticsOn, isHole, newScope, onCancel, resumeScope, setDiagnostics, setSink, setTracing, suspendScope)
import Prim.Row (class Cons)
import Record as Record

-- | A value of any type standing for logic not written yet (guardrails L18):
-- | any read of it, or call, throws and marks the page (`__bambikHoleReached`),
-- | so a view run with its logic stubbed to holes that reaches none proves no
-- | data reached the logic. The one exception is the field `__bambikHole`,
-- | which answers `true` — what `PUI.isHole` reads, so a
-- | word consuming a logic value when built can skip a hole.
foreign import hole :: forall a. a

foreign import data Node :: Type

-- Builds Web Document keeping track of parent/last sibling node
newtype Web a = Web (StateT DOM Effect a) -- TODO rename to DocumentBuilder?

type DOM =
  { parent :: Node
  , sibling :: Node -- last sibling
  , scope :: Scope -- the subtree being built (`PUI`'s scope: suspended by a pane, cancelled by a leaver)
  }

derive newtype instance Functor Web
derive newtype instance Apply Web
derive newtype instance Applicative Web
derive newtype instance Bind Web
derive newtype instance Monad Web
derive newtype instance MonadEffect Web
derive newtype instance MonadState DOM Web

-- | The scope of the subtree being built is in the builder's state: a pane
-- | opens a child for its content, a collection one per element.
instance Scoped Web where
  scope = gets _.scope

instance Interacting Web where
  gestured gesture = do
    node <- gets _.sibling
    pure \action -> void $ addEventListener (eventName gesture) node (const action)
    where
    eventName Click = "click"
    eventName DoubleClick = "dblclick"
  focused = do
    node <- gets _.parent
    pure { focusInside: focusWithin node, onDeparture: onDeparture node }


-- others

attachableWeb :: forall r. Web r -> Web { result :: r, ensureAttached :: Effect Unit, ensureDetached :: Effect Unit }
attachableWeb dom = do
  parent <- gets _.parent
  outer <- gets _.scope
  -- the content is built parked, so its scope starts suspended: a tick or
  -- an action inside speaks only while the pane is attached
  child <- liftEffect $ newScope (Just outer) Suspended
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
        resumeScope child

      ensureDetached :: Effect Unit
      ensureDetached = do
        detachedDocumentFragment <- Ref.read detachedDocumentFragmentRef
        when (isNothing detachedDocumentFragment) do
          documentFragment <- createDocumentFragment
          moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
          Ref.write (Just documentFragment) detachedDocumentFragmentRef
        suspendScope child

    pure $ { ensureAttached, ensureDetached, initialDocumentFragment }
  modify_ _ { parent = initialDocumentFragment, scope = child }
  result <- dom
  newSibling <- liftEffect $ lastChild initialDocumentFragment
  modify_ _ { parent = parent, sibling = newSibling, scope = outer }
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
foreign import eachMatching :: String -> Node -> (Node -> Effect Unit) -> Effect Unit
foreign import hasClass :: String -> Node -> Effect Boolean
foreign import listenCapture :: Node -> String -> Effect Unit -> Effect Unit
foreign import addEventListener :: String -> Node -> (Event -> Effect Unit) -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import separatorRule :: Node -> Node -> Node -> Effect (Effect Unit)
foreign import listenNodeDeferred :: Node -> String -> Effect Unit -> Effect Unit
foreign import ensureStyle :: String -> String -> Effect Unit
foreign import autoDismiss :: Node -> String -> Int -> Effect Unit
foreign import invokeMethod :: String -> Node -> Effect Unit
foreign import showModalOnce :: Node -> Effect Unit
foreign import setAttribute :: Node -> String -> String -> Effect Unit
foreign import removeAttribute :: Node -> String -> Effect Unit
foreign import removeAllChildren :: Node -> Effect Unit
foreign import removeChild :: Node -> Node -> Effect Unit
foreign import addClass :: Node -> String -> Effect Unit
foreign import removeClass :: Node -> String -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
foreign import uniqueId :: Effect String
foreign import lastChild :: Node -> Effect Node
foreign import childCount :: Node -> Effect Int
foreign import childrenFrom :: Int -> Node -> Effect (Array Node)

-- | Pointer-down emitter with coordinates mapped into the element's local
-- | space (an SVG's viewBox units when present, CSS pixels otherwise) —
-- | works for mouse, touch and pen alike.
foreign import onPointerXY :: String -> Node -> (Number -> Number -> Effect Unit) -> Effect Unit
foreign import onDeparture :: Node -> Effect Unit -> Effect Unit
foreign import focusWithin :: Node -> Effect Unit
foreign import watchPosition :: ({ lat :: Number, lng :: Number } -> Effect Unit) -> Effect Unit -> Effect (Effect Unit)
foreign import isOnline :: Effect Boolean
foreign import watchOnline :: (Boolean -> Effect Unit) -> Effect (Effect Unit)
foreign import localTime :: Effect { hours :: Int, minutes :: Int, seconds :: Int }
foreign import everyMs :: Number -> Effect Unit -> Effect (Effect Unit)
foreign import prefersDark :: Effect Boolean
foreign import watchColorScheme :: (Boolean -> Effect Unit) -> Effect (Effect Unit)
foreign import onFilePicked :: Node -> ({ name :: String, size :: Number, mime :: String } -> Effect Unit) -> Effect (Effect Unit)
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
runDomInNode node builder = do
  root <- newScope Nothing Active
  runDomInScope root node builder

-- a build under a given scope — a collection's element, a `dynamic` rebuild
runDomInScope :: forall a. Scope -> Node -> Web a -> Effect a
runDomInScope sc node (Web domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node, scope: sc }

-- | The DOM carrier hosts collection children under the enclosing parent
-- | (`PUI`'s container action): the freshly appended child is the instance's
-- | node, detach removes it, restack re-appends in key order (`appendChild`
-- | moves an existing node, so identity — focus, local state — travels with
-- | it). Each element is built in a scope of its own under the collection's,
-- | cancelled when the element leaves.
instance Hosting Web (Array Node) where
  hosting w = do
    parent <- gets _.parent
    outer <- gets _.scope
    pure
      { instantiate: do
          child <- newScope (Just outer) Active
          -- an element is the run of nodes its builder appended — one for a
          -- wrapped item, several for a bare group of siblings, none for `blank`
          before <- childCount parent
          inst <- runDomInScope child parent (handles w)
          nodes <- childrenFrom before parent
          pure { feed: inst.toUser, subscribe: inst.fromUser, node: nodes, cancel: cancelScope child }
      , detach: \nodes -> for_ nodes \node -> removeChild node parent
      , restack: \runs -> for_ (concat runs) \node -> appendChild node parent
      }
  attachable = attachableWeb

-- | Fixed decoration given as a raw markup string — for chrome a design
-- | system only documents as markup. Like `staticText` it never changes and
-- | carries no data; unlike it, the string is inserted as markup, so it must
-- | be written in the source and never assembled from model or user text.
-- |
-- | **Internal chrome plumbing** (L10): it lives here, on the carrier, rather
-- | than in the `PUI.Web.HTML` vocabulary, because an HTML-string surface must
-- | not be part of the public vocabulary an application composes from. The
-- | design-system modules reach it; application code does not.
staticHTMLExact :: String -> PUI Web {} {}
staticHTMLExact html = fromHandles do
  parent <- gets _.parent
  newNode <- liftEffect $ appendRawHtml html parent
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
    }

staticHTML :: forall r. String -> PUI Web { | r } { | r }
staticHTML = built <<< handles <<< staticHTMLExact

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

-- Conversion tags scope which field names lift a bare value to `Just`:
-- a field may be optional on some leaves and required on others (`label`
-- is optional on `button`, required on `slider`), so the *tag*, not a
-- global per-symbol instance, decides which fields are optional for a
-- given leaf. One tag per distinct optional-field set, shared by the
-- vocabularies that have the leaf.

-- | Marks `label` and `icon` as optional on the buttons — write either,
-- | both or neither, as a plain string.
data OptLabelIcon = OptLabelIcon

instance ConvertOption OptLabelIcon "label" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabelIcon "icon" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabelIcon sym a a where
  convertOption _ _ = identity

-- | Marks a list's `selected` test as optional — `listOfAt` selects
-- | nothing.
data OptSelected = OptSelected

instance ConvertOption OptSelected sym a a where
  convertOption _ _ = identity

-- | The editor of field `l` over its leaf: the leaf lifted by
-- | `focusField @l`, so it is a whole-row citizen, and its host stamped
-- | with the label as its `name` (the stamp invariant, `OptCaption`).
-- | Every vocabulary's editors and selectors are this over their own leaf
-- | (`textField = fieldLeaf @l $ inputLeaf …`).
fieldLeaf :: forall @l a b r. IsSymbol l => Cons l a b r => PUI Web a a -> PUI Web { | r } { | r }
fieldLeaf leaf = focusField @l $ "name" := reflectSymbol (Proxy @l) $ leaf

-- | Build a component inside a leaf's builder as **chrome**: placed where
-- | the builder stands and never fed or listened to — a caption, a ripple,
-- | a static's markup beside the element the leaf wires. The one way a
-- | vocabulary places a component it does not wire, so placing chrome
-- | needs no `handles`.
chrome :: forall a b. PUI Web a b -> Web Unit
chrome = void <<< handles

-- | A runtime string made safe to concatenate into `staticHTML` markup —
-- | as text or as a double-quoted attribute value. Every caption, label,
-- | title or URL a vocabulary interpolates into markup passes through it,
-- | so no copy can close a tag or open an attribute.
escaped :: String -> String
escaped = replaceAll (Pattern "&") (Replacement "&amp;")
  >>> replaceAll (Pattern "<") (Replacement "&lt;")
  >>> replaceAll (Pattern ">") (Replacement "&gt;")
  >>> replaceAll (Pattern "\"") (Replacement "&quot;")
  >>> replaceAll (Pattern "'") (Replacement "&#39;")

-- | Listen for an event on a node, the remover discarded: a leaf's node is
-- | detached whole with its listeners, never unwired.
listenNode :: Node -> String -> Effect Unit -> Effect Unit
listenNode node event handler = void $ addEventListener event node (const handler)

-- | The options of a group, as its clearing sees them.
type Members =
  { selector :: String
  , isPicked :: Node -> Effect Boolean
  , unpickAll :: Node -> Effect Unit
  -- register the snapshot of which option was picked before a click: on the
  -- press for radios (whose arrow keys synthesize clicks), on the click's
  -- capture phase for segments
  , beforeClick :: Node -> Effect Unit -> Effect Unit
  }

-- The clearing gesture of an optional group: pressing (pointer or key) the
-- option already picked and letting its click land clears the group and
-- emits case `n`. The press snapshots which option was picked, so a click
-- the platform synthesizes for an arrow-key move never clears; the clear
-- runs after the element's own click handling, which re-checks a radio.
clearedByRepress :: forall @n b v. IsSymbol n => Cons n {} b v => Members -> PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
clearedByRepress members options = fromHandles do
  container <- gets _.parent
  inner <- handles options
  propRef <- liftEffect $ Ref.new Nothing
  liftEffect $ eachMatching members.selector container \node -> do
    pressedRef <- Ref.new false
    members.beforeClick container (members.isPicked node >>= flip Ref.write pressedRef)
    listenNode node "click" do
      pressed <- Ref.read pressedRef
      Ref.write false pressedRef
      when pressed $ afterTask do
        members.unpickAll container
        Ref.read propRef >>= traverse_ (_ $ inj (Proxy @n) {})
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> Ref.write (Just prop) propRef *> inner.fromUser prop
    }

-- The element-neutral vocabulary (see the module header).

-- | A **separator**: a static rule shown only *between* things. The rule
-- | is built before the content, and an observer on their parent keeps it
-- | visible exactly while something rendered precedes it and the content
-- | renders something — a detached pane, an empty collection or a blank
-- | leaf on either side hides it, a feed that fills them shows it again.
-- | "Something" is decided on the DOM, not the data: a non-blank text node,
-- | a replaced or custom element, or an element containing one; comments,
-- | hidden nodes and other rules count for nothing. So the separator is an
-- | ocular — `forall a b`, it reads no row and touches no channel, it only
-- | watches the UI it decorates — and is written leading like every
-- | static: `divider $ slider @"Attendees" # inCaseAt @"chosen" @"Room"`
-- | (meeting-booker: no rule above an absent slider). Every vocabulary's
-- | `divider`, and HTML's `hr`, is `separating` over its own rule.
separating :: forall a b. PUI Web {} {} -> PUI Web a b -> PUI Web a b
separating rule w = fromHandles do
  parentNode <- gets _.parent
  sc <- gets _.scope
  start <- liftEffect $ createCommentNode "separator"
  liftEffect $ appendChild start parentNode
  _ <- handles rule
  ruleEnd <- liftEffect $ lastChild parentNode
  w' <- handles w
  end <- liftEffect $ createCommentNode "/separator"
  liftEffect $ appendChild end parentNode
  disconnect <- liftEffect $ separatorRule start ruleEnd end
  liftEffect $ onCancel sc disconnect
  pure w'

-- | Show a string that changes — a readout, a total, a sentence, a name in
-- | a list row. (Wording that doesn't change is `staticText`.)
-- |
-- | **Copy is a function, not a field**: the argument is the read — a named
-- | function from the fields it needs to the words on the screen, living in
-- | the logic module where it is one pure function and one unit test
-- | (`text progressLineOf`). The read function is typed at
-- | the row its line is fed (the model, or a list's element),
-- | so no call site coerces. This is why `text` takes no label:
-- | its content *is* the copy, so there is no field to name and nothing to
-- | caption — a caption is surrounding chrome (`staticText`, a `label`, a
-- | column header). A field shown as it is stored is not copy but the
-- | field itself, named by its label: `textAt @"title"`.
-- |
-- | A whole line is one function, glue included — never several leaves with
-- | `staticText` between them, and never a formatter in the view.
-- | doc/research-copy-is-a-function.md is the rationale.
-- |
-- | Like every display it is a stage that **changes no data**: the row it
-- | shows is the row it releases, so it is a line of a `Semigroupoid.do`
-- | block like an editor, in its place on the screen.
text :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
text = textLeaf

-- | `text` of one field shown as it is stored: the field is named by its
-- | label, never picked by a function (`textAt @"title"`). A line that
-- | formats, joins or derives is a copy function, `text`.
textAt :: forall @l b r. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textAt = text (Record.get (Proxy @l))

-- | `text`'s status sibling: a status face renders its caption on every
-- | occurrence (`statusText caption`), adopted as its case by `atCase @l`
-- | — a `+→×` leaf. The caption is the case label verbatim, so the face
-- | reads nothing of the payload: a status tells its case. Unlike a static,
-- | nothing shows before the first occurrence. Vocabulary-internal —
-- | application code shows copy with `text`, whose row-shaped read states
-- | its footprint. Unlike the display it renders with, a status **releases
-- | nothing**: the `+→×` merge forwards every release as the whole shared
-- | row, so the leaf's echo is swallowed here and the output is typed at
-- | every row, as the silent component's is.
statusText :: forall a r. String -> PUI Web a { | r }
statusText caption = fromHandles do
  leaf <- handles (textLeaf (const caption))
  pure { toUser: leaf.toUser, fromUser: \_ -> leaf.fromUser \_ -> pure unit }

textLeaf :: forall a. (a -> String) -> PUI Web a a
textLeaf f = fromHandles do
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
  -- the display's answer to the feed is the row it was shown, so the
  -- stage after it is fed it (`confirmed`'s confirm replays what its
  -- content last answered). Nothing at registration, since an answer
  -- needs a feed. Every display follows this protocol (`recordToRecordWiring` with nothing reported).
  -- a hole read renders nothing (guardrails L18): a row may reach a display
  -- before its copy is written, as from a merge of sources
  liftEffect $ recordToRecordWiring { show: \s -> setTextNodeValue node (if isHole f then "" else f s), held: pure false, reports: \_ -> pure unit, read: pure }

-- | **Static copy**: text that is part of the structure — on screen before,
-- | and regardless of, any model value — a value, never a label, since it
-- | names no field and no case: `h3 (staticText "Hours")`, `label $
-- | staticText "Name "`. Fixed decoration carrying no data, rendered at
-- | build, the wire at any row. A static inside a pane is still a static
-- | — the pane decides when it shows, the text reads nothing
-- | (`staticText "Error" # inCaseAt @"faulty" @"condition"`), so a copy
-- | function ignoring its row is a static in the wrong module. Copy that
-- | is fixed but reads data — the glue of a sentence, a formatted value, a
-- | business constant — is a *constant*, and lives in the copy function
-- | of the view model module (`text refundLine`); text that *is* data is
-- | `staticText` too.

-- | Fixed text given as a runtime string — for vocabulary code captioning
-- | from its configuration, and for closure-known text that is data (a
-- | parsed markdown run). Application copy that is static is `staticText (reflectSymbol (Proxy @s))`.
staticTextExact :: String -> PUI Web {} {}
staticTextExact content = fromHandles do
  -- the `{}` it announces at registration is the point's answer to the
  -- terminal record: at `{}` chrome is complete as built
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

staticText :: forall r. String -> PUI Web { | r } { | r }
staticText = built <<< handles <<< staticTextExact

-- | Set a fixed attribute on the element being decorated, written infix as
-- | `:=`: `"placeholder" := "you@example.com" $ input "email" $ …`. For an
-- | attribute that follows the data (a colour, a coordinate, a width), use
-- | `attrWith`.
attr :: String -> String -> Ocular (PUI Web)
attr name value w = fromHandles do
  w' <- handles w
  attribute name value
  pure w'

-- | `attr` written infix: `"src" := url $ img $ …`.
infixr 10 attr as :=

-- | Add a fixed class to the element being decorated — how a design
-- | system's stylesheet is applied. For a class that comes and goes with the
-- | data, use `clWhen`.
cl :: String -> Ocular (PUI Web)
cl name w = fromHandles do
  w' <- handles w
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
init nodeInitializer pre post w = fromHandles do
  w' <- handles w
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
attrDyn name valueFunction w = fromHandles do
  w' <- handles w
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

-- | Style by data: the class is on exactly while the test holds for what is
-- | being shown — the strike-through on a done todo, the error colour on an
-- | overdrawn amount, the highlight on the selected row.
-- |
-- | Styling only. To make something *appear and disappear*, use
-- | `provided`, which takes the content away with the pane instead of
-- | leaving it on the page in a different colour. Applies to the last
-- | element built, not to a group of siblings.
clWhen :: forall r a. ({ | r } -> Boolean) -> String -> PUI Web { | r } a -> PUI Web { | r } a
clWhen pred name w = fromHandles do
  w' <- handles w
  node <- gets _.sibling
  pure
    { toUser: \fed -> do
        (if pred fed then addClass else removeClass) node name
        w'.toUser fed
    , fromUser: w'.fromUser
    }

-- | An attribute computed from the data being shown — a swatch's colour, a
-- | circle's centre, a bar's width, a cell's inline style
-- | (`circle >>> attrWith "style" cellFace`).
-- |
-- | This is what keeps a drawing or a large grid from being rebuilt: the
-- | element is created once and restyled in place as values arrive, so
-- | selection, focus and scrolling survive every update. Pair it with
-- | `foreach` for a collection whose elements are never torn down.
attrWith :: forall r a. String -> ({ | r } -> String) -> PUI Web { | r } a -> PUI Web { | r } a
attrWith name valueOf w = fromHandles do
  w' <- handles w
  node <- gets _.sibling
  pure
    { toUser: \fed -> do
        setAttribute node name (valueOf fed)
        w'.toUser fed
    , fromUser: w'.fromUser
    }

-- | `attrWith` of one field set as it is stored, the field named by its
-- | label before the attribute's name: `circle >>> attrAt @"x" "cx"`.
attrAt :: forall @l b r a. IsSymbol l => Cons l String b r => String -> PUI Web { | r } a -> PUI Web { | r } a
attrAt name = attrWith name (Record.get (Proxy @l))

-- | Report *where* the user clicked, in the container's own coordinates —
-- | inside an `<svg>` those are its drawing coordinates, so a click and the
-- | shapes are in the same units whatever size the drawing is on screen.
-- | The canvas gesture, where the place clicked *is* the interaction:
-- | `svg >>> "viewBox" := "0 0 500 300" $ onClickedXY @"picked" $ …`. An event
-- | source: the point leaves as an occurrence of case `l`. The primary button
-- | only; a right click is `onContextMenuXY`'s.
onClickedXY :: forall @l r a b v. IsSymbol l => Cons l { x :: Number, y :: Number } b v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
onClickedXY = pointedAt @l "pointerdown"

-- | `onClickedXY`'s sibling for the pointer **moving** over the container:
-- | every move leaves as case `l` with the point under the pointer, and so
-- | does the point where it leaves — the hover gesture, what lies under the
-- | pointer *is* the interaction (circle-drawer's nearest circle). Merged
-- | beside the other sources of one container, `onHoveredXY @"Canvas
-- | hovered" identity` when it has no content of its own.
onHoveredXY :: forall @l r a b v. IsSymbol l => Cons l { x :: Number, y :: Number } b v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
onHoveredXY = pointedAt @l "pointermove"

-- | `onClickedXY`'s sibling for the **context-menu** gesture — a right
-- | click, a long press — the point leaving as case `l` and the browser's
-- | own menu kept away, so the app's popup takes its place.
onContextMenuXY :: forall @l r a b v. IsSymbol l => Cons l { x :: Number, y :: Number } b v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
onContextMenuXY = pointedAt @l "contextmenu"

pointedAt :: forall @l r a b v. IsSymbol l => Cons l { x :: Number, y :: Number } b v => String -> PUI Web { | r } a -> PUI Web { | r } [ | v ]
pointedAt eventType content = fromHandles do
  w' <- handles content
  node <- gets _.parent
  pure
    { toUser: w'.toUser
    , fromUser: \prop -> do
        w'.fromUser \_ -> pure unit
        onPointerXY eventType node \x y -> prop (inj (Proxy @l) { x, y })
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
foreachWith :: forall a b. (a -> PUI Web {} b) -> PUI Web (Array a) b
foreachWith build = fromHandles do
  parent <- gets _.parent
  outer <- gets _.scope
  propRef <- liftEffect $ Ref.new Nothing
  -- each feed's build has a scope of its own, cancelled by the next feed
  -- with the elements it rebuilt
  builtRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \items -> do
        Ref.read builtRef >>= traverse_ cancelScope
        child <- newScope (Just outer) Active
        Ref.write (Just child) builtRef
        removeAllChildren parent
        for_ items \item -> do
          w' <- runDomInScope child parent (handles (build item))
          mProp <- Ref.read propRef
          for_ mProp \prop -> w'.fromUser prop
          void $ w'.toUser {}
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | Draw a UI component from a function for a single value and
-- | redraw when the value changes — the scene whose whole composition
-- | depends on the data. `div $ dynamic renderSwatch`. Owns the element it
-- | sits in.
dynamic :: forall r. ({ | r } -> PUI Web {} {}) -> PUI Web { | r } { | r }
dynamic build = fromHandles do
  w <- handles (foreachWith build)
  liftEffect $ w.fromUser \_ -> pure unit
  liftEffect $ recordToRecordWiring { show: \value -> w.toUser [ value ], held: pure false, reports: \_ -> pure unit, read: pure }

-- | Lay out a list that is known up front and never changes — the courses
-- | on a menu, the keys of a keypad, a row of preset swatches:
-- | `ul $ each courses renderCourse`, `tr $ each keys keyComponent`. Nothing
-- | about it comes from the model, so it sits among fixed decoration and,
-- | like every static, is built once, at registration: it is on screen
-- | before any feed and whether or not one arrives, and like every static
-- | it is the wire at its row: the elements are fed nothing, and the row
-- | passes.
each :: forall a r. Array a -> (a -> PUI Web {} {}) -> PUI Web { | r } { | r }
each items0 build = let items = if isHole items0 then [] else items0 in built do
  w <- handles (foreachWith build)
  liftEffect do
    w.fromUser \_ -> pure unit
    w.toUser items

-- A static is the wire with something built at registration: what `built`
-- builds is on screen before any feed, and every feed passes through it.
built :: forall a r. Web a -> PUI Web { | r } { | r }
built build = fromHandles (build *> handles (identity :: PUI Web { | r } { | r }))

-- Sources

-- | Where the device is: `located` with its coordinates once the platform
-- | answers, `unknown` until then, `denied` when the user refuses or the
-- | platform has no geolocation. Follows the device thereafter.
geolocated
  :: forall @l r r1
   . IsSymbol l
  => Cons l [ located :: { lat :: Number, lng :: Number }, unknown :: {}, denied :: {} ] () r1
  => PUI Web { | r } { | r1 }
geolocated = source @l (pure (inj (Proxy @"unknown") {})) \tell ->
  watchPosition (tell <<< inj (Proxy @"located")) (tell (inj (Proxy @"denied") {}))

-- | Whether the browser is online, as it reports it, followed as it changes.
online
  :: forall @l r r1
   . IsSymbol l
  => Cons l [ online :: {}, offline :: {} ] () r1
  => PUI Web { | r } { | r1 }
online = source @l (onlineCase <$> isOnline) \tell -> watchOnline (tell <<< onlineCase)
  where
  onlineCase on = if on then inj (Proxy @"online") {} else inj (Proxy @"offline") {}

-- | The wall clock as a fact — local hours, minutes and seconds, read every
-- | `period` — the `×→×` sibling of `ticks`, which is the clock as an
-- | event. Known at registration; a hole period never ticks (L18).
clock
  :: forall @l r r1
   . IsSymbol l
  => Cons l { hours :: Int, minutes :: Int, seconds :: Int } () r1
  => { ms :: Number }
  -> PUI Web { | r } { | r1 }
clock period = source @l localTime \tell ->
  if isHole period then pure (pure unit) else everyMs period.ms (localTime >>= tell)

-- | The colour scheme the user prefers, `light` or `dark`, followed as the
-- | system setting changes.
colorScheme
  :: forall @l r r1
   . IsSymbol l
  => Cons l [ light :: {}, dark :: {} ] () r1
  => PUI Web { | r } { | r1 }
colorScheme = source @l (schemeCase <$> prefersDark) \tell -> watchColorScheme (tell <<< schemeCase)
  where
  schemeCase dark = if dark then inj (Proxy @"dark") {} else inj (Proxy @"light") {}

-- Entry point

-- | Any element by name — for a tag this vocabulary has no name for, and
-- | for a tag computed at runtime (`el ("h" <> show level)`). The named
-- | oculars above are all `el` at a fixed tag.
el :: String -> Ocular (PUI Web)
el tagName = fromHandles <<< element tagName <<< handles
