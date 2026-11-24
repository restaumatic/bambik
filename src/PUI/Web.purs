-- | The browser carrier, and the root of everything web-specific: the `Web`
-- | monad (`StateT DOM Effect`) the algebra is instantiated at for the
-- | browser, the DOM building blocks and FFI, and the **element-neutral
-- | vocabulary** every vocabulary beneath this module is written with.
-- |
-- | No *element* lives here — elements are the submodules' to name
-- | (`PUI.Web.HTML`, `PUI.Web.SVG`, and one module per design system). What
-- | lives here works on whatever element was built last, or on no element
-- | at all. Two floors share the module (guardrails L16): the **vocabulary**
-- | an application imports —
-- |
-- | - **decorators** — `attr`/`:=` and `attrDyn`/`:=>`, `cl`, and the
-- |   channel-fed `attrWith`, `attrAt` and `clWhen`; `init` for per-element
-- |   setup;
-- | - **text leaves** — `text`, `textAt`, `staticText`;
-- | - **occurrence sources** — `onClickedXY @l`, `onHoveredXY @l`,
-- |   `onContextMenuXY @l`;
-- | - **panes** (re-exported from `PUI`) — `provided`, `inCase`,
-- |   `providedAt`, `inCaseAt`, and `awaited`, content absent until its
-- |   first feed;
-- | - **facts of the host beside the model**, never in it — each a line
-- |   that reads the fact under field `l` of its row and follows it
-- |   (`PUI.reading` at one fact): `geolocated`, `online`, `colorScheme`,
-- |   `reducedMotion`, `pointer`, `viewport`, `language`, `timeZone`,
-- |   `timestamped` (the wall clock) and `timed` (the time run since the
-- |   page opened, to measure with); and the event that carries one as
-- |   it leaves (`PUI.stamping`): `geolocating`, `timestamping`,
-- |   `timing`, and the edit, `timingEdits`;
-- | - **oculars that watch the DOM** — `separating`, a rule shown only
-- |   between things, and `revealing`, a scroller that shows what it was
-- |   last given;
-- | - **structure from data** — `dynamic`, `each`, and `el` for a computed
-- |   tag —
-- |
-- | and the **internals** only the vocabularies reach: the DOM builder and
-- | FFI (`element`, `Node`, `runDomInNode`, …), `staticHTML` (kept off the
-- | public vocabulary, L10), `statusText`, and the leaf building blocks
-- | over `PUI`'s host-neutral leaf wirings — `fieldLeaf`, `chrome`,
-- | `listenNode`/`listenNodeDeferred`/`listenCapture`, `ensureStyle`,
-- | `autoDismiss`, the selector-group helpers (`grouping`, `member`,
-- | `Member`, `unpickAll`, `hasClass`, `clearedByRepress`) and the
-- | presentation-option tags `OptCaption`, `OptLabelIcon`, `OptSelected`.
-- |
-- | Two ways to draw a screen from data: when the **shape is fixed and only
-- | the values move** (a grid, a canvas, a table), build it once and let
-- | data flow through it (`actedOf` for the repeated part, `text` for
-- | contents, `attrWith`/`clWhen` for anything computed) — elements are
-- | updated in place, so nothing loses focus, scroll position or a
-- | half-finished gesture; when the **shape itself depends on the data** (a
-- | rendered markdown document), `dynamic` and `each` build it from a
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
  , listenNode
  , listenNodeDeferred
  , afterTask
  , hasClass
  , listenCapture
  , Member
  , grouping
  , member
  , unpickAll
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
  , revealing
  , separating
  , geolocated
  , geolocating
  , online
  , reducedMotion
  , pointer
  , viewport
  , language
  , timeZone
  , timestamped
  , timestamping
  , timed
  , timing
  , timingEdits
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
import Data.Variant (inj)
import Type.Proxy (Proxy(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import PUI (awaited, inCase, inCaseAt, provided, providedAt) as Panes
import PUI (class Interacting, Gesture(..), fromHandles, handles, reading, recordToRecordWiring, stamping, stampingEdits, class Hosting, class Scoped, Ocular, PUI, Logged, Scope, ScopeState(..), cancelScope, diagnosticsOn, newScope, onCancel, resumeScope, setDiagnostics, setSink, setTracing, suspendScope)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record

foreign import data Node :: Type

-- Builds Web Document keeping track of parent/last sibling node
newtype Web a = Web (StateT DOM Effect a) -- TODO rename to DocumentBuilder?

type DOM =
  { parent :: Node
  , sibling :: Node -- last sibling
  , scope :: Scope -- the subtree being built (`PUI`'s scope: suspended by a pane, cancelled by a leaver)
  , group :: Maybe (Ref.Ref (Array Member)) -- the selector group being built, its options registering in it
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
foreign import hasClass :: String -> Node -> Effect Boolean
foreign import listenCapture :: Node -> String -> Effect Unit -> Effect Unit
foreign import addEventListener :: String -> Node -> (Event -> Effect Unit) -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import separatorRule :: Node -> Node -> Node -> Effect (Effect Unit)
foreign import revealRule :: Node -> Effect (Effect Unit)
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
foreign import liveRun :: Array Node -> Effect (Array Node)

-- | Pointer-down emitter with coordinates mapped into the element's local
-- | space (an SVG's viewBox units when present, CSS pixels otherwise) —
-- | works for mouse, touch and pen alike.
foreign import onPointerXY :: String -> Node -> (Number -> Number -> Effect Unit) -> Effect Unit
foreign import onDeparture :: Node -> Effect Unit -> Effect Unit
foreign import focusWithin :: Node -> Effect Unit
foreign import positionNow :: forall a. ({ lat :: Number, lng :: Number } -> a) -> a -> a -> Effect a
foreign import positionChanges :: Effect Unit -> Effect (Effect Unit)
foreign import isOnline :: Effect Boolean
foreign import onlineChanges :: Effect Unit -> Effect (Effect Unit)
foreign import mediaMatches :: String -> Effect Boolean
foreign import mediaChanges :: Array String -> Effect Unit -> Effect (Effect Unit)
foreign import viewportNow :: Effect { width :: Int, height :: Int }
foreign import viewportChanges :: Effect Unit -> Effect (Effect Unit)
foreign import languageNow :: Effect String
foreign import languageChanges :: Effect Unit -> Effect (Effect Unit)
foreign import timeZoneNow :: Effect String
foreign import localTime :: Effect { hours :: Int, minutes :: Int, seconds :: Int }
foreign import timeRun :: Effect { ms :: Number }
foreign import everyMs :: Number -> Effect Unit -> Effect (Effect Unit)
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
runDomInScope sc node (Web domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node, scope: sc, group: Nothing }

-- | The DOM carrier hosts collection children under the enclosing parent
-- | (`PUI`'s container action): the freshly appended child is the instance's
-- | node, detach removes it, restack re-appends in key order (`appendChild`
-- | moves an existing node, so identity — focus, local state — travels with
-- | it). Each element is built in a scope of its own under the collection's,
-- | cancelled when the element leaves. An element is a run of siblings, and
-- | the run is read when it is moved or removed, from its first node to its
-- | last: a pane standing among them attaches its content after the build,
-- | between two placeholders of the run, and that content leaves and moves
-- | with the element.
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
      , detach: \nodes -> liveRun nodes >>= traverse_ \node -> removeChild node parent
      , restack: \runs -> for_ runs \nodes -> liveRun nodes >>= traverse_ \node -> appendChild node parent
      }
  attachable = attachableWeb

-- | Fixed decoration given as a raw markup string — for chrome a design
-- | system only documents as markup. Like `staticText` it never changes and
-- | carries no data; unlike it, the string is inserted as markup, so it must
-- | be written in the source and never assembled from model or user text:
-- | a runtime string — a caption, a title, a URL — is a `staticText` or an
-- | attribute (`:=`) on an element built with `el`. Internal chrome
-- | plumbing (guardrails L10): the design-system modules reach it,
-- | application code does not.
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
-- | left out, it defaults to the row label **verbatim**; nothing derives a
-- | caption from an identifier (guardrails L3). The
-- | `ConvertOptionsWithDefaults` tag the design systems' captioned leaves
-- | share.
-- |
-- | The **stamp invariant**, vocabulary-wide: every label-indexed leaf
-- | stamps its label on its host element — `name` where the element is a
-- | form citizen, `aria-label` where it is a display — so inspecting any
-- | element answers which `@l` in the code it is (`text` carries no label
-- | to stamp; under host diagnostics it plants a bare `text` comment
-- | marker). Its second half is the **accessible name**, the caption
-- | verbatim: browsers compute names from rendered text, so every face
-- | whose styling transforms its caption (MD2's stylesheet uppercases
-- | buttons, tabs, segments and the menu anchor) and every face the
-- | platform cannot name by itself (MD3's checkbox and switch, whose
-- | wrapping `<label>` never reaches the input in their shadow root) stamps
-- | `aria-label` with the caption. The smoke harness enforces both halves:
-- | role + accessible-name locators match exactly (scripts/smoke/a11y.mjs),
-- | and axe's name-and-reference rules run per vocabulary
-- | (tests/a11y-laws.mjs).
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

-- | Listen for an event on a node, the remover discarded: a leaf's node is
-- | detached whole with its listeners, never unwired.
listenNode :: Node -> String -> Effect Unit -> Effect Unit
listenNode node event handler = void $ addEventListener event node (const handler)

-- | One option of a selector group, as it registers with the group: its
-- | element, whether it shows itself picked, and how it stops.
type Member = { node :: Node, picked :: Effect Boolean, unpick :: Effect Unit }

-- | Build a selector group: every option built inside registers with it
-- | (`member`), so the group knows its options by composition rather than
-- | by finding them in the DOM. Returns the result and the members.
grouping :: forall a. Web a -> Web { result :: a, members :: Effect (Array Member) }
grouping w = do
  outer <- gets _.group
  ref <- liftEffect $ Ref.new []
  modify_ _ { group = Just ref }
  result <- w
  modify_ _ { group = outer }
  pure { result, members: Ref.read ref }

-- | Register an option with the group being built around it, handing back
-- | the group's members (none outside a group).
member :: Member -> Web (Effect (Array Member))
member m = gets _.group >>= case _ of
  Nothing -> pure (pure [])
  Just ref -> liftEffect (Ref.modify_ (_ <> [ m ]) ref) $> Ref.read ref

-- | Show every member unpicked.
unpickAll :: Effect (Array Member) -> Effect Unit
unpickAll members = members >>= traverse_ _.unpick

-- The clearing gesture of an optional group: pressing (pointer or key) the
-- option already picked and letting its click land clears the group and
-- emits case `n`. `beforeClick` registers, on the group's container, the
-- snapshot of which option was picked before a click: on the press for
-- radios (whose arrow keys synthesize clicks), on the click's capture
-- phase for segments. So a click the platform synthesizes for an
-- arrow-key move never clears; the clear runs after the element's own
-- click handling, which re-checks a radio.
clearedByRepress :: forall @n b v. IsSymbol n => Cons n {} b v => (Node -> Effect Unit -> Effect Unit) -> PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
clearedByRepress beforeClick options = fromHandles do
  container <- gets _.parent
  members <- gets _.group <#> maybe (pure []) Ref.read
  inner <- handles options
  propRef <- liftEffect $ Ref.new Nothing
  liftEffect $ members >>= traverse_ \m -> do
    pressedRef <- Ref.new false
    beforeClick container (m.picked >>= flip Ref.write pressedRef)
    listenNode m.node "click" do
      pressed <- Ref.read pressedRef
      Ref.write false pressedRef
      when pressed $ afterTask do
        unpickAll members
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

-- | A scroller that **shows what it was last given**: the element built
-- | last scrolls, itself and nothing around it, just far enough to bring
-- | into view the element that last entered it or the one whose text last
-- | changed, with as much around it as fits (a row for a cell) — `( orderRow # gatheredOf @"dish" tableLines ) # scrolling #
-- | revealing` (diner: a dish added from the menu is seen on the order,
-- | wherever the order was scrolled to). Decided on the DOM, not the data,
-- | like `separating`: it is an ocular, reads no row and touches no
-- | channel. A text written again as it was moves nothing, and so does
-- | whatever is already in view.
revealing :: forall a b. PUI Web a b -> PUI Web a b
revealing w = fromHandles do
  sc <- gets _.scope
  w' <- handles w
  node <- gets _.sibling
  disconnect <- liftEffect $ revealRule node
  liftEffect $ onCancel sc disconnect
  pure w'

-- | Show a string that changes — a readout, a total, a sentence, a name in
-- | a list row (`text countLine`, counter); wording that doesn't change is
-- | `staticText`. **Copy is a function, not a field** (guardrails L17): the
-- | argument is a named read function of the view model module, typed at
-- | the row its line is fed, so `text` takes no label — its content *is*
-- | the copy, and a caption is surrounding chrome. A field shown as stored
-- | is `textAt @"title"`. Like every display it changes no data: the row it
-- | shows is the row it releases, so it is a line of a `Semigroupoid.do`
-- | block in its place on the screen.
text :: forall r. ({ | r } -> String) -> PUI Web { | r } { | r }
text = textLeaf

-- | `text` of one field shown as it is stored: the field is named by its
-- | label, never picked by a function (`textAt @"title"`). A line that
-- | formats, joins or derives is a copy function, `text`.
textAt :: forall @l b r. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textAt = text (Record.get (Proxy @l))

-- | `text`'s status sibling, vocabulary-internal: a status face renders its
-- | caption — the case label verbatim, reading nothing of the payload — on
-- | every occurrence, adopted as its case by `atCase @l`; nothing shows
-- | before the first occurrence. Unlike the display it renders with, it
-- | **releases nothing**: the `+→×` merge forwards every release as the
-- | whole shared row, so the leaf's echo is swallowed here and the output
-- | is typed at every row, as the silent component's is.
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
  liftEffect $ recordToRecordWiring { show: \s -> setTextNodeValue node (f s), held: pure false, reports: \_ -> pure unit, read: pure }

-- | **Static copy**: text that is part of the structure — on screen before,
-- | and regardless of, any model value — a value, never a label, since it
-- | names no field and no case: `h3 $ staticText "Hours"`. Rendered at
-- | build, the wire at any row; a static inside a pane is still a static
-- | (`staticText "Error" # inCaseAt @"faulty" @"condition"`, calculator).
-- | Which copy is static and which a constant in a copy function is
-- | writing.md's *Fixed copy is static or constant*.

-- | Fixed text given as a runtime string — for vocabulary code captioning
-- | from its configuration, and for closure-known text that is data (a
-- | parsed markdown run).
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
-- | being shown — the strike-through on a done todo (`clWhen isCompleted
-- | "todo-done"`, todo-list). Styling only: to make something *appear and
-- | disappear*, use a pane. Applies to the last element built, not to a
-- | group of siblings.
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
-- | `actedOf` for a collection whose elements are never torn down.
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
onClickedXY :: forall @l r a v. IsSymbol l => Cons l { x :: Number, y :: Number } () v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
onClickedXY = pointedAt @l "pointerdown"

-- | `onClickedXY`'s sibling for the pointer **moving** over the container:
-- | every move leaves as case `l` with the point under the pointer, and so
-- | does the point where it leaves — the hover gesture, what lies under the
-- | pointer *is* the interaction (circle-drawer's nearest circle). Merged
-- | beside the other sources of one container, `onHoveredXY @"Canvas
-- | hovered" identity` when it has no content of its own.
onHoveredXY :: forall @l r a v. IsSymbol l => Cons l { x :: Number, y :: Number } () v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
onHoveredXY = pointedAt @l "pointermove"

-- | `onClickedXY`'s sibling for the **context-menu** gesture — a right
-- | click, a long press — the point leaving as case `l` and the browser's
-- | own menu kept away, so the app's popup takes its place.
onContextMenuXY :: forall @l r a v. IsSymbol l => Cons l { x :: Number, y :: Number } () v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
onContextMenuXY = pointedAt @l "contextmenu"

pointedAt :: forall @l r a v. IsSymbol l => Cons l { x :: Number, y :: Number } () v => String -> PUI Web { | r } a -> PUI Web { | r } [ | v ]
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
-- | move, `actedOf` with `text` and `attrWith` updates the same elements in
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
-- | on a menu, a row of preset swatches: `ul $ each courses renderCourse`.
-- | Like every static it is built once, at registration, on screen before
-- | any feed, and is the wire at its row: the elements are fed nothing, and
-- | the row passes.
each :: forall a r. Array a -> (a -> PUI Web {} {}) -> PUI Web { | r } { | r }
each items build = built do
  w <- handles (foreachWith build)
  liftEffect do
    w.fromUser \_ -> pure unit
    w.toUser items

-- A static is the wire with something built at registration: what `built`
-- builds is on screen before any feed, and every feed passes through it.
built :: forall a r. Web a -> PUI Web { | r } { | r }
built build = fromHandles (build *> handles (identity :: PUI Web { | r } { | r }))

-- Facts beside the model

-- | A line that **reads where the device is** under field `l`:
-- | `located` with its coordinates once the platform answers, `unknown`
-- | until then, `denied` when the user refuses or the platform has no
-- | geolocation — `( headlineSmall $ text whereLine ) # geolocated
-- | @"position"` (here-and-now). The line follows the device; the model
-- | holds no position. `reading`'s laws, all four. One platform watch is
-- | shared by every reader and runs while any is registered.
geolocated
  :: forall @l r t
   . IsSymbol l
  => Cons l [ located :: { lat :: Number, lng :: Number }, unknown :: {}, denied :: {} ] r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
geolocated = reading @l position positionChanges

-- | An emitter whose event **carries where the device was** when it left,
-- | `geolocated`'s cases, under field `l` of its payload — `button @"File
-- | the report" # geolocating @"position"` (incident-report): a place
-- | the model keeps is a fact of the event, never the device's live
-- | position. The platform is watched from registration, so the event
-- | carries the last answer, `unknown` before the first.
geolocating
  :: forall @l c a b v w i
   . IsSymbol l
  => IsSymbol c
  => RowToList v (RL.Cons c { | a } RL.Nil)
  => Cons c { | a } () v
  => Cons l [ located :: { lat :: Number, lng :: Number }, unknown :: {}, denied :: {} ] a b
  => Cons c { | b } () w
  => RowToList w (RL.Cons c { | b } RL.Nil)
  => PUI Web i [ | v ]
  -> PUI Web i [ | w ]
geolocating = stamping @l @c position positionChanges

position :: Effect [ located :: { lat :: Number, lng :: Number }, unknown :: {}, denied :: {} ]
position = positionNow (inj (Proxy @"located")) (inj (Proxy @"unknown") {}) (inj (Proxy @"denied") {})

-- | A line that **reads whether the browser is online**, as it reports
-- | it, under field `l` — `( p $ text connectionLine ) # online
-- | @"connection"` (here-and-now). `reading`'s laws.
online
  :: forall @l r t
   . IsSymbol l
  => Cons l [ online :: {}, offline :: {} ] r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
online = reading @l (onlineCase <$> isOnline) onlineChanges
  where
  onlineCase on = if on then inj (Proxy @"online") {} else inj (Proxy @"offline") {}

-- | A line that **reads the colour scheme the user prefers**, `light` or
-- | `dark`, under field `l`, followed as the system setting changes —
-- | `( p $ text schemeLine ) # colorScheme @"scheme"` (here-and-now). For
-- | styling alone the stylesheet's `prefers-color-scheme` needs no word;
-- | this is for a line whose words or data differ. `reading`'s laws.
colorScheme
  :: forall @l r t
   . IsSymbol l
  => Cons l [ light :: {}, dark :: {} ] r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
colorScheme = reading @l (schemeCase <$> mediaMatches dark) (mediaChanges [ dark ])
  where
  dark = "(prefers-color-scheme: dark)"
  schemeCase d = if d then inj (Proxy @"dark") {} else inj (Proxy @"light") {}

-- | A line that **reads whether the user asks for less motion**, the
-- | platform's `prefers-reduced-motion` — `reduce` or `noPreference` —
-- | under field `l` (`( p $ text motionLine ) # reducedMotion @"motion"`,
-- | here-and-now). `reading`'s laws.
reducedMotion
  :: forall @l r t
   . IsSymbol l
  => Cons l [ reduce :: {}, noPreference :: {} ] r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
reducedMotion = reading @l (motionCase <$> mediaMatches reduce) (mediaChanges [ reduce ])
  where
  reduce = "(prefers-reduced-motion: reduce)"
  motionCase r = if r then inj (Proxy @"reduce") {} else inj (Proxy @"noPreference") {}

-- | A line that **reads what the user points with**, the platform's
-- | primary `pointer` — `coarse` (a finger), `fine` (a mouse, a pen) or
-- | `none` — under field `l` (`( p $ text pointerLine ) # pointer
-- | @"pointer"`, here-and-now). `reading`'s laws.
pointer
  :: forall @l r t
   . IsSymbol l
  => Cons l [ coarse :: {}, fine :: {}, none :: {} ] r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
pointer = reading @l pointerCase (mediaChanges [ coarse, fine ])
  where
  coarse = "(pointer: coarse)"
  fine = "(pointer: fine)"
  pointerCase = do
    isCoarse <- mediaMatches coarse
    isFine <- mediaMatches fine
    pure
      if isCoarse then inj (Proxy @"coarse") {}
      else if isFine then inj (Proxy @"fine") {}
      else inj (Proxy @"none") {}

-- | A line that **reads the size of the window**, its layout viewport in
-- | CSS pixels, under field `l`, followed as it is resized — `( p $ text
-- | windowLine ) # viewport @"window"` (here-and-now). For layout alone a
-- | media query needs no word; this is for a line whose words or data
-- | differ by size. `reading`'s laws.
viewport
  :: forall @l r t
   . IsSymbol l
  => Cons l { width :: Int, height :: Int } r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
viewport = reading @l viewportNow viewportChanges

-- | A line that **reads the user's preferred language**, as a BCP 47 tag
-- | (`"pl-PL"`), under field `l`, followed as the browser's setting
-- | changes — `( p $ text languageLine ) # language @"language"`
-- | (here-and-now). `reading`'s laws.
language
  :: forall @l r t
   . IsSymbol l
  => Cons l String r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
language = reading @l languageNow languageChanges

-- | A line that **reads the time zone** the platform keeps, as an IANA name
-- | (`"Europe/Warsaw"`), under field `l` — `( p $ text whenLine ) #
-- | timeZone @"zone" # timestamped @"time" clockPeriod` (here-and-now).
-- | The platform reports no change, so the zone is read at each feed;
-- | under `timestamped`, as here, every beat is one. `reading`'s laws.
timeZone
  :: forall @l r t
   . IsSymbol l
  => Cons l String r t
  => PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
timeZone = reading @l timeZoneNow unwatched

-- | A line that **reads the time**: the stage is fed its row with the wall
-- | clock under field `l` — `( labelLarge $ text waitLine ) # timestamped
-- | @"time" clockPeriod`, `waitLine :: { placed :: Time, time :: { hours ::
-- | Int, minutes :: Int, seconds :: Int } | r } -> String` — and fed it
-- | again every `period`, so the time is a fact of the platform the line
-- | reads where it is drawn and no field of the model. `reading`'s laws,
-- | the change being each `period`. The time has no source of its own: a
-- | source would make it a field of the model, and each beat a turn of
-- | the knot. Beats fall on whole periods of the wall clock, one timer for
-- | every use of a period.
timestamped
  :: forall @l r t
   . IsSymbol l
  => Cons l { hours :: Int, minutes :: Int, seconds :: Int } r t
  => { ms :: Number }
  -> PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
timestamped period = reading @l localTime (everyMs period.ms)

-- | A line that **reads the time run**: `timestamped` with the stopwatch's
-- | clock for the wall's — field `l` is the milliseconds run since the
-- | page opened, to the fraction, on a clock that only runs forward
-- | (`( displaySmall $ text elapsedLine ) # timed @"time" tickPeriod`,
-- | stopwatch). It is the time to measure with: two readings subtract to
-- | what passed between them, whatever the wall clock was set to
-- | meanwhile, and a moment of `{ ms: 0.0 }` in a seed is the moment the
-- | page opened, so an application that runs from its start seeds one and
-- | asks nothing. `reading`'s laws.
timed
  :: forall @l r t
   . IsSymbol l
  => Cons l { ms :: Number } r t
  => { ms :: Number }
  -> PUI Web { | t } { | t }
  -> PUI Web { | r } { | r }
timed period = reading @l timeRun (everyMs period.ms)

-- | An emitter whose event **carries the time it left at**: the wall
-- | clock is read as the event leaves and joins its payload under field
-- | `l` — `button @"Punch in" # timestamping @"time"`, folded by `foldInto
-- | @"Punch in" punchIn`, `punchIn :: { event :: { time :: … | r }, model
-- | :: Shift } -> Shift`. The fold stays a function of the event and the
-- | model; no timer runs. Takes an emitter closed to one case, whose
-- | payload is a record without field `l`.
timestamping
  :: forall @l c a b v w i
   . IsSymbol l
  => IsSymbol c
  => RowToList v (RL.Cons c { | a } RL.Nil)
  => Cons c { | a } () v
  => Cons l { hours :: Int, minutes :: Int, seconds :: Int } a b
  => Cons c { | b } () w
  => RowToList w (RL.Cons c { | b } RL.Nil)
  => PUI Web i [ | v ]
  -> PUI Web i [ | w ]
timestamping = stamping @l @c localTime unwatched

-- | An emitter whose event **carries the time run** when it left:
-- | `timestamping` with `timed`'s clock — `button' @"Lap" { icon: "flag" }
-- | # timing @"time"`, folded by `foldInto @"Lap" recordLap` (stopwatch).
timing
  :: forall @l c a b v w i
   . IsSymbol l
  => IsSymbol c
  => RowToList v (RL.Cons c { | a } RL.Nil)
  => Cons c { | a } () v
  => Cons l { ms :: Number } a b
  => Cons c { | b } () w
  => RowToList w (RL.Cons c { | b } RL.Nil)
  => PUI Web i [ | v ]
  -> PUI Web i [ | w ]
timing = stamping @l @c timeRun unwatched

-- | An editor whose **edit carries the time run** when it left: field
-- | `l` of the row an edit releases is the time of that edit —
-- | `sliderLive @"Duration" # timingEdits @"changed"` (timer), `changed
-- | :: { ms :: Number }` a field of the model: the moment of its last
-- | edit, which is a fact the model keeps, where the current time is
-- | none. What follows from an edit made then is the line's `# settled`,
-- | a function of the row. Laws: **a feed is answered as fed** — the
-- | editor's answer to a feed leaves with the `l` it was fed, so a turn of
-- | the knot stamps nothing; **an edit is stamped** — a release outside a
-- | feed leaves with `l` read as it leaves. The editor answers within its
-- | feed, as every leaf does (`unanswering`'s precondition).
timingEdits
  :: forall @l a r
   . IsSymbol l
  => Cons l { ms :: Number } a r
  => PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
timingEdits = stampingEdits @l timeRun unwatched

unwatched :: Effect Unit -> Effect (Effect Unit)
unwatched _ = pure (pure unit)

-- Entry point

-- | Any element by name — for a tag this vocabulary has no name for, and
-- | for a tag computed at runtime (`el ("h" <> show level)`). The named
-- | oculars above are all `el` at a fixed tag.
el :: String -> Ocular (PUI Web)
el tagName = fromHandles <<< element tagName <<< handles
