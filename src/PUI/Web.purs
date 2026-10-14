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
-- |   field shown as stored), `textOf` (a status's payload), `staticText`,
-- |   and `staticHTML`, kept off the public vocabularies (L10);
-- | - **occurrence sources** — `clicked @l @k`, `onClickedXY @l`;
-- | - **panes** — `provided` (emitters) and `inCase` (displays and
-- |   editors), each fed the whole row; a pane over a variant the model
-- |   stores names its field by label (`providedAt`, `inCaseAt`);
-- | - **sources** — `source` and the facts over it (`geolocated`, `online`,
-- |   `clock`, `colorScheme`), each owning one field the app cannot seed;
-- | - **structure from data** — `dynamic`, `each`, and `el` for a computed
-- |   tag;
-- | - **the leaf protocol, as functions** — what every vocabulary's leaves
-- |   are built from: `editorWiring` (write unless held, echo every feed,
-- |   report the platform's change through a busy guard),
-- |   `selectorWiring` (the same over an option list, in index terms),
-- |   `displayWiring` (render, release the row shown), `statusWiring` (render,
-- |   release nothing), `eventLeaf` (the click emitter over chrome),
-- |   `escaped` (a runtime
-- |   string made safe for `staticHTML`), `listenNode`/`listenNodeDeferred`,
-- |   `ensureStyle`, `autoDismiss` and the option tags `OptCaption`,
-- |   `OptLabelIcon`, `OptSelected`, `OptIcon`. A leaf that
-- |   writes its own `toUser`/`fromUser` pair instead of one of the four
-- |   wirings is a leaf that can forget the echo a gate depends on.
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
  , OptIcon(..)
  , Wiring
  , editorWiring
  , selectorWiring
  , displayWiring
  , statusWiring
  , eventLeaf
  , escaped
  , listenNode
  , listenNodeDeferred
  , ensureStyle
  , autoDismiss
  , choice
  , andChoice
  , (<+>)
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
  , hole
  , separating
  , source
  , geolocated
  , online
  , clock
  , colorScheme
  , onFilePicked
  , inCase
  , inCaseAt
  , text
  , textAt
  , textOf
  , staticText
  , attr
  , (:=)
  , cl
  , init
  , attrDyn
  , (:=>)
  , provided
  , providedAt
  , clWhen
  , attrWith
  , attrAt
  , clicked
  , clickedRow
  , onClickedXY
  , dynamic
  , each
  , el
  )
  where

import Data.Profunctor.Row.Structural (withStructuralEq)
import Data.Profunctor (rmap)
import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import ConvertableOptions (class ConvertOption)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Array (concat, findIndex, (!!))
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Data.Variant (Variant, case_, expand, inj, on, prj)
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
import PUI (class Hosting, class Scoped, Ocular, PUI, Logged, Scope, ScopeState(..), cancelScope, diagnosticsOn, joined, newScope, onCancel, resumeScope, scopeActive, setDiagnostics, setSink, setTracing, suspendScope)
import Data.Profunctor.Row (atAnyRow)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Prim.Row (class Cons)
import Data.Profunctor.Seeding (isHole)
import Record as Record

-- | A value of any type standing for logic not written yet (guardrails L18):
-- | any read of it, or call, throws and marks the page (`__bambikHoleReached`),
-- | so a view run with its logic stubbed to holes that reaches none proves no
-- | data reached the logic. The one exception is the field `__bambikHole`,
-- | which answers `true` — what `Data.Profunctor.Seeding.isHole` reads, so a
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

uniqueId :: Effect String
uniqueId = randomElementId

-- others

attachable :: forall r. Web r -> Web { result :: r, ensureAttached :: Effect Unit, ensureDetached :: Effect Unit }
attachable dom = do
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
foreign import separatorRule :: Node -> Node -> Node -> Effect (Effect Unit)
foreign import listenNodeDeferred :: Node -> String -> Effect Unit -> Effect Unit
foreign import ensureStyle :: String -> String -> Effect Unit
foreign import autoDismiss :: Node -> String -> Int -> Effect Unit
foreign import setAttribute :: Node -> String -> String -> Effect Unit
foreign import removeAttribute :: Node -> String -> Effect Unit
foreign import removeAllChildren :: Node -> Effect Unit
foreign import removeChild :: Node -> Node -> Effect Unit
foreign import addClass :: Node -> String -> Effect Unit
foreign import removeClass :: Node -> String -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
foreign import randomElementId :: Effect String
foreign import lastChild :: Node -> Effect Node
foreign import childCount :: Node -> Effect Int
foreign import childrenFrom :: Int -> Node -> Effect (Array Node)

-- | Pointer-down emitter with coordinates mapped into the element's local
-- | space (an SVG's viewBox units when present, CSS pixels otherwise) —
-- | works for mouse, touch and pen alike.
foreign import onClickXY :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit
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
          inst <- runDomInScope child parent (unwrap w)
          nodes <- childrenFrom before parent
          pure { feed: inst.toUser, subscribe: inst.fromUser, node: nodes, cancel: cancelScope child }
      , detach: \nodes -> for_ nodes \node -> removeChild node parent
      , restack: \runs -> for_ (concat runs) \node -> appendChild node parent
      }

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
staticHTMLExact html = wrap do
  parent <- gets _.parent
  newNode <- liftEffect $ appendRawHtml html parent
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
    }

staticHTML :: forall r. String -> PUI Web { | r } { | r }
staticHTML = built <<< unwrap <<< staticHTMLExact

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

-- | Marks the `icon` of a tab as optional — tabs may be labelled, iconed or
-- | both.
data OptIcon = OptIcon

instance ConvertOption OptIcon "icon" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptIcon sym a a where
  convertOption _ _ = identity

-- The leaf protocol, as functions (see the module header): every leaf of
-- every vocabulary is one of these four wirings over its own element, so
-- the echo a gate depends on, the busy guard around a programmatic write
-- and the one-registration listener are written once here and nowhere
-- else.

-- | What a `PUI Web a b` carries once built: the feed leg and the
-- | registration leg (`PUI`'s wiring record, named for the vocabularies).
type Wiring a b = { toUser :: a -> Effect Unit, fromUser :: (b -> Effect Unit) -> Effect Unit }

-- | An **editor leaf**'s wiring: `show` writes each fed value to the
-- | element unless `held` says the user is in it (the focus guard — a model
-- | update never clobbers a field being typed in); every feed is **echoed**
-- | whether written or not, so the lifted stage releases the row and an
-- | enclosing gate opens; `reports` registers the platform event that
-- | means the user changed the value, and `read` reads that value off the
-- | element given the row last fed (a slider reads its position into the
-- | quantity it was shown). A report while a write is in progress is the
-- | write's own echo and is dropped, and a report before any feed has
-- | nothing to complete and is dropped too.
editorWiring
  :: forall a
   . { show :: a -> Effect Unit
     , held :: Effect Boolean
     , reports :: Effect Unit -> Effect Unit
     , read :: a -> Effect a
     }
  -> Effect (Wiring a a)
editorWiring leaf = do
  propRef <- Ref.new Nothing
  lastRef <- Ref.new Nothing
  busyRef <- Ref.new false
  pure
    { toUser: \a -> do
        Ref.write (Just a) lastRef
        held <- leaf.held
        unless held do
          Ref.write true busyRef
          leaf.show a
          Ref.write false busyRef
        Ref.read propRef >>= traverse_ (_ $ a)
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        leaf.reports do
          busy <- Ref.read busyRef
          unless busy do
            last <- Ref.read lastRef
            for_ last \a -> leaf.read a >>= prop
    }

-- | A **selector leaf**'s wiring over its option list, in index terms:
-- | `show` checks the option at the index (none for `Nothing`), `reports`
-- | registers the platform's pick event, `read` reads the picked index
-- | (`Nothing` for a cleared control). The option's value travels through
-- | `selectedAt` and its siblings, which lift this `Maybe a` leaf to the
-- | field editor and answer the feed themselves — so, unlike
-- | `editorWiring`, nothing is echoed here. A pick reported while a
-- | programmatic selection is in progress is that selection's own event
-- | and is dropped.
selectorWiring
  :: forall a
   . Eq a
  => Array { value :: a, label :: String }
  -> { show :: Maybe Int -> Effect Unit
     , reports :: Effect Unit -> Effect Unit
     , read :: Effect (Maybe Int)
     }
  -> Effect (Wiring (Maybe a) (Maybe a))
selectorWiring options leaf = do
  propRef <- Ref.new Nothing
  busyRef <- Ref.new false
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        leaf.show (ma >>= \a -> findIndex (\o -> o.value == a) options)
        Ref.write false busyRef
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        leaf.reports do
          busy <- Ref.read busyRef
          unless busy do
            mi <- leaf.read
            Ref.read propRef >>= traverse_ (_ $ (mi >>= (options !! _) <#> _.value))
    }

-- | A **display leaf**'s wiring: `show` renders each fed value, and the
-- | feed is answered with the value itself — the display's echo, so a
-- | display is a stage of the pipeline that changes no data: the row it
-- | shows is the row the next line is fed. Nothing at registration, since
-- | an answer needs a feed.
displayWiring :: forall a. (a -> Effect Unit) -> Effect (Wiring a a)
displayWiring show = do
  propRef <- Ref.new Nothing
  pure
    { toUser: \a -> do
        show a
        Ref.read propRef >>= traverse_ (_ $ a)
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | A **status leaf**'s wiring: `show` renders each occurrence, and the
-- | status releases nothing — the `+→×` merge forwards every release as
-- | the whole shared row, so a status is typed at every row, as `silence`
-- | is.
statusWiring :: forall a b. (a -> Effect Unit) -> Wiring a b
statusWiring show = { toUser: show, fromUser: \_ -> pure unit }

-- | The click-emitter protocol over any `{}`-output element chrome: replay
-- | the row last fed on click, as case `l` (a click before any value
-- | arrived is withheld) — `clickedRow` over the chrome placed at the row,
-- | the last-built element listening. Every vocabulary's `button` family
-- | is this over its own chrome.
eventLeaf :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web {} {} -> PUI Web { | r } [ | v ]
eventLeaf chrome = clickedRow @l (atAnyRow chrome)

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

-- | One selector **option**, named by its case: `choice @"Boardroom"` both
-- | injects that case and draws its label, so a choice states its copy once
-- | — exactly as a captioned leaf does:
-- |
-- | ```
-- | dropdown @"Room" (choice @"Focus pod (4 seats)" <+> choice @"Boardroom (12 seats)")
-- | ```
-- |
-- | A choice is a one-option array over the **closed** singleton row of its
-- | case, and `<+>` joins option lists uniting their rows, so the options
-- | stay a plain array in the order they are written — a selector's option
-- | order is a design decision (rooms by size, durations by length) and
-- | deliberately does **not** come from the variant row, which the compiler
-- | sorts alphabetically — while the row the selector's field holds is
-- | closed by the list itself, with no case left open for a typed hole to
-- | report.
choice :: forall @l r. IsSymbol l => Row.Cons l {} () r => Array { value :: Variant r, label :: String }
choice = [ { value: inj (Proxy :: Proxy l) {}, label: reflectSymbol (Proxy :: Proxy l) } ]

-- | Join two option lists in writing order, their variant rows united — so
-- | `choice @"one-way" <+> choice @"return"` is the **closed** row
-- | `[ "one-way" :: {}, return :: {} ]` in that order, and a selector over
-- | it leaves no option case open for a typed hole to report (guardrails
-- | L18, the determination half). Option order stays the order written,
-- | never the row's alphabetical one.
andChoice :: forall v1 v2 v. Row.Union v1 v2 v => Row.Union v2 v1 v => Array { value :: Variant v1, label :: String } -> Array { value :: Variant v2, label :: String } -> Array { value :: Variant v, label :: String }
andChoice xs ys = map (\o -> o { value = expand o.value }) xs <> map (\o -> o { value = expand o.value }) ys

infixr 5 andChoice as <+>

-- | Lift a bare selection leaf — the option to check in (`Nothing`: none),
-- | the option the user checked out (`Nothing`: cleared) — to the
-- | **selection editor** of field `l`, a whole-row citizen
-- | `{ l :: a | rest } → { l :: a | rest }` like every other editor: the
-- | widget stores a value, so it has an editor's shape and owes an editor's
-- | answer — every feed is answered with the row, and every pick is stored
-- | into field `l`. Here the field is the option itself, so the model has a
-- | choice at all times. Vocabulary plumbing, beside `focusField @l`: every
-- | plain selector in every vocabulary is its leaf lifted with this.
selectedAt :: forall @l a b r. IsSymbol l => Row.Cons l a b r => PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
selectedAt = selectedWith @l Just identity

-- | `selectedAt` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case the
-- | application names. Every other case shows nothing checked, a pick
-- | stores case `c` and there is no way back, so the stages demanding the
-- | selection adopt the made case. Every `…Unpicked` selector is its leaf
-- | lifted with this.
selectedUnpickedAt :: forall @l @c a b1 v b r. IsSymbol l => IsSymbol c => Row.Cons c a b1 v => Row.Cons l (Variant v) b r => PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
selectedUnpickedAt = selectedWith @l (prj (Proxy @c)) (map (inj (Proxy @c)))

-- | `selectedAt` for a choice the user may leave unmade: field `l` is a
-- | variant whose case `c` is the made choice and whose case `n` is none,
-- | and clearing the widget stores case `n`. Every `…Optional` selector is
-- | its leaf lifted with this.
selectedOptionalAt :: forall @l @c @n a b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Row.Cons c a b1 v => Row.Cons n {} b2 v => Row.Cons l (Variant v) b r => PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
selectedOptionalAt = selectedWith @l (prj (Proxy @c)) (Just <<< maybe (inj (Proxy @n) {}) (inj (Proxy @c)))

-- | The clearing gesture of an optional radio group: the platform's radios
-- | cannot be unchecked, so pressing the checked member again (pointer or
-- | key, on `press`) and letting its `click` land clears the group. The
-- | press snapshots the selection, so a click the platform synthesizes for
-- | an arrow-key move to another member never clears; the clear runs after
-- | the element's own click handling, which re-checks it. Vocabulary plumbing
-- | for the `…Optional` radio leaves.
clearedOnRepress :: forall a m. Ref.Ref (Maybe a) -> Array { press :: Node, click :: Node, value :: a | m } -> Effect Unit -> Effect Unit
clearedOnRepress selRef members clear = withStructuralEq @a (do
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
      when (pressed == Just m.value) (afterTask clear))

-- checkedOf: the option the field shows checked; stored: what a pick (`Just`)
-- or a clear (`Nothing`) stores, if anything; `checkedOf <=< stored` must give
-- back the pick, so a pick's echo is the pick
selectedWith :: forall @l f a b r. IsSymbol l => Row.Cons l f b r => (f -> Maybe a) -> (Maybe a -> Maybe f) -> PUI Web (Maybe a) (Maybe a) -> PUI Web { | r } { | r }
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
separating rule w = wrap do
  parentNode <- gets _.parent
  sc <- gets _.scope
  start <- liftEffect $ createCommentNode "separator"
  liftEffect $ appendChild start parentNode
  _ <- unwrap rule
  ruleEnd <- liftEffect $ lastChild parentNode
  w' <- unwrap w
  end <- liftEffect $ createCommentNode "/separator"
  liftEffect $ appendChild end parentNode
  disconnect <- liftEffect $ separatorRule start ruleEnd end
  liftEffect $ onCancel sc disconnect
  pure w'

-- | The **pane**: a whole-row citizen — a display, an editor, or a pipeline
-- | of them — that *exists* only while the classifier yields case `l`:
-- | attached and fed the whole row on that case, detached on any other,
-- | the fed row released always. Where its content is a display, it shows
-- | the row only in that state (`text ticketLine # inCaseAt @"serving"
-- | @"display"`); where
-- | this pane's content emits the **row**, so the pane is a stage — the
-- | pane's channel and the wire's, side by side over one input and one
-- | output.
-- |
-- | It dissolves the identity fold: a field that exists only in one mode
-- | is *not* a payload to fold back into the row by hand — it is a
-- | whole-row editor whose existence is gated, and
-- | its `focusField @l` lift already re-attaches the rest of the row. The
-- | classifier is a business function typed at the stage's row, exactly as
-- | `pane`'s is; a variant the model stores is named by its label
-- | instead, `inCaseAt` (`# inCaseAt @"Delivery" @"selected"`, order-form). One release per feed either way: attached, the
-- | editor's own echo is the release; detached, the wire speaks for the
-- | absent editor. What the edit does to the rest of the row is a `settled`
-- | normalization on the same stage when it is a state invariant
-- | (meeting-booker's `seatsInRoom`, circle-drawer's `resizeSelected`).
-- | Like the other panes, it takes the classifier's variant as a visible
-- | type argument after the case.
inCase
  :: forall @l @v r a b
   . IsSymbol l => Cons l a b v
  => ({ | r } -> [ | v ]) -> PUI Web { | r } { | r } -> PUI Web { | r } { | r }
inCase f w = wrap do
  { result: editor, ensureAttached, ensureDetached } <- attachable $ unwrap w
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \row -> do
        case prj (Proxy @l) (f row) of
          Nothing -> do
            ensureDetached
            -- the wire speaks only for the absent editor: attached, the
            -- editor's own echo is the release (one feed, one release)
            mProp <- Ref.read propRef
            for_ mProp \prop -> prop row
          -- attach before feeding, as `provided` does
          Just _ -> ensureAttached *> editor.toUser row
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        editor.fromUser prop
    }

-- | `inCase` over a variant the model stores: the editor exists in case `c`
-- | of field `l`, both named by label (`# inCaseAt @"return" @"Flight type"`).
inCaseAt
  :: forall @c @l r a v b1 b2
   . IsSymbol c => IsSymbol l => Cons c a b1 v => Cons l [ | v ] b2 r
  => PUI Web { | r } { | r } -> PUI Web { | r } { | r }
inCaseAt = inCase @c @v (Record.get (Proxy @l))

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

-- | `text`'s status sibling: a status face renders its payload through
-- | its copy function (`textOf copy`), adopted as its case by
-- | `atCase @l` — a `+→×` leaf. Vocabulary-internal — application
-- | code shows copy with `text`, whose row-shaped read states its footprint.
-- | Unlike the display it renders with, a status **releases nothing**: the
-- | `+→×` merge forwards every release as the whole shared row, so the
-- | leaf's echo is swallowed here and the output is typed at every
-- | row, as `silence`'s is.
textOf :: forall a r. (a -> String) -> PUI Web a { | r }
textOf f = wrap do
  leaf <- unwrap (textLeaf f)
  pure { toUser: leaf.toUser, fromUser: \_ -> leaf.fromUser \_ -> pure unit }

textLeaf :: forall a. (a -> String) -> PUI Web a a
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
  -- the display's answer to the feed is the row it was shown, so the
  -- stage after it is fed it (`simpleDialog`'s confirm replays what its
  -- content last answered). Nothing at registration, since an answer
  -- needs a feed. Every display follows this protocol (`displayWiring`).
  -- a hole read renders nothing (guardrails L18): a row may reach a display
  -- before its copy is written, as from a merge of sources
  liftEffect $ displayWiring \s -> setTextNodeValue node (if isHole f then "" else f s)

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
staticTextExact content = wrap do
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
staticText = built <<< unwrap <<< staticTextExact

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

-- | Show the **emitter pane** while the model is in state `l` — visibility
-- | is **case adoption**: the argument is a business function classifying
-- | the situation into a variant, and the pane is attached and fed the row
-- | on case `l`, detached on every other case. A quiz whose run is either `asking` or `finished` shows its
-- | answer list as `listOf … # provided @"asking" quizPhase`; a stopwatch
-- | shows Start only while `halted`.
-- |
-- | The content is an emitter (`×→+`), and that is what makes the pane
-- | lawful: a feed arms an emitter and fires nothing, so a detached one
-- | firing nothing is the same answer — an emitter removed by `provided`
-- | *is* `silence`. For a button that is the document's doing (a parked
-- | node gets no clicks); for a `ticks` or an `action` inside, whose
-- | occurrences have no cause on the channel, it is the pane's **scope**:
-- | the content is built in a scope the pane suspends while detached, so a
-- | tick skips its beat and an outcome that settles meanwhile is dropped
-- | (`PUI`, "Scope"). The pane family is indexed by the shape of what the
-- | pane holds, each member lawful at its shape: `provided` for emitters,
-- | `inCase` for displays and editors (the wire while detached).
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
-- | The pane is fed the whole row, like every stage, and it is removed
-- | from the page while the model sits elsewhere: the rule for *what is on
-- | screen when* lives in business code where it can be tested, and a pane
-- | that is absent contributes nothing — so anything downstream waiting on
-- | it waits, rather than showing a stale or invented value.
-- |
-- | The classifier's variant is a visible type argument after the case, so
-- | the view can declare every state the pane chooses among
-- | (`# provided @"reading" @( reading :: {}, browsing :: {} ) messageView`).
-- | A variant the model stores is named by its label instead, `providedAt`.
provided :: forall @l @v r a b v1. IsSymbol l => Cons l a b v => ({ | r } -> [ | v ]) -> PUI Web { | r } [ | v1 ] -> PUI Web { | r } [ | v1 ]
provided = attachedOn @l

-- | `provided` over a variant the model stores: the emitter exists in case
-- | `c` of field `l`, both named by label (`# providedAt @"halted" @"phase"`).
providedAt :: forall @c @l r a v v1 b1 b2. IsSymbol c => IsSymbol l => Cons c a b1 v => Cons l [ | v ] b2 r => PUI Web { | r } [ | v1 ] -> PUI Web { | r } [ | v1 ]
providedAt = provided @c @v (Record.get (Proxy @l))

-- The pane mechanism every pane shares, at any content output: attach and
-- feed the row on case `l`, detach on every other case. Public only
-- through its lawful restrictions — `provided` at emitter content, where a
-- detached pane's silence is `×→+`'s Answer (`inCase` answers for a
-- detached content itself).
attachedOn :: forall @l @v r a b c. IsSymbol l => Cons l a b v => ({ | r } -> [ | v ]) -> PUI Web { | r } c -> PUI Web { | r } c
attachedOn f w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ unwrap w
  pure
    { toUser: \fed -> case prj (Proxy @l) (f fed) of
      Nothing -> ensureDetached
      Just _ -> do
        -- attach before feeding: a UI component that measures itself on toUser (the
        -- MDC slider positions its thumb from the track width) needs to be in
        -- the document first, or it lays out against a zero-width detached node
        ensureAttached
        toUser fed
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
clWhen :: forall r a. ({ | r } -> Boolean) -> String -> PUI Web { | r } a -> PUI Web { | r } a
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
-- | (`circle >>> attrWith "style" cellFace`).
-- |
-- | This is what keeps a drawing or a large grid from being rebuilt: the
-- | element is created once and restyled in place as values arrive, so
-- | selection, focus and scrolling survive every update. Pair it with
-- | `foreach` for a collection whose elements are never torn down.
attrWith :: forall r a. String -> ({ | r } -> String) -> PUI Web { | r } a -> PUI Web { | r } a
attrWith name valueOf w = wrap do
  w' <- unwrap w
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

-- | Make any element clickable: it reports, as case `l`, field `k` of
-- | whatever it is currently showing. A grid cell, a list row, a chip, a
-- | picture — the content is the display, the click is the report, so the
-- | identity of what was picked comes from what was on screen and cannot be
-- | got wrong: `clicked @"picked" @"key" (td $ textAt @"text")`, the field
-- | named by its label as `listOf @l @k` names its key. A click before the
-- | element has been shown anything does nothing. An **event source**,
-- | `× → +` by shape as by behaviour: the click **replays** the last row
-- | fed — replay is lawful over records only, an entity's value may be
-- | re-said where a one-shot event may not (the `looped`/`observed`/
-- | `simpleDialog` argument) — and leaves as an occurrence of `l`, so
-- | nothing record-shaped ever stands for a click. The content is fed the
-- | row it replays; statics and chrome sit at any row, so
-- | `clicked @l @k staticChrome` needs no adapter.
-- |
-- | The **replay contract**, a law of this word rather than of the shape
-- | (the shape's two are Data.Profunctor.Row's Repetition and Answer, both
-- | held: a feed rewrites the replay slot, and a feed never fires): a click
-- | emits field `k` of the row last fed, as case `l`; before the first feed
-- | a click emits nothing.
clicked :: forall @l @k r a b f v. IsSymbol l => IsSymbol k => Cons k f b r => Cons l f () v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
clicked w = replayed @l (Record.get (Proxy @k)) (occurrences w)

-- | `clicked` replaying the whole row it was last fed, as case `l` — the
-- | click half of every vocabulary's emitter family (`button @l` emits the
-- | row it replays). Vocabulary-internal: application code emits through
-- | the vocabulary's emitters, or names the field a click reports with
-- | `clicked @l @k`.
clickedRow :: forall @l r a v. IsSymbol l => Cons l { | r } () v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
clickedRow w = replayed @l identity (occurrences w)

-- `joined` at the empty payload: the click source's occurrence carries
-- nothing, so the event is dropped, `f` is read off the row last fed and
-- the case is renamed from the source's to `l`. Replay is `Strong`'s
-- retention and nothing else — the primed `Strong` law: an occurrence
-- before any feed is withheld, a feed never emits.
replayed :: forall @l r k v. IsSymbol l => Cons l k () v => ({ | r } -> k) -> PUI Web { | r } [ occurred :: {} ] -> PUI Web { | r } [ | v ]
replayed f src = joined @"occurred" src # rmap (on (Proxy @"occurred") (inj (Proxy @l) <<< f <<< _.model) case_)

-- The click source `clicked` is built from: each click on the last-built
-- element (the content's own node) leaves as an occurrence carrying nothing;
-- the content is fed the row and its output written off. The `× → +`
-- leaf's occurrence half — at the closed empty row, `occurrences chrome ::
-- PUI Web {} [ occurred :: {} ]` is the point's dual, an occurrence out of
-- the terminal record (`ticks` is its timer sibling in `PUI`). Private, with
-- one fixed case: the business label is `clicked @l`'s to state, and
-- `replayed @l` relabels while it attaches the row — replay is `Strong`'s
-- retention, so no source keeps a copy of the row it was shown.
occurrences :: forall a b. PUI Web a b -> PUI Web a [ occurred :: {} ]
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
onClickedXY :: forall @l r a v. IsSymbol l => Cons l { x :: Number, y :: Number } () v => PUI Web { | r } a -> PUI Web { | r } [ | v ]
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
foreachWith :: forall a b. (a -> PUI Web {} b) -> PUI Web (Array a) b
foreachWith build = wrap do
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
          w' <- runDomInScope child parent (unwrap (build item))
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
dynamic build = wrap do
  w <- unwrap (foreachWith build)
  liftEffect $ w.fromUser \_ -> pure unit
  liftEffect $ displayWiring \value -> w.toUser [ value ]

-- | Lay out a list that is known up front and never changes — the courses
-- | on a menu, the keys of a keypad, a row of preset swatches:
-- | `ul $ each courses renderCourse`, `tr $ each keys keyComponent`. Nothing
-- | about it comes from the model, so it sits among fixed decoration and,
-- | like every static, is built once, at registration: it is on screen
-- | before any feed and whether or not one arrives, and like every static
-- | it is the wire at its row: the elements are fed nothing, and the row
-- | passes.
each :: forall @a r. Array a -> (a -> PUI Web {} {}) -> PUI Web { | r } { | r }
each items0 build = let items = if isHole items0 then [] else items0 in built do
  w <- unwrap (foreachWith build)
  liftEffect do
    w.fromUser \_ -> pure unit
    w.toUser items

-- A static is the wire with something built at registration: what `built`
-- builds is on screen before any feed, and every feed passes through it.
built :: forall a r. Web a -> PUI Web { | r } { | r }
built build = wrap (build *> unwrap (identity :: PUI Web { | r } { | r }))

-- Sources

-- | A **source**: a fact the platform knows and the app cannot seed — where
-- | the device is, whether it is online, what time it is, which colour
-- | scheme is preferred. A source owns one field, `l`, reads nothing of the
-- | row it is fed, and is **pointed**: it announces its first value at
-- | registration (`now`), so a merge of sources is a row the gate releases
-- | at once and keeps current. Every feed is answered with the last value
-- | known (Answer at `×→×`), and each change the platform reports is an
-- | emission outside any feed — the editor whose user is the platform. A
-- | fact the platform may never supply is a variant with its named cases
-- | (`geolocated`'s `located`/`unknown`/`denied`), never a `Maybe`, so the
-- | gate never starves on it. The watch is quiet while the scope is
-- | suspended (a detached pane) and ends when it is cancelled. Several
-- | sources are merged, `RecordToRecord.do`, each owning its field
-- | (here-and-now, incident-report) — the one application use of the
-- | `×→×` merge, since every editor owns the whole row.
source
  :: forall @l a r r1
   . IsSymbol l
  => Cons l a () r1
  => Effect a
  -> ((a -> Effect Unit) -> Effect (Effect Unit))
  -> PUI Web { | r } { | r1 }
source now watch = wrap do
  sc <- gets _.scope
  first <- liftEffect now
  lastRef <- liftEffect $ Ref.new first
  propRef <- liftEffect $ Ref.new Nothing
  let row a = Record.insert (Proxy @l) a {}
  pure
    { toUser: \_ -> do
        a <- Ref.read lastRef
        Ref.read propRef >>= traverse_ (_ $ row a)
    , fromUser: \prop -> do
        Ref.write (Just prop) propRef
        Ref.read lastRef >>= prop <<< row
        stop <- watch \a -> do
          Ref.write a lastRef
          active <- scopeActive sc
          when active $ prop (row a)
        onCancel sc stop
    }

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
el tagName = wrap <<< element tagName <<< unwrap
