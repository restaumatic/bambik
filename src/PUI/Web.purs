-- | The browser carrier, and the root of everything web-specific: the `Web`
-- | monad (`StateT DOM Effect`) the algebra is instantiated at for the
-- | browser, plus the DOM building blocks and FFI every vocabulary beneath
-- | this module is built from.
-- |
-- | No UI components live here — those are the submodules: the element
-- | vocabularies `PUI.Web.HTML` and `PUI.Web.SVG`, and one module per
-- | design system — `PUI.Web.MDC2`, `PUI.Web.MDC3`, `PUI.Web.Shoelace`,
-- | `PUI.Web.Fluent`, `PUI.Web.Bootstrap`. A screen is written in one of
-- | those; this module is what a *new* vocabulary would be written from.
module PUI.Web
  ( DOM
  , Event
  , Node
  , OptCaption(..)
  , choice
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
  , uniqueId
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import ConvertableOptions (class ConvertOption)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, inj)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import PUI (class Hosting, PUI, Logged, setDiagnostics, setSink, setTracing)

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
-- | system only documents as markup. Like `staticText` it never changes and
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
-- | `PUI.Web.HTML.text` is outside the family: copy is a function, not a
-- | field, so it carries no label to stamp — under host diagnostics it
-- | plants a bare `text` comment marker, and in production nothing.
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
