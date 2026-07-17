-- | The carrier: the `Web` monad (`StateT DOM Effect`) the whole algebra is
-- | instantiated at for the browser, plus the DOM building blocks and FFI
-- | the HTML vocabulary (`PUI.HTML`) and the design system (`PUI.MDC`) are
-- | built from. No widgets live here — for the 1-1 HTML vocabulary see
-- | `PUI.HTML`.
module PUI.Web
  ( DOM
  , Event
  , Node
  , Web
  , addClass
  , addEventListener
  , appendChild
  , appendRawHtml
  , attachable
  , attribute
  , clazz
  , createTextNode
  , documentBody
  , element
  , getChecked
  , getValue
  , isFocused
  , onInputDebounced
  , onClickXY
  , onKeyClick
  , removeAllChildren
  , removeAttribute
  , removeClass
  , runDomInNode
  , selectedNode
  , setAttribute
  , setChecked
  , setInnerHTML
  , setTextNodeValue
  , setValue
  , uniqueId
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Default (class Default, default)
import Data.Foldable (for_)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import PUI (PropagationStatus, PUI)
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

element :: forall a. String -> Web a -> Web a
element tagName contents = do
  newNode <- liftEffect $ createElement tagName
  parentNode <- gets _.parent
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
foreign import selectedNode :: String -> Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElement :: String -> Effect Node
foreign import insertBefore :: Node -> Node -> Effect Unit
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import removeAllNodesBetweenSiblings :: Node -> Node -> Effect Unit
foreign import appendRawHtml :: String -> Node -> Effect Node
foreign import moveAllNodesBetweenSiblings :: Node -> Node -> Node -> Effect Unit
foreign import addEventListener :: String -> Node -> (Event -> Effect Unit) -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import setAttributes :: Node -> Object String -> Effect Unit
foreign import setAttribute :: Node -> String -> String -> Effect Unit
foreign import removeAttribute :: Node -> String -> Effect Unit
foreign import removeAllChildren :: Node -> Effect Unit
foreign import addClass :: Node -> String -> Effect Unit
foreign import removeClass :: Node -> String -> Effect Unit
foreign import insertAsFirstChild :: Node -> Node -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
foreign import randomElementId :: Effect String
foreign import lastChild :: Node -> Effect Node
foreign import setInnerHTML :: Node -> String -> Effect Unit
foreign import onKeyClick :: Node -> (String -> Effect Unit) -> Effect Unit

-- | Pointer-down emitter with coordinates mapped into the element's local
-- | space (an SVG's viewBox units when present, CSS pixels otherwise) —
-- | works for mouse, touch and pen alike.
foreign import onClickXY :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit
foreign import onInputDebounced :: Node -> Number -> (String -> Effect Unit) -> Effect Unit

runDomInNode :: forall a. Node -> Web a -> Effect a
runDomInNode node (Web domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node }

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0
