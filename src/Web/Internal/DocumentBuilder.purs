module Web.Internal.DocumentBuilder
  ( DOMBuilderEnv
  , DocumentBuilder
  , Event
  , Node
  , addClass
  , addEventListener
  , appendChild
  , at
  , ats
  , attachable'
  , cl
  , createCommentNode
  , createDocumentFragment
  , documentBody
  , element
  , getChecked
  , getValue
  , html
  , initializeInBody
  , initializeInNode
  , insertAsFirstChild
  , insertBefore
  , listener
  , moveAllNodesBetweenSiblings
  , removeAllNodesBetweenSiblings
  , removeAttribute
  , removeClass
  , runDomInNode
  , setAttribute
  , setChecked
  , setTextNodeValue
  , setValue
  , speaker
  , text
  , uniqueId
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)
-- import Web.Internal.DOM (Event, Node, TagName, addClass, addEventListener, appendChild, appendRawHtml, createCommentNode, createDocumentFragment, createElement, createTextNode, documentBody, insertAsFirstChild, insertBefore, moveAllNodesBetweenSiblings, removeAllNodesBetweenSiblings, setAttribute, setAttributes)

-- Builds DOM and keeping track of parent/last sibling node
newtype DocumentBuilder a = DocumentBuilder (StateT DOMBuilderEnv Effect a)

type DOMBuilderEnv =
  { parent :: Node
  , sibling :: Node -- last sibling
  }

derive newtype instance Functor DocumentBuilder
derive newtype instance Apply DocumentBuilder
derive newtype instance Applicative DocumentBuilder
derive newtype instance Bind DocumentBuilder
derive newtype instance Monad DocumentBuilder
derive newtype instance MonadEffect DocumentBuilder
derive newtype instance MonadState DOMBuilderEnv DocumentBuilder

initializeInBody :: forall a. DocumentBuilder (a -> DocumentBuilder Unit) → a -> Effect Unit
initializeInBody dom a = measured "initialized" do
  body <- documentBody
  initializeInNode body dom a

initializeInNode :: forall a. Node -> DocumentBuilder (a -> DocumentBuilder Unit) -> a -> Effect Unit
initializeInNode node dom a = runDomInNode node do
  { attach, update } <- attachable' true dom
  update a
  attach

text :: DocumentBuilder Unit
text = do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode mempty
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}

html :: String -> DocumentBuilder Unit
html htmlString = do
  parent <- gets _.parent
  lastNode <- liftEffect $ appendRawHtml htmlString parent
  modify_ _ { sibling = lastNode}

element :: forall a. TagName -> DocumentBuilder a -> DocumentBuilder a
element tagName contents = do
  newNode <- liftEffect $ createElement tagName
  parentNode <- gets _.parent
  liftEffect $ appendChild newNode parentNode
  modify_ _ { parent = newNode}
  result <- contents
  modify_ _ { parent = parentNode, sibling = newNode}
  pure result

at :: String -> String -> DocumentBuilder Unit
at name value = do
  node <- gets _.sibling
  liftEffect $ setAttribute node name value

ats :: Object String -> DocumentBuilder Unit
ats attrs = do
  node <- gets _.sibling
  liftEffect $ setAttributes node attrs

cl :: String -> DocumentBuilder Unit
cl name = do
  node <- gets _.sibling
  liftEffect $ addClass node name
  pure unit

listener :: String -> (Event -> DocumentBuilder Unit) -> DocumentBuilder Unit
listener eventType callback = do
  node <- gets _.sibling
  void $ liftEffect $ addEventListener eventType node (\evt -> runDomInNode node $ callback evt)

speaker :: forall a. (Node -> a) -> DocumentBuilder a
speaker action = do
  node <- gets _.sibling
  pure $ action node

uniqueId :: Effect String
uniqueId = randomElementId

-- private

runDomInNode :: forall a. Node -> DocumentBuilder a -> Effect a
runDomInNode node (DocumentBuilder domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node }

-- TODO remove?
attachable' :: forall a. Boolean -> DocumentBuilder (a -> DocumentBuilder Unit) -> DocumentBuilder { update :: a -> DocumentBuilder Unit, attach :: DocumentBuilder Unit, detach :: DocumentBuilder Unit }
attachable' removePrecedingSiblingNodes dom = do
  parent <- gets _.parent
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do

    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo

    (if removePrecedingSiblingNodes then insertAsFirstChild else appendChild) placeholderBefore parent
    appendChild placeholderAfter parent

    initialDocumentFragment <- createDocumentFragment
    update <- runDomInNode initialDocumentFragment dom

    documentFragmentRef <- Ref.new initialDocumentFragment

    let
      attach :: DocumentBuilder Unit
      attach = measured' slotNo "attached" $ liftEffect do
        removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
        documentFragment <- Ref.modify' (\documentFragment -> { state: unsafeCoerce unit, value: documentFragment}) documentFragmentRef
        -- inserting documentFragment makes it empty but just in case not keeping reference to it while it's not needed
        documentFragment `insertBefore` placeholderAfter

      detach :: DocumentBuilder Unit
      detach = measured' slotNo "detached" $ liftEffect do
        documentFragment <- createDocumentFragment
        moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
        Ref.write documentFragment documentFragmentRef

    pure $ { attach, detach, update }
    where
      measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
      measured' slotNo actionName = measured $ "component " <> show slotNo <> " " <> actionName


logIndent :: Ref.Ref Int
logIndent = unsafePerformEffect $ Ref.new 0

measured :: forall a m. MonadEffect m ⇒ String → m a → m a
measured actionName action = do
  start <- liftEffect now
  _ <- liftEffect $ Ref.modify (_ + 1) logIndent
  a <- action
  currentIndent <- liftEffect $ Ref.modify (_ - 1) logIndent
  stop <- liftEffect now
  info $ "[DocumentBuilder] " <> repeatStr currentIndent "." <> actionName <> " in " <> show (unwrap (unInstant stop) - unwrap (unInstant start)) <> " ms"
  pure a
    where
      repeatStr i s
        | i <= 0 = ""
        | otherwise = s <> repeatStr (i - 1) s

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0

newPlaceholderBefore :: forall a. Show a ⇒ a → Effect Node
newPlaceholderBefore slotNo = createCommentNode $ "begin component " <> show slotNo

newPlaceholderAfter :: forall a. Show a ⇒ a → Effect Node
newPlaceholderAfter slotNo = createCommentNode $ "end component " <> show slotNo

foreign import randomElementId :: Effect String

-- from former DOM.purs

type TagName = String

-- | XML namespace URI.
type Namespace = String

-- | DOMBuilder node.
foreign import data Node :: Type

-- | DOMBuilder event.
foreign import data Event :: Type

foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import documentBody :: Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElementNS :: Namespace -> TagName -> Effect Node
foreign import createElement :: TagName -> Effect Node
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
foreign import addClass :: Node -> String -> Effect Unit
foreign import removeClass :: Node -> String -> Effect Unit
foreign import insertAsFirstChild :: Node -> Node -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
