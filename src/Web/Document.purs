-- TODO merge with Document module?
module Web.Document
  ( DocumentEnv
  , Document
  , Event
  , Node
  , addClass
  , addEventListener
  , appendChild
  , at
  , ats
  , cl
  , createCommentNode
  , createDocumentFragment
  , documentBody
  , element
  , getChecked
  , getValue
  , html
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
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (Object)

-- Builds Document and keeping track of parent/last sibling node
newtype Document a = Document (StateT DocumentEnv Effect a)

type DocumentEnv =
  { parent :: Node
  , sibling :: Node -- last sibling
  }

derive newtype instance Functor Document
derive newtype instance Apply Document
derive newtype instance Applicative Document
derive newtype instance Bind Document
derive newtype instance Monad Document
derive newtype instance MonadEffect Document
derive newtype instance MonadState DocumentEnv Document

text :: Document Unit
text = do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode mempty
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}

html :: String -> Document Unit
html htmlString = do
  parent <- gets _.parent
  lastNode <- liftEffect $ appendRawHtml htmlString parent
  modify_ _ { sibling = lastNode}

element :: forall a. TagName -> Document a -> Document a
element tagName contents = do
  newNode <- liftEffect $ createElement tagName
  parentNode <- gets _.parent
  liftEffect $ appendChild newNode parentNode
  modify_ _ { parent = newNode}
  result <- contents
  modify_ _ { parent = parentNode, sibling = newNode}
  pure result

at :: String -> String -> Document Unit
at name value = do
  node <- gets _.sibling
  liftEffect $ setAttribute node name value

ats :: Object String -> Document Unit
ats attrs = do
  node <- gets _.sibling
  liftEffect $ setAttributes node attrs

cl :: String -> Document Unit
cl name = do
  node <- gets _.sibling
  liftEffect $ addClass node name
  pure unit

listener :: String -> (Event -> Document Unit) -> Document Unit
listener eventType callback = do
  node <- gets _.sibling
  void $ liftEffect $ addEventListener eventType node (\evt -> runDomInNode node $ callback evt)

speaker :: forall a. (Node -> a) -> Document a
speaker action = do
  node <- gets _.sibling
  pure $ action node

uniqueId :: Effect String
uniqueId = randomElementId

runDomInNode :: forall a. Node -> Document a -> Effect a
runDomInNode node (Document domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node }

foreign import randomElementId :: Effect String

type TagName = String

-- | XML namespace URI.
type Namespace = String

-- | Document node.
foreign import data Node :: Type

-- | Document event.
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
