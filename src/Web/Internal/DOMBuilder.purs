module Web.Internal.DOMBuilder
  ( DOMBuilder
  , element
  , getCurrentNode
  , html
  , initializeInBody
  , initializeInNode
  , text
  )
  where

import Prelude

import Control.Monad.Reader (asks)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Propagator (class MonadGUI)
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Node, TagName, appendChild, appendRawHtml, createCommentNode, createDocumentFragment, createElement, createTextNode, documentBody, insertAsFirstChild, insertBefore, lastChild, moveAllNodesBetweenSiblings, removeAllNodesBetweenSiblings, setAttributes, setTextNodeValue)
import Web.Internal.RIO (RIO, runRIO)
import Web.Internal.RIO as RIO

newtype DOMBuilder a = DOMBuilder (RIO DOMBuilderEnv a)

type DOMBuilderEnv =
  { parent :: Node
  }

derive newtype instance Functor DOMBuilder
derive newtype instance Apply DOMBuilder
derive newtype instance Applicative DOMBuilder
derive newtype instance Bind DOMBuilder
derive newtype instance Monad DOMBuilder
derive newtype instance MonadEffect DOMBuilder

instance MonadGUI DOMBuilder where
  attachable = attachable' false

-- TODO should be private
getCurrentNode :: DOMBuilder Node
getCurrentNode = do
  parent <- getParentNode
  liftEffect $ lastChild parent

initializeInBody :: forall a. DOMBuilder (a -> Effect Unit) → a -> Effect Unit
initializeInBody dom a = measured "initialized" do
  body <- documentBody
  initializeInNode body dom a

initializeInNode :: forall a. Node -> DOMBuilder (a -> Effect Unit) -> a -> Effect Unit
initializeInNode node dom a = runDomInNode node do
  { attach, update } <- attachable' true dom
  liftEffect do
    update a
    attach

text :: DOMBuilder { write :: String -> Effect Unit }
text = do
  parent <- getParentNode
  liftEffect $ do
    node <- createTextNode mempty
    appendChild node parent
    pure
      { write: setTextNodeValue node
      }

html :: String -> DOMBuilder Unit
html htmlString = do
  parent <- getParentNode
  liftEffect $ appendRawHtml htmlString parent

element :: forall a. TagName -> Object String -> DOMBuilder a -> DOMBuilder (Tuple Node a)
element tagName attrs contents = do
  parent <- getParentNode
  node <- liftEffect $ createElement tagName
  liftEffect $ setAttributes node attrs
  result <- DOMBuilder $ RIO.local (\env -> env { parent = node }) (unDOMBuilder contents)
  liftEffect $ appendChild node parent
  pure $ Tuple node result

-- private

unDOMBuilder ∷ ∀ (a ∷ Type). DOMBuilder a → RIO { parent ∷ Node } a
unDOMBuilder (DOMBuilder r) = r

getParentNode :: DOMBuilder Node
getParentNode = DOMBuilder (asks _.parent)

runDomInNode :: forall a. Node -> DOMBuilder a -> Effect a
runDomInNode node (DOMBuilder f) = runRIO { parent: node } f

attachable' :: forall a. Boolean -> DOMBuilder (a -> Effect Unit) -> DOMBuilder { update :: a -> Effect Unit, attach :: Effect Unit, detach :: Effect Unit }
attachable' removePrecedingSiblingNodes dom = do
  parent <- getParentNode
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
      attach :: Effect Unit
      attach = measured' slotNo "attached" do
        removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
        documentFragment <- Ref.modify' (\documentFragment -> { state: unsafeCoerce unit, value: documentFragment}) documentFragmentRef
        -- inserting documentFragment makes it empty but just in case not keeping reference to it while it's not needed
        documentFragment `insertBefore` placeholderAfter

      detach :: Effect Unit
      detach = measured' slotNo "detached" do
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
  info $ "[DOMBuilder] " <> repeatStr currentIndent "." <> actionName <> " in " <> show (unwrap (unInstant stop) - unwrap (unInstant start)) <> " ms"
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
