module Web.Internal.DOM
  ( AttrValue(..)
  , Attrs
  , Component
  , DOM
  , Event
  , Node
  , TagName
  , TextValue(..)
  , addEventCallback
  , attachComponent
  , attr
  , classes
  , createComponent
  , createElementNS
  , createTextValue
  , detachComponent
  , elAttr
  , getChecked
  , getCurrentNode
  , getParentNode
  , getValue
  , initializeInBody
  , rawHtml
  , setAttributes
  , setChecked
  , setValue
  , writeTextValue
  )
  where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (asks)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Specular.Internal.RIO (RIO, runRIO)
import Specular.Internal.RIO as RIO
import Unsafe.Coerce (unsafeCoerce)

newtype DOM a = DOM (RIO DOMEnv a)

type DOMEnv =
  { parent :: Node
  }

derive newtype instance Functor DOM
derive newtype instance Apply DOM
derive newtype instance Applicative DOM
derive newtype instance Bind DOM
derive newtype instance Monad DOM
derive newtype instance MonadEffect DOM

initializeInBody :: forall a. DOM a → (a -> Effect Unit) -> Effect Unit
initializeInBody dom initializer =  measured "initialized" do
  body <- documentBody
  initializeInNode body do
    Tuple documentComponent result <- createComponent' true dom
    liftEffect $ do
      initializer result
      attachComponent documentComponent

initializeInNode :: forall a. Node -> DOM a -> Effect a
initializeInNode node (DOM f) = runRIO { parent: node } f

data TextValue = TextValue
  { write :: String -> Effect Unit
  }

createTextValue :: DOM TextValue
createTextValue = do
  parent <- getParentNode
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do
    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo
    appendChild placeholderBefore parent
    node <- createTextNode mempty
    appendChild node parent
    appendChild placeholderAfter parent
    pure $ TextValue
      { write: \str -> measured' slotNo "written" do
        setTextNodeValue node str
      }
      where
        measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
        measured' slotNo actionName = measured $ "slot " <> show slotNo <> ": text value " <> actionName

writeTextValue :: TextValue -> String -> Effect Unit
writeTextValue (TextValue { write }) = write

data Component = Component
  { attach :: Effect Unit
  , detach :: Effect Unit
  }

createComponent :: forall a. DOM a -> DOM (Tuple Component a)
createComponent = createComponent' false

attachComponent :: Component -> Effect Unit
attachComponent (Component { attach }) = attach

detachComponent :: Component -> Effect Unit
detachComponent (Component { detach }) = detach

--

rawHtml :: String -> DOM Unit
rawHtml html = do
  parent <- getParentNode
  liftEffect $ appendRawHtml html parent

elAttr :: forall a. TagName -> Attrs -> DOM a -> DOM (Tuple Node a)
elAttr tagName attrs (DOM dom) = do
  parent <- getParentNode
  node <- liftEffect $ createElementNS Nothing tagName
  liftEffect $ setAttributes node attrs
  result <- DOM $ RIO.local (\env -> env { parent = node }) dom
  liftEffect $ appendChild node parent
  pure $ Tuple node result

instance Semigroup a => Semigroup (DOM a) where
  append = lift2 append

instance Monoid a => Monoid (DOM a) where
  mempty = pure mempty

type Attrs = Object AttrValue

data AttrValue = ClassNames String | AttrValue String

classes :: String -> Attrs
classes spaceSeparatedClassNames = Object.singleton "class" $ ClassNames spaceSeparatedClassNames

attr ∷ String → String → Attrs
attr attrName attrValue = Object.singleton attrName $ AttrValue attrValue

instance Semigroup AttrValue where
  append (ClassNames classNames1) (ClassNames classNames2) = ClassNames $ classNames1 <> " " <> classNames2
  append _ second = second

instance Show AttrValue where
  show (ClassNames s) = s
  show (AttrValue s) = s

type TagName = String

-- | XML namespace URI.
type Namespace = String

-- | DOM node.
foreign import data Node :: Type

-- | DOM event.
foreign import data Event :: Type

-- | HTML event type, e.g. "click".
type EventType = String

addEventCallback :: EventType -> Node -> (Event -> Effect Unit) -> Effect Unit
addEventCallback etype node callback = void $ addEventListener etype (measured (etype <> " event handled") <<< callback) node

-- | Create an element, optionally with namespace.
createElementNS :: Maybe Namespace -> TagName -> Effect Node
createElementNS (Just namespace) = createElementNSImpl namespace
createElementNS Nothing = createElement

setAttributes :: Node -> Attrs -> Effect Unit
setAttributes node attrs = runEffectFn2 setAttributesImpl node (show <$> attrs)

foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit

-- private

getParentNode :: DOM Node
getParentNode = DOM (asks _.parent)

getCurrentNode :: DOM Node
getCurrentNode = do
  parent <- getParentNode
  liftEffect $ lastChild parent


createComponent' :: forall a. Boolean -> DOM a -> DOM (Tuple Component a)
createComponent' removePrecedingSiblingNodes dom = do
  parent <- getParentNode
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do

    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo

    (if removePrecedingSiblingNodes then insertAsFirstChild else appendChild) placeholderBefore parent
    appendChild placeholderAfter parent

    initialDocumentFragment <- createDocumentFragment
    built <- initializeInNode initialDocumentFragment dom

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

    pure $ Tuple (Component { attach, detach }) built
    where
      measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
      measured' slotNo actionName = measured $ "slot " <> show slotNo <> ": component " <> actionName


logIndent :: Ref.Ref Int
logIndent = unsafePerformEffect $ Ref.new 0

measured :: forall a m. MonadEffect m ⇒ String → m a → m a
measured actionName action = do
  start <- liftEffect now
  _ <- liftEffect $ Ref.modify (_ + 1) logIndent
  a <- action
  currentIndent <- liftEffect $ Ref.modify (_ - 1) logIndent
  stop <- liftEffect now
  info $ "[DOM] " <> repeatStr currentIndent "." <> actionName <> " in " <> show (unwrap (unInstant stop) - unwrap (unInstant start)) <> " ms"
  pure a
    where
      repeatStr i s
        | i <= 0 = ""
        | otherwise = s <> repeatStr (i - 1) s

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0

newPlaceholderBefore :: forall a. Show a ⇒ a → Effect Node
newPlaceholderBefore slotNo = createCommentNode $ "BEGIN COMPONENT " <> show slotNo

newPlaceholderAfter :: forall a. Show a ⇒ a → Effect Node
newPlaceholderAfter slotNo = createCommentNode $ "END COMPONENT " <> show slotNo

foreign import documentBody :: Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElementNSImpl :: Namespace -> TagName -> Effect Node
foreign import createElement :: TagName -> Effect Node
foreign import insertBefore :: Node -> Node -> Effect Unit
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import removeAllNodesBetweenSiblings :: Node -> Node -> Effect Unit
foreign import appendRawHtml :: String -> Node -> Effect Unit
foreign import moveAllNodesBetweenSiblings :: Node -> Node -> Node -> Effect Unit
foreign import addEventListener :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import setAttributesImpl :: EffectFn2 Node (Object String) Unit
foreign import insertAsFirstChild :: Node -> Node -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
foreign import lastChild :: Node -> Effect Node
