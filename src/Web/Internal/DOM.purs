module Web.Internal.DOM
  ( AttrValue(..)
  , Attrs
  , Builder
  , DetachableDocumentFragment
  , Event
  , Node
  , TagName
  , WritableTextNode(..)
  , addEventListener
  , attachDocumentFragment
  , attr
  , buildInDocumentBody
  , classes
  , createDetachableDocumentFragment
  , createElementNS
  , createWritableTextNode
  , detachDocumentFragment
  , elAttr
  , getChecked
  , getValue
  , rawHtml
  , removeNode
  , buildInNode
  , setAttributes
  , setChecked
  , setValue
  , writeToTextNode
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
import Specular.Internal.RIO (RIO, rio, runRIO)
import Specular.Internal.RIO as RIO
import Unsafe.Coerce (unsafeCoerce)


newtype Builder a = Builder (RIO BuilderEnv a)

type BuilderEnv =
  { parent :: Node
  }

derive newtype instance functorBuilder :: Functor Builder
derive newtype instance applyBuilder :: Apply Builder
derive newtype instance applicativeBuilder :: Applicative Builder
derive newtype instance bindBuilder :: Bind Builder
derive newtype instance monadBuilder :: Monad Builder
derive newtype instance monadEffectBuilder :: MonadEffect Builder

mkBuilder :: forall a. (BuilderEnv -> Effect a) -> Builder a
mkBuilder = Builder <<< rio

unBuilder :: forall a. Builder a -> RIO BuilderEnv a
unBuilder (Builder f) = f

buildInNode :: forall a. Node -> Builder a -> Effect a
buildInNode node (Builder f) = runRIO { parent: node } f

data WritableTextNode = WritableTextNode (String -> Effect Unit)

createWritableTextNode :: Builder WritableTextNode
createWritableTextNode = do
  node <- getParentNode
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do
    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo
    appendChild placeholderBefore node
    appendChild placeholderAfter node
    pure $ WritableTextNode \str -> measured' slotNo "written" do
      newNode <- createTextNode str
      removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
      newNode `insertBefore` placeholderAfter
      where
        measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
        measured' slotNo actionName = measured $ "text node " <> show slotNo <> " " <> actionName


writeToTextNode :: WritableTextNode -> String -> Effect Unit
writeToTextNode (WritableTextNode write) = write

data DetachableDocumentFragment = DetachableDocumentFragment
  { attach :: Effect Unit
  , detach :: Effect Unit
  }

createDetachableDocumentFragment :: forall a. Builder a -> Builder (Tuple DetachableDocumentFragment a)
createDetachableDocumentFragment = createDetachableDocumentFragment' false

attachDocumentFragment :: DetachableDocumentFragment -> Effect Unit
attachDocumentFragment (DetachableDocumentFragment { attach }) = attach

detachDocumentFragment :: DetachableDocumentFragment -> Effect Unit
detachDocumentFragment (DetachableDocumentFragment { detach }) = detach

--

rawHtml :: String -> Builder Unit
rawHtml html = mkBuilder \env ->
  appendRawHtml html env.parent

elAttr :: forall a. TagName -> Attrs -> Builder a -> Builder (Tuple Node a)
elAttr tagName attrs inner = do
  parent <- getParentNode
  node <- liftEffect $ createElementNS Nothing tagName
  liftEffect $ setAttributes node attrs
  result <- Builder $ RIO.local (\env -> env { parent = node }) $ unBuilder inner
  liftEffect $ appendChild node parent
  pure $ Tuple node result

instance semigroupBuilder :: Semigroup a => Semigroup (Builder a) where
  append = lift2 append

instance monoidBuilder :: Monoid a => Monoid (Builder a) where
  mempty = pure mempty

buildInDocumentBody :: forall a. Builder a → (a -> Effect Unit) -> Effect Unit
buildInDocumentBody builder initializer =  measured "initialized" do
  body <- documentBody
  buildInNode body do
    Tuple fragment built <- createDetachableDocumentFragment' true builder
    liftEffect $ do
      initializer built
      attachDocumentFragment fragment

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

-- | Register an event listener. Returns unregister action.
addEventListener :: EventType -> Node -> (Event -> Effect Unit) -> Effect Unit
addEventListener etype node callback = void $ addEventListenerImpl etype (measured (etype <> " event handled") <<< callback) node

-- | Create an element, optionally with namespace.
createElementNS :: Maybe Namespace -> TagName -> Effect Node
createElementNS (Just namespace) = createElementNSImpl namespace
createElementNS Nothing = createElement

setAttributes :: Node -> Attrs -> Effect Unit
setAttributes node attrs = runEffectFn2 setAttributesImpl node (show <$> attrs)

foreign import removeNode :: Node -> Effect Unit
foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit

-- private

getParentNode :: Builder Node
getParentNode = Builder (asks _.parent)

createDetachableDocumentFragment' :: forall a. Boolean -> Builder a -> Builder (Tuple DetachableDocumentFragment a)
createDetachableDocumentFragment' isRoot builder = do
  parent <- getParentNode
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do

    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo

    if isRoot
      then insertAsFirstChild placeholderBefore parent
      else appendChild placeholderBefore parent
    appendChild placeholderAfter parent

    initialDocumentFragment <- createDocumentFragment
    built <- buildInNode initialDocumentFragment builder

    documentFragmentRef <- Ref.new initialDocumentFragment

    let
      attach :: Effect Unit
      attach = measured' slotNo "attached" do
        when isRoot do
          removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
        documentFragment <- Ref.modify' (\fragment -> { state: unsafeCoerce unit, value: fragment}) documentFragmentRef
          -- inserting documentFragment makes it empty but just in case not keeping reference to it while it's not needed
        documentFragment `insertBefore` placeholderAfter

      detach :: Effect Unit
      detach = measured' slotNo "detached" do
        documentFragment <- createDocumentFragment
        moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
        Ref.write documentFragment documentFragmentRef

    pure $ Tuple (DetachableDocumentFragment { attach, detach }) built
    where
      measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
      measured' slotNo actionName = measured ("document fragment " <> show slotNo <> " " <> actionName)


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

newPlaceholderBefore ∷ ∀ (a95 ∷ Type). Show a95 ⇒ a95 → Effect Node
newPlaceholderBefore slotNo = createCommentNode $ "<" <> show slotNo <> ">"

newPlaceholderAfter ∷ ∀ (a100 ∷ Type). Show a100 ⇒ a100 → Effect Node
newPlaceholderAfter slotNo = createCommentNode $ "</" <> show slotNo <> ">"

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
foreign import addEventListenerImpl :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import setAttributesImpl :: EffectFn2 Node (Object String) Unit
foreign import insertAsFirstChild :: Node -> Node -> Effect Unit

