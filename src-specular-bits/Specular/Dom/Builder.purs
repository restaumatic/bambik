module Specular.Dom.Builder
  ( AttrValue(..)
  , Attrs
  , Builder
  , Event
  , EventType
  , Namespace
  , Node
  , Slot
  , TagName
  , addEventListener
  , appendChild
  , appendChildToBody
  , appendSlot
  , attr
  , classes
  , comment
  , createDocumentFragment
  , destroySlot
  , elAttr
  , getChecked
  , getEnv
  , getParentNode
  , getValue
  , local
  , newSlot
  , populateBody
  , populateSlot
  , rawHtml
  , removeNode
  , runBuilder
  , setAttributes
  , setChecked
  , setValue
  , text
  )
  where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
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


newtype Builder env a = Builder (RIO (BuilderEnv env) a)

type BuilderEnv env =
  { parent :: Node
  , userEnv :: env
  }

derive newtype instance functorBuilder :: Functor (Builder env)
derive newtype instance applyBuilder :: Apply (Builder env)
derive newtype instance applicativeBuilder :: Applicative (Builder env)
derive newtype instance bindBuilder :: Bind (Builder env)
derive newtype instance monadBuilder :: Monad (Builder env)
derive newtype instance monadEffectBuilder :: MonadEffect (Builder env)

instance monadAskBuilder :: MonadAsk env (Builder env) where
  ask = _.userEnv <$> getEnv

instance monadReaderBuilder :: MonadReader env (Builder env) where
  local = local

local :: forall e r a. (e -> r) -> Builder r a -> Builder e a
local fn (Builder x) = Builder $ RIO.local (\env -> env { userEnv = fn env.userEnv }) x

mkBuilder :: forall env a. (BuilderEnv env -> Effect a) -> Builder env a
mkBuilder = Builder <<< rio

unBuilder :: forall env a. Builder env a -> RIO (BuilderEnv env) a
unBuilder (Builder f) = f

runBuilder :: forall a env. env -> Node -> Builder env a -> Effect a
runBuilder = runBuilderWithUserEnv

runBuilderWithUserEnv :: forall env a. env -> Node -> Builder env a -> Effect a
runBuilderWithUserEnv userEnv parent (Builder f) = do
  let env = { parent, userEnv }
  runRIO env f

getEnv :: forall env. Builder env (BuilderEnv env)
getEnv = Builder ask

setParent :: forall env. Node -> BuilderEnv env -> BuilderEnv env
setParent parent env = env { parent = parent }

getParentNode :: forall env. Builder env Node
getParentNode = Builder (asks _.parent)

data Slot m = Slot
  (forall a. m a -> Effect a) -- ^ run inner widget, replace contents
  (Effect Unit) -- ^ destroy
  (Effect (Slot m)) -- ^ Create a new slot after this one

populateSlot :: forall m a. Slot m -> m a -> Effect a
populateSlot (Slot replace _ _) = replace

destroySlot :: forall m. Slot m -> Effect Unit
destroySlot (Slot _ destroy _) = destroy

appendSlot :: forall m. Slot m -> Effect (Slot m)
appendSlot (Slot _ _ append) = append

newSlot :: forall env. Builder env (Slot ((Builder env)))
newSlot = do
  env <- getEnv

  placeholderBefore <- liftEffect $ createTextNodeImpl ""
  placeholderAfter <- liftEffect $ createTextNodeImpl ""

  liftEffect $ appendChild placeholderBefore env.parent
  liftEffect $ appendChild placeholderAfter env.parent

  let
    populate :: forall x. Builder env x -> Effect x
    populate builder = measured "slot populated" do
      removeAllBetween placeholderBefore placeholderAfter
      fragment <- createDocumentFragment
      result <- runBuilderWithUserEnv env.userEnv fragment builder
      m_parent <- parentNodeImpl Just Nothing placeholderAfter
      case m_parent of
        Just parent -> do
          insertBefore fragment placeholderAfter parent
        Nothing ->
          pure unit -- FIXME
      pure result

    destroy :: Effect Unit
    destroy = do
      removeAllBetween placeholderBefore placeholderAfter
      removeNode placeholderBefore
      removeNode placeholderAfter

    append :: Effect (Slot (Builder env))
    append = do
      fragment <- createDocumentFragment
      slot <- runBuilderWithUserEnv env.userEnv fragment newSlot

      m_parent <- parentNodeImpl Just Nothing placeholderAfter

      case m_parent of
        Just parent -> do
          insertBefore fragment placeholderAfter parent
        Nothing ->
          pure unit -- FIXME

      pure slot

  pure $ Slot populate destroy append

text :: forall env. String -> Builder env Unit
text str = mkBuilder \env -> do
  node <- createTextNodeImpl str
  appendChild node env.parent

rawHtml :: forall env. String -> Builder env Unit
rawHtml html = mkBuilder \env ->
  appendRawHtml html env.parent

elAttr :: forall a env. TagName -> Attrs -> Builder env a -> Builder env (Tuple Node a)
elAttr tagName attrs inner = do
  env <- getEnv
  node <- liftEffect $ createElementNS Nothing tagName
  liftEffect $ setAttributes node attrs
  result <- Builder $ RIO.local (setParent node) $ unBuilder inner
  liftEffect $ appendChild node env.parent
  pure $ Tuple node result

comment :: forall env. String -> Builder env Unit
comment str = mkBuilder \env -> do
  node <- createCommentNode str
  appendChild node env.parent

instance semigroupBuilder :: Semigroup a => Semigroup (Builder node a) where
  append = lift2 append

instance monoidBuilder :: Monoid a => Monoid (Builder node a) where
  mempty = pure mempty

populateBody :: forall a. Builder Unit a → Effect a
populateBody builder = do
  body <- documentBody
  runBuilder unit body builder

type Attrs = Object AttrValue

data AttrValue = ClassNames String | AttrValue String

classes :: String -> Attrs
classes spaceSeparatedClassNames = "class" := ClassNames spaceSeparatedClassNames

attr ∷ String → String → Attrs
attr attrName attrValue =  attrName := AttrValue attrValue

instance Semigroup AttrValue where
  append (ClassNames classNames1) (ClassNames classNames2) = ClassNames $ classNames1 <> " " <> classNames2
  append _ second = second

instance Show AttrValue where
  show (ClassNames s) = s
  show (AttrValue s) = s

-- | Convenient syntax for building Attrs
infix 8 Object.singleton as :=

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

createDocumentFragment :: Effect Node
createDocumentFragment = createDocumentFragmentImpl

-- | Create an element, optionally with namespace.
createElementNS :: Maybe Namespace -> TagName -> Effect Node
createElementNS (Just namespace) = createElementNSImpl namespace
createElementNS Nothing = createElementImpl

setAttributes :: Node -> Attrs -> Effect Unit
setAttributes node attrs = runEffectFn2 setAttributesImpl node (show <$> attrs)

-- | `insertBefore newNode nodeAfter parent`
-- | Insert `newNode` before `nodeAfter` in `parent`
insertBefore :: Node -> Node -> Node -> Effect Unit
insertBefore = insertBeforeImpl

-- | `appendChild newNode parent`
appendChild :: Node -> Node -> Effect Unit
appendChild = appendChildImpl

appendChildToBody ::Node -> Effect Unit
appendChildToBody child = do
  body <- documentBody
  appendChildImpl child body

-- | Append a chunk of raw HTML to the end of the node.
appendRawHtml :: String -> Node -> Effect Unit
appendRawHtml = appendRawHtmlImpl

-- | `removeAllBetween from to`
-- |
-- | Remove all nodes after `from` and before `to` from their
-- | parent. `from` and `to` are not removed.
-- |
-- | Assumes that `from` and `to` have the same parent,
-- | and `from` is before `to`.
removeAllBetween :: Node -> Node -> Effect Unit
removeAllBetween = removeAllBetweenImpl

createCommentNode ∷ String → Effect Node
createCommentNode = createCommentNodeImpl

logIndent :: Ref.Ref Int
logIndent = unsafePerformEffect $ Ref.new 0

measured :: forall a m. Bind m ⇒ MonadEffect m ⇒ String → m a → m a
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

foreign import documentBody :: Effect Node
foreign import removeNode :: Node -> Effect Unit
foreign import createTextNodeImpl :: String -> Effect Node
foreign import setTextImpl :: Node -> String -> Effect Unit
foreign import createDocumentFragmentImpl :: Effect Node
foreign import createElementNSImpl :: Namespace -> TagName -> Effect Node
foreign import createElementImpl :: TagName -> Effect Node
foreign import removeAttributesImpl :: Node -> Array String -> Effect Unit
foreign import parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> Effect (Maybe Node)
foreign import insertBeforeImpl :: Node -> Node -> Node -> Effect Unit
foreign import appendChildImpl :: Node -> Node -> Effect Unit
foreign import removeAllBetweenImpl :: Node -> Node -> Effect Unit
foreign import appendRawHtmlImpl :: String -> Node -> Effect Unit
foreign import moveAllBetweenInclusiveImpl :: Node -> Node -> Node -> Effect Unit
foreign import addEventListenerImpl :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)
foreign import preventDefault :: Event -> Effect Unit
foreign import innerHTML :: Node -> Effect String
foreign import createCommentNodeImpl :: String -> Effect Node
foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import setAttributesImpl :: EffectFn2 Node (Object String) Unit
