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
  , appendSlot
  , attr
  , classes
  , comment
  , destroySlot
  , elAttr
  , getEnv
  , getParentNode
  , local
  , mkBuilder'
  , newSlot
  , onDomEvent
  , rawHtml
  , replaceSlot
  , runBuilder
  , runBuilder'
  , runMainBuilderInBody
  , runMainWidgetInNode
  , text
  , unBuilder
  )
  where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple), fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify_, new, read, write)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Specular.Internal.Effect (DelayedEffects, emptyDelayed, pushDelayed, sequenceEffects, unsafeFreezeDelayed)
import Specular.Internal.RIO (RIO(..), rio, runRIO)
import Specular.Internal.RIO as RIO
import Specular.Profiling as Profiling
import Foreign.Object (Object)
import Foreign.Object as Object


newtype Builder env a = Builder (RIO (BuilderEnv env) a)

type BuilderEnv env =
  { parent :: Node
  , cleanup :: DelayedEffects
  , userEnv :: env
  }

derive newtype instance functorBuilder :: Functor (Builder env)
derive newtype instance applyBuilder :: Apply (Builder env)
derive newtype instance applicativeBuilder :: Applicative (Builder env)
derive newtype instance bindBuilder :: Bind (Builder env)
derive newtype instance monadBuilder :: Monad (Builder env)
derive newtype instance monadEffectBuilder :: MonadEffect (Builder env)

onCleanup :: forall env. Effect Unit -> Builder env Unit
onCleanup action = mkBuilder $ \env -> pushDelayed env.cleanup action

instance monadAskBuilder :: MonadAsk env (Builder env) where
  ask = _.userEnv <$> getEnv

instance monadReaderBuilder :: MonadReader env (Builder env) where
  local = local

local :: forall e r a. (e -> r) -> Builder r a -> Builder e a
local fn (Builder x) = Builder $ RIO.local (\env -> env { userEnv = fn env.userEnv }) x

mkBuilder' :: forall env a. (EffectFn1 (BuilderEnv env) a) -> Builder env a
mkBuilder' = Builder <<< RIO

mkBuilder :: forall env a. (BuilderEnv env -> Effect a) -> Builder env a
mkBuilder = Builder <<< rio

unBuilder :: forall env a. Builder env a -> RIO (BuilderEnv env) a
unBuilder (Builder f) = f

runBuilder' :: forall env a. EffectFn2 (BuilderEnv env) (Builder env a) a
runBuilder' = mkEffectFn2 \env (Builder (RIO f)) -> runEffectFn1 f env

runBuilder :: forall a. Node -> Builder Unit a -> Effect (Tuple a (Effect Unit))
runBuilder = runBuilderWithUserEnv unit

runBuilderWithUserEnv :: forall env a. env -> Node -> Builder env a -> Effect (Tuple a (Effect Unit))
runBuilderWithUserEnv userEnv parent (Builder f) = do
  actionsMutable <- emptyDelayed
  let env = { parent, cleanup: actionsMutable, userEnv }
  result <- runRIO env f
  actions <- unsafeFreezeDelayed actionsMutable
  pure (Tuple result (sequenceEffects actions))

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

replaceSlot :: forall m a. Slot m -> m a -> Effect a
replaceSlot (Slot replace _ _) = replace

destroySlot :: forall m. Slot m -> Effect Unit
destroySlot (Slot _ destroy _) = destroy

appendSlot :: forall m. Slot m -> Effect (Slot m)
appendSlot (Slot _ _ append) = append

newSlot :: forall env. Builder env (Slot ((Builder env)))
newSlot = do
  env <- getEnv

  placeholderBefore <- liftEffect $ createTextNode ""
  placeholderAfter <- liftEffect $ createTextNode ""
  liftEffect $ appendChild placeholderBefore env.parent
  liftEffect $ appendChild placeholderAfter env.parent

  cleanupRef <- liftEffect $ new (mempty :: Effect Unit)

  let
    replace :: forall a. Builder env a -> Effect a
    replace inner = Profiling.measure "slot replace" do
      Profiling.measure "slot remove DOM" do
        removeAllBetween placeholderBefore placeholderAfter

      fragment <- createDocumentFragment
      Tuple result cleanup <- Profiling.measure "slot init" do
        runBuilderWithUserEnv env.userEnv fragment inner
      join $ read cleanupRef

      m_parent <- parentNode placeholderAfter

      case m_parent of
        Just parent -> do
          insertBefore fragment placeholderAfter parent

          write
            ( Profiling.measure "slot cleanup" do
                cleanup
                write mempty cleanupRef -- TODO: explain this
            )
            cleanupRef

        Nothing ->
          -- we've been removed from the DOM
          write cleanup cleanupRef

      pure result

    destroy :: Effect Unit
    destroy = do
      removeAllBetween placeholderBefore placeholderAfter
      removeNode placeholderBefore
      removeNode placeholderAfter
      join $ read cleanupRef

    append :: Effect (Slot (Builder env))
    append = do
      fragment <- createDocumentFragment
      Tuple slot cleanup <- runBuilderWithUserEnv env.userEnv fragment newSlot
      modify_ (_ *> cleanup) cleanupRef -- FIXME: memory leak if the inner slot is destroyed

      m_parent <- parentNode placeholderAfter

      case m_parent of
        Just parent -> do
          insertBefore fragment placeholderAfter parent
        Nothing ->
          pure unit -- FIXME

      pure slot

  onCleanup $ join $ read cleanupRef

  pure $ Slot replace destroy append

text :: forall env. String -> Builder env Unit
text str = mkBuilder \env -> do
  node <- createTextNode str
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


-- | Runs a widget in the specified parent element. Returns the result and cleanup action.
runWidgetInNode :: forall a. Node -> Builder Unit a -> Effect (Tuple a (Effect Unit))
runWidgetInNode parent widget = runBuilder parent do
  slot <- newSlot
  onCleanup (destroySlot slot)
  liftEffect $ replaceSlot slot widget

foreign import documentBody :: Effect Node

-- | Runs a widget in the specified parent element and discards cleanup action.
runMainWidgetInNode :: forall a. Node -> Builder Unit a -> Effect a
runMainWidgetInNode parent widget = fst <$> runWidgetInNode parent widget

-- | Runs a builder in `document.body` and discards cleanup action.
runMainBuilderInBody :: forall a. Builder Unit a -> Effect a
runMainBuilderInBody widget = do
  body <- documentBody
  runMainWidgetInNode body widget

-- copied from Browser.proplus

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
addEventListener :: Node -> EventType -> (Event -> Effect Unit) -> Effect (Effect Unit)
addEventListener node etype callback = addEventListenerImpl etype callback node

createTextNode :: String -> Effect Node
createTextNode = createTextNodeImpl

setText :: Node -> String -> Effect Unit
setText = setTextImpl

createDocumentFragment :: Effect Node
createDocumentFragment = createDocumentFragmentImpl

-- | Create an element, optionally with namespace.
createElementNS :: Maybe Namespace -> TagName -> Effect Node
createElementNS (Just namespace) = createElementNSImpl namespace
createElementNS Nothing = createElementImpl

createElement :: TagName -> Effect Node
createElement = createElementNS Nothing

setAttributes :: Node -> Attrs -> Effect Unit
setAttributes node attrs = runEffectFn2 _setAttributes node (show <$> attrs)

removeAttributes :: Node -> Array String -> Effect Unit
removeAttributes = removeAttributesImpl

-- | Return parent node of the node,
-- | or Nothing if it has been detached.
parentNode :: Node -> Effect (Maybe Node)
parentNode = parentNodeImpl Just Nothing

-- | `insertBefore newNode nodeAfter parent`
-- | Insert `newNode` before `nodeAfter` in `parent`
insertBefore :: Node -> Node -> Node -> Effect Unit
insertBefore = insertBeforeImpl

-- | `appendChild newNode parent`
appendChild :: Node -> Node -> Effect Unit
appendChild = appendChildImpl

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

-- | `moveAllBetweenInclusive from to parent`
-- |
-- | Moves `from`, all nodes after `from` and before `to` and `to` to
-- | `parent`.
-- |
-- | Assumes that `from` and `to` have the same parent,
-- | and `from` is before `to`.
moveAllBetweenInclusive :: Node -> Node -> Node -> Effect Unit
moveAllBetweenInclusive = moveAllBetweenInclusiveImpl


onDomEvent :: forall m. MonadEffect m => EventType -> Node -> (Event -> Effect Unit) -> m Unit
onDomEvent eventType node handler = do
  void $ liftEffect $ addEventListener node eventType handler
  -- onCleanup unsub
createCommentNode ∷ String → Effect Node
createCommentNode = createCommentNodeImpl

-- | Remove node from its parent node. No-op when the node has no parent.
foreign import removeNode :: Node -> Effect Unit
foreign import createTextNodeImpl :: String -> Effect Node
foreign import setTextImpl :: Node -> String -> Effect Unit
foreign import createDocumentFragmentImpl :: Effect Node
foreign import createElementNSImpl :: Namespace -> TagName -> Effect Node
foreign import createElementImpl :: TagName -> Effect Node
foreign import _setAttributes :: EffectFn2 Node (Object String) Unit
foreign import removeAttributesImpl :: Node -> Array String -> Effect Unit
foreign import parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> Effect (Maybe Node)
foreign import insertBeforeImpl :: Node -> Node -> Node -> Effect Unit
foreign import appendChildImpl :: Node -> Node -> Effect Unit
foreign import removeAllBetweenImpl :: Node -> Node -> Effect Unit
foreign import appendRawHtmlImpl :: String -> Node -> Effect Unit
foreign import moveAllBetweenInclusiveImpl :: Node -> Node -> Node -> Effect Unit
foreign import addEventListenerImpl :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)
-- | JS `Event.preventDefault()`.
foreign import preventDefault :: Event -> Effect Unit
-- | Get `innerHTML` of a node.
foreign import innerHTML :: Node -> Effect String
foreign import createCommentNodeImpl :: String -> Effect Node
