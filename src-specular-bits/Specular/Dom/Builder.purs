module Specular.Dom.Builder
  ( Builder
  , Slot
  , appendSlot
  , replaceSlot
  , destroySlot
  , getEnv
  , getParentNode
  , local
  , mkBuilder'
  , newSlot
  , runBuilder
  , runBuilder'
  , runMainBuilderInBody
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
import Specular.Dom.Browser (Node, appendChild, appendRawHtml, createCommentNode, createDocumentFragment, createElementNS, createTextNode, insertBefore, parentNode, removeAllBetween, removeNode, setAttributes)
import Specular.Dom.Builder.Class (class MonadDomBuilder)
import Specular.Internal.Effect (DelayedEffects, emptyDelayed, pushDelayed, sequenceEffects, unsafeFreezeDelayed)
import Specular.Internal.RIO (RIO(..), rio, runRIO)
import Specular.Internal.RIO as RIO
import Specular.Profiling as Profiling

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

instance monadDomBuilderBuilder :: MonadDomBuilder (Builder env) where

  text str = mkBuilder \env -> do
    node <- createTextNode str
    appendChild node env.parent

  -- dynText dstr = do
  --   node <- mkBuilder \env -> do
  --     node <- createTextNode ""
  --     appendChild node env.parent
  --     pure node
  --   subscribeWeakDyn_ (setText node) dstr

  rawHtml html = mkBuilder \env ->
    appendRawHtml html env.parent

  -- elDynAttrNS' namespace tagName dynAttrs inner = do
  --   env <- getEnv
  --   node <- liftEffect $ createElementNS namespace tagName

  --   attrsRef <- liftEffect $ new mempty
  --   let
  --     resetAttributes newAttrs = do
  --       oldAttrs <- read attrsRef
  --       write newAttrs attrsRef
  --       let
  --         changed = SM.filterWithKey (\k v -> SM.lookup k oldAttrs /= Just v) newAttrs
  --         removed = A.filter (\k -> not (k `SM.member` newAttrs)) $ SM.keys oldAttrs

  --       removeAttributes node removed
  --       setAttributes node changed

  --   subscribeWeakDyn_ resetAttributes dynAttrs
  --   result <- Builder $ RIO.local (setParent node) $ unBuilder inner
  --   liftEffect $ appendChild node env.parent
  --   pure (Tuple node result)

  elAttr tagName attrs inner = do
    env <- getEnv
    node <- liftEffect $ createElementNS Nothing tagName
    liftEffect $ setAttributes node attrs
    result <- Builder $ RIO.local (setParent node) $ unBuilder inner
    liftEffect $ appendChild node env.parent
    pure $ Tuple node result

  liftBuilder fn = Builder (RIO fn)
  liftBuilderWithRun fn =
    Builder $ rio \env ->
      runEffectFn2 fn env (mkEffectFn2 \env' (Builder (RIO m)) -> runEffectFn1 m env')

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

-- | Runs a widget `document.body`. Returns the result and cleanup action.
runWidgetInBody :: forall a. Builder Unit a -> Effect (Tuple a (Effect Unit))
runWidgetInBody widget = do
  body <- documentBody
  runWidgetInNode body widget

-- | Runs a widget in the specified parent element and discards cleanup action.
runMainWidgetInNode :: forall a. Node -> Builder Unit a -> Effect a
runMainWidgetInNode parent widget = fst <$> runWidgetInNode parent widget

-- | Runs a builder in `document.body` and discards cleanup action.
runMainBuilderInBody :: forall a. Builder Unit a -> Effect a
runMainBuilderInBody widget = do
  body <- documentBody
  runMainWidgetInNode body widget
