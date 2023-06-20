module Specular.Dom.Builder.Class
  ( BuilderEnv
  -- , domEvent
  -- , domEventWithSample
  -- , dynRawHtml
  , el
  -- , el'
  -- , elAttr'
  , elAttr_
  -- , elDynAttr
  -- , elDynAttr'
  , el_
  -- , onDomEvent
  , class MonadDomBuilder
  , text
  -- , dynText
  -- , elDynAttrNS'
  , rawHtml
  , elAttr
  , liftBuilder
  , liftBuilderWithRun
  , comment
  )
  where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Tuple (Tuple)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn2)
import Specular.Dom.Browser (Attrs, Node, TagName)
import Specular.Internal.Effect (DelayedEffects)

type BuilderEnv env =
  { parent :: Node
  , cleanup :: DelayedEffects
  , userEnv :: env
  }

class Monad m <= MonadDomBuilder m where
  text :: String -> m Unit
  -- dynText :: WeakDynamic String -> m Unit
  -- elDynAttrNS' :: forall a. Maybe Namespace -> TagName -> WeakDynamic Attrs -> m a -> m (Tuple Node a)
  rawHtml :: String -> m Unit

  elAttr :: forall a. TagName -> Attrs -> m a -> m (Tuple Node a)

  liftBuilder :: forall a. (forall env. EffectFn1 (BuilderEnv env) a) -> m a
  liftBuilderWithRun :: forall a b. (forall env. EffectFn2 (BuilderEnv env) (EffectFn2 (BuilderEnv env) (m b) b) a) -> m a

-- elDynAttr' :: forall m a. MonadDomBuilder m => String -> WeakDynamic Attrs -> m a -> m (Tuple Node a)
-- elDynAttr' = elDynAttrNS' Nothing

-- elDynAttr
--   :: forall m a
--    . MonadDomBuilder m
--   => String
--   -> WeakDynamic Attrs
--   -> m a
--   -> m a
-- elDynAttr tagName dynAttrs inner = snd <$> elDynAttr' tagName dynAttrs inner

-- elAttr' :: forall m a. MonadDomBuilder m => String -> Attrs -> m a -> m (Tuple Node a)
-- elAttr' tagName attrs inner = elDynAttr' tagName (pure attrs) inner

  comment :: String -> m Unit

elAttr_ :: forall m. MonadDomBuilder m => String -> Attrs -> m (Tuple Node Unit)
elAttr_ tagName attrs = elAttr tagName attrs (pure unit)

-- el' :: forall m a. MonadDomBuilder m => String -> m a -> m (Tuple Node a)
-- el' tagName inner = elAttr' tagName mempty inner

el :: forall m a. MonadDomBuilder m => String -> m a -> m (Tuple Node a)
el tagName inner = elAttr tagName mempty inner

el_ :: forall m. MonadDomBuilder m => String -> m (Tuple Node Unit)
el_ tagName = el tagName (pure unit)

-- dynRawHtml :: forall m. MonadDomBuilder m => MonadReplace m => MonadFRP m => WeakDynamic String -> m Unit
-- dynRawHtml dynHtml = weakDynamic_ (rawHtml <$> dynHtml)

-- domEventWithSample :: forall m a. MonadFRP m => (DOM.Event -> Effect a) -> EventType -> Node -> m (FRP.Event a)
-- domEventWithSample sample eventType node = do
--   { event, fire } <- newEvent
--   onDomEvent eventType node (sample >=> fire)
--   pure event

-- domEvent :: forall m. MonadFRP m => EventType -> Node -> m (FRP.Event Unit)
-- domEvent = domEventWithSample (\_ -> pure unit)

-- | Register a DOM event listener.
-- onDomEvent :: forall m. MonadFRP m => EventType -> Node -> (DOM.Event -> Effect Unit) -> m Unit
-- onDomEvent eventType node handler = do
--   unsub <- liftEffect $ addEventListener eventType handler node
--   onCleanup unsub

instance monadDomBuilderReaderT :: MonadDomBuilder m => MonadDomBuilder (ReaderT r m) where
  text = lift <<< text
  -- dynText = lift <<< dynText
  -- elDynAttrNS' ns tag attrs body = ReaderT $ \env -> elDynAttrNS' ns tag attrs $ runReaderT body env
  rawHtml = lift <<< rawHtml
  elAttr tag attrs body =
    ReaderT $ \env -> elAttr tag attrs $ runReaderT body env
  liftBuilder b = lift (liftBuilder b)
  liftBuilderWithRun fn = ReaderT \e ->
    liftBuilderWithRun
      ( mkEffectFn2 \benv run ->
          runEffectFn2 fn benv
            ( mkEffectFn2 \benv' m ->
                runEffectFn2 run benv' (runReaderT m e)
            )
      )
  comment = lift <<< comment
