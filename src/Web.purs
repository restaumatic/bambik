module Web
  ( Component(..)
  , Tag
  , Hop
  , noChoiceComponent
  , buildComponent
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Either (Either(..))
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, class Tagged)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Specular.Dom.Browser (appendChild, createCommentNode)
import Specular.Dom.Builder (Builder, getEnv)

newtype Component :: Type -> Type
newtype Component a = Component
  { builder :: (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
  , tag :: Tag
  }

derive instance Newtype (Component a) _

withoutTag :: forall a . ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> Component a
withoutTag builder = Component { builder, tag: mempty}

-- static' :: forall a . Component Void -> Component a
-- static' (Component widget) = Component \_ -> do
--     _ <- widget absurd
--     pure mempty

-- sta :: forall a s . a -> Component a -> Component s
-- sta a c = wrap \callback -> do
--   update <- unwrap c mempty
--   liftEffect $ update a
--   pure mempty


type Hop = String

type Tag = Array (Array Hop)

instance Tagged Tag Component where
  getTag component = (unwrap component).tag
  setTag tag (Component { builder } ) = Component {builder, tag}


addComment :: forall a. String -> Builder a Unit
addComment comment = do
  env <- getEnv
  placeholderBefore <- liftEffect $ createCommentNode comment
  liftEffect $ appendChild placeholderBefore env.parent

instance Plus Component where
  plus c1 c2 = wrap
    { builder: \callback -> do
      -- TODO how to get rid of this ref?
      mUpdate2Ref <- liftEffect $ Ref.new Nothing
      addComment $ "bambik: " <> show (unwrap c1).tag
      update1 <- (unwrap c1).builder $ (\a -> do
        mUpdate2 <- Ref.read mUpdate2Ref
        case mUpdate2 of
          Just update2 -> update2 a
          Nothing -> pure unit) <> callback
      addComment $ "bambik: " <> show (unwrap c2).tag
      update2 <- (unwrap c2).builder $ update1 <> callback
      liftEffect $ Ref.write (Just update2) mUpdate2Ref
      pure \i -> do
        update1 i
        update2 i
    , tag: (unwrap c1).tag <> (unwrap c2).tag
    }
  zero = Component
    { builder: mempty
    , tag: mempty
    }

instance Invariant Component where
  invmap pre post c = wrap
    { builder: \callback -> do
      f <- (unwrap c).builder $ callback <<< pre
      pure $ f <<< post
    , tag: (unwrap c).tag
    }

instance Cartesian Component where
  invfirst c = wrap
    { builder: \abcallback -> do
      bref <- liftEffect $ Ref.new Nothing
      update <- (unwrap c).builder \a -> do
        mb <- liftEffect $ Ref.read bref
        maybe (pure unit) (\b -> abcallback (Tuple a b)) mb
      pure $ \ab -> do
        Ref.write (Just (snd ab)) bref
        update (fst ab)
    , tag: (unwrap c).tag
    }
  invsecond c = wrap
    { builder: \abcallback -> do
      aref <- liftEffect $ Ref.new Nothing
      update <- (unwrap c).builder \b -> do
        ma <- liftEffect $ Ref.read aref
        maybe (pure unit) (\a -> abcallback (Tuple a b)) ma
      pure $ \ab -> do
        Ref.write (Just (fst ab)) aref
        update (snd ab)
    , tag: (unwrap c).tag
    }

instance CoCartesian Component where
  invleft c = wrap
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure case _ of
          Left a -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrap c).builder (abcallback <<< Left)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update a
          Right _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrap c).tag
    }
  invright c = wrap
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure case _ of
          Right b -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrap c).builder (abcallback <<< Right)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update b
          Left _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrap c).tag
    }


noChoiceComponent :: forall a. Component a
noChoiceComponent = withoutTag $ pure mempty

buildComponent :: forall a. Component a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
buildComponent component = (unwrap component).builder
