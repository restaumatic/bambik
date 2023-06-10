module Web
  ( Component(..)
  , OnPath
  , makeComponent
  , buildMainComponent
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, class Tagged)
import Data.Invariant.Optics (Path, remainingPath, pathTail)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Specular.Dom.Browser (appendChild, createCommentNode)
import Specular.Dom.Builder (Builder, getEnv, runMainBuilderInBody)

newtype Component :: Type -> Type
newtype Component a = Component
  { builder :: (OnPath a -> Effect Unit) -> Builder Unit (OnPath a -> Effect Unit)
  , tag :: Path
  }

derive instance Newtype (Component a) _

newtype OnPath a = OnPath
  { value :: a
  , path :: Path}

derive instance Functor OnPath

instance Invariant Component where
  invmap pre post c = wrap
    { builder: \callback -> do
      f <- (unwrap c).builder $ callback <<< map pre
      pure $ f <<< map post
    , tag: (unwrap c).tag
    }

instance Cartesian Component where
  invfirst c = wrap
    { builder: \abcallback -> do
      bref <- liftEffect $ Ref.new Nothing
      update <- (unwrap c).builder \(OnPath {path, value: a}) -> do
        mb <- liftEffect $ Ref.read bref
        maybe (pure unit) (\b -> abcallback (OnPath { path, value: Tuple a b})) mb
      pure $ \(OnPath { path, value: ab}) -> do
        Ref.write (Just (snd ab)) bref
        update $ OnPath {path, value: fst ab}
    , tag: (unwrap c).tag
    }
  invsecond c = wrap
    { builder: \abcallback -> do
      aref <- liftEffect $ Ref.new Nothing
      update <- (unwrap c).builder \(OnPath {path, value: b}) -> do
        ma <- liftEffect $ Ref.read aref
        maybe (pure unit) (\a -> abcallback (OnPath {path, value: Tuple a b})) ma
      pure $ \(OnPath { path, value: ab }) -> do
        Ref.write (Just (fst ab)) aref
        update $ OnPath { path, value: snd ab }
    , tag: (unwrap c).tag
    }

instance CoCartesian Component where
  invleft c = wrap
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure case _ of
          OnPath {path, value: Left a} -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrap c).builder (abcallback <<< map Left)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update $ OnPath { path, value: a}
          OnPath _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrap c).tag
    }
  invright c = wrap
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure case _ of
          OnPath {path, value: Right b} -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrap c).builder (abcallback <<< map Right)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update $ OnPath { path, value: b}
          _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrap c).tag
    }

instance Tagged Path Component where
  getTag component = (unwrap component).tag
  setTag tag (Component { builder } ) = Component {builder, tag}

instance Plus Component where
  plus c1 c2 = wrap
    { builder: \callback -> do
      -- TODO how to get rid of this ref?
      mUpdate2Ref <- liftEffect $ Ref.new Nothing
      unless (null (unwrap (unwrap c1).tag)) $ addComment $ "bambik > " <> show (unwrap c1).tag
      update1 <- (unwrap c1).builder $ (\(OnPath { path, value })  -> do
        mUpdate2 <- Ref.read mUpdate2Ref
        case mUpdate2 of
          Just update2 -> maybe (pure unit) (\newPath -> update2 (OnPath { path: newPath, value } )) (remainingPath (unwrap c2).tag path)
          Nothing -> pure unit) <> (\(OnPath { path, value }) -> do
            let newPath = (unwrap c1).tag <> path
            -- log $ "path: " <> show newPath
            callback $ OnPath { path: newPath, value })
      unless (null (unwrap (unwrap c1).tag)) $ addComment $ "bambik < " <> show (unwrap c1).tag
      unless (null (unwrap (unwrap c2).tag)) $ addComment $ "bambik > " <> show (unwrap c2).tag
      update2 <- (unwrap c2).builder $ (\(OnPath { path, value }) -> maybe (pure unit) (\newPath -> update1 (OnPath { path: newPath, value } )) (remainingPath (unwrap c1).tag path)) <> (\(OnPath { path, value }) -> do
            let newPath = (unwrap c2).tag <> path
            -- log $ "path: " <> show newPath
            callback $ OnPath { path: newPath, value })
      unless (null (unwrap (unwrap c2).tag)) $ addComment $ "bambik < " <> show (unwrap c2).tag
      liftEffect $ Ref.write (Just update2) mUpdate2Ref
      pure \(OnPath { path, value }) -> do
        maybe (pure unit) (\newPath -> update1 (OnPath { path: newPath, value } )) (remainingPath (unwrap c1).tag path)
        maybe (pure unit) (\newPath -> update2 (OnPath { path: newPath, value } )) (remainingPath (unwrap c2).tag path)
    , tag: mempty
    }
  zero = wrap
    { builder: mempty
    , tag: mempty
    }

makeComponent :: forall a . ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> Component a
makeComponent builder = Component
  { builder: \callback -> do
      update <- builder \value -> callback $ OnPath { path: mempty, value }
      pure \(OnPath { value }) -> update value
  , tag: mempty}

buildComponent :: forall a. Component a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
buildComponent component callback = do
  update <- (unwrap component).builder \(OnPath { path, value }) -> do
    log $ intercalate "." (unwrap path)
    callback value
  pure \value -> update (OnPath {path: wrap [], value })

buildMainComponent ∷ ∀ (a ∷ Type). Component a → Effect (a → Effect Unit)
buildMainComponent app = runMainBuilderInBody $ buildComponent app mempty

addComment :: forall a. String -> Builder a Unit
addComment comment = do
  env <- getEnv
  placeholderBefore <- liftEffect $ createCommentNode comment
  liftEffect $ appendChild placeholderBefore env.parent

-- static' :: forall a . Component Void -> Component a
-- static' (Component widget) = Component \_ -> do
--     _ <- widget absurd
--     pure mempty

-- sta :: forall a s . a -> Component a -> Component s
-- sta a c = wrap \callback -> do
--   update <- unwrap c mempty
--   liftEffect $ update a
--   pure mempty
