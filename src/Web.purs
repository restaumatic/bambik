module Web
  ( Component(..)
  , Hop
  , OnPath(..)
  , Path
  , Tag(..)
  , buildMainComponent
  , withoutTag
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Array (filter, null)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, class Tagged)
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
  , tag :: Tag
  }

derive instance Newtype (Component a) _

withoutTag :: forall a . ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> Component a
withoutTag builder' = Component
  { builder: \callback -> do
      update <- builder' \value -> callback $ OnPath { path: [], value }
      pure \(OnPath { value }) -> update value
  , tag: emptyTag}

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

newtype Tag = Tag
  { hops :: Array Hop
  , subtags :: Array Tag
  }

derive instance Newtype Tag _
derive instance Eq Tag

type Path = Array Hop

newtype OnPath a = OnPath
  { value :: a
  , path :: Path}

derive instance Functor OnPath

instance Show Tag where
  show (Tag {hops, subtags}) = (intercalate "." hops) -- <> (if null subtags then "" else ("(" <> intercalate "," (show <$> subtags) <> ")"))

emptyTag :: Tag
emptyTag = Tag {hops: [], subtags: [] }

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
      unless (null (unwrap (unwrap c1).tag).hops) $ addComment $ "bambik > " <> show (unwrap c1).tag
      update1 <- (unwrap c1).builder $ (\a -> do
        mUpdate2 <- Ref.read mUpdate2Ref
        case mUpdate2 of
          Just update2 -> update2 a
          Nothing -> pure unit) <> (\(OnPath { path, value }) -> do
            let newPath = (unwrap (unwrap c1).tag).hops <> path
            -- log $ "path: " <> show newPath
            callback $ OnPath { path: newPath, value })
      unless (null (unwrap (unwrap c1).tag).hops) $ addComment $ "bambik < " <> show (unwrap c1).tag
      unless (null (unwrap (unwrap c2).tag).hops) $ addComment $ "bambik > " <> show (unwrap c2).tag
      update2 <- (unwrap c2).builder $ update1 <> (\(OnPath { path, value }) -> do
            let newPath = (unwrap (unwrap c2).tag).hops <> path
            -- log $ "path: " <> show newPath
            callback $ OnPath { path: newPath, value })
      unless (null (unwrap (unwrap c2).tag).hops) $ addComment $ "bambik < " <> show (unwrap c2).tag
      liftEffect $ Ref.write (Just update2) mUpdate2Ref
      pure \i -> do
        update1 i
        update2 i
    , tag: Tag { hops: [], subtags: filter (_ /= emptyTag) [(unwrap c1).tag, (unwrap c2).tag]}
    }
  zero = Component
    { builder: mempty
    , tag: emptyTag
    }

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


-- noChoiceComponent :: forall a. Component a
-- noChoiceComponent = withoutTag $ pure mempty

buildComponent :: forall a. Component a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
buildComponent component callback = do
  update <- (unwrap component).builder \(OnPath { path, value }) -> do
    log $ intercalate "." path
    callback value
  pure \value -> update (OnPath {path: [], value })

buildMainComponent ∷ ∀ (a ∷ Type). Component a → Effect (a → Effect Unit)
buildMainComponent app = runMainBuilderInBody $ buildComponent app mempty
