module Web
  ( Component
  -- component construtors
  , component
  -- component polymorphic transformers
  , inside
  , inside'
  -- component runners
  , runComponent
  , runMainComponent
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant)
import Data.Invariant.Optics.Tagged (class Tagged, Path, UserInput, prefixingPaths, propagatedDown, propagatedUp, userInput, userInputValue)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Plus (class Plus)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Specular.Dom.Browser (Attrs, Node, TagName)
import Specular.Dom.Builder (Builder, runMainBuilderInBody)
import Specular.Dom.Builder.Class (comment, elAttr)

newtype Component :: Type -> Type
newtype Component a = Component
  { builder :: (UserInput a -> Effect Unit) -> Builder Unit (UserInput a -> Effect Unit)
  , tag :: Path
  }

-- intentionally not making `instance Newtype Component _` in order to conceal `Component` constructor
-- instead using these private functions for (un)wrapping
wrapC :: forall a.
  { builder :: (UserInput a -> Effect Unit) -> Builder Unit (UserInput a -> Effect Unit)
  , tag :: Path
  }
  -> Component a
wrapC a = Component a

unwrapC :: forall a.
  Component a
  -> { builder :: (UserInput a -> Effect Unit) -> Builder Unit (UserInput a -> Effect Unit)
     , tag :: Path
     }
unwrapC (Component a) = a

instance Invariant Component where
  invmap pre post c = wrapC
    { builder: \callback -> do
      f <- (unwrapC c).builder $ callback <<< map pre
      pure $ f <<< map post
    , tag: (unwrapC c).tag
    }

instance Cartesian Component where
  invfirst c = wrapC
    { builder: \abcallback -> do
      bref <- liftEffect $ Ref.new Nothing
      update <- (unwrapC c).builder \userInput -> do
        mb <- liftEffect $ Ref.read bref
        maybe (pure unit) (\b -> abcallback (userInput <#> \a -> Tuple a b)) mb
      pure $ \userInput -> do
        Ref.write (Just (snd (userInputValue userInput))) bref
        update $ userInput <#> fst
    , tag: (unwrapC c).tag
    }
  invsecond c = wrapC
    { builder: \abcallback -> do
      aref <- liftEffect $ Ref.new Nothing
      update <- (unwrapC c).builder \userInput -> do
        ma <- liftEffect $ Ref.read aref
        maybe (pure unit) (\a -> abcallback (userInput <#> \b -> Tuple a b)) ma
      pure $ \userInput -> do
        Ref.write (Just (fst (userInputValue userInput))) aref
        update $ userInput <#> snd
    , tag: (unwrapC c).tag
    }

instance CoCartesian Component where
  invleft c = wrapC
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure \userInput -> case userInputValue userInput of
          Left a -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrapC c).builder (abcallback <<< map Left)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update $ userInput $> a
          _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrapC c).tag
    }
  invright c = wrapC
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure \userInput -> case userInputValue userInput of
          Right b -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrapC c).builder (abcallback <<< map Right)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update $ userInput $> b
          _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrapC c).tag
    }

instance Tagged Component where
  getPath c = (unwrapC c).tag
  setPath tag (Component { builder } ) = Component {builder, tag}

instance Plus Component where
  plus c1 c2 = wrapC
    { builder: \callback -> do
      -- optimization: we already know that it's redundant to propagade change between children unless
      -- one child's path is a prefix of the other child's path,
      -- in particular, when their paths are the same.
      let propagationBetweenChildrenNecessary = prefixingPaths (unwrapC c1).tag (unwrapC c2).tag
      -- TODO how to get rid of this ref?
      mUpdate2Ref <- liftEffect $ Ref.new Nothing
      addComponentPathOpeningComment c1
      update1 <- (unwrapC c1).builder \op -> do
        mUpdate2 <- Ref.read mUpdate2Ref
        let update2 = maybe mempty identity mUpdate2
        propagateToSiblingAndParent propagationBetweenChildrenNecessary (unwrapC c1).tag (unwrapC c2).tag update2 callback op
      addComponentPathClosingComment c1
      addComponentPathOpeningComment c2
      update2 <- (unwrapC c2).builder $ propagateToSiblingAndParent propagationBetweenChildrenNecessary (unwrapC c2).tag (unwrapC c1).tag update1 callback
      liftEffect $ Ref.write (Just update2) mUpdate2Ref
      addComponentPathClosingComment c2
      pure $ propagateToChild (unwrapC c1).tag update1 <> propagateToChild (unwrapC c2).tag update2
    , tag: mempty
    }
    where
      addComponentPathOpeningComment = addComponentPathTextComment "path "
      addComponentPathClosingComment = addComponentPathTextComment "path /"
      addComponentPathTextComment text c = unless (null (unwrap (unwrapC c).tag)) $ comment $ text <> show (unwrapC c).tag
      propagateToSiblingAndParent :: forall a . Boolean -> Path -> Path -> (UserInput a -> Effect Unit) -> (UserInput a -> Effect Unit) -> UserInput a -> Effect Unit
      propagateToSiblingAndParent siblingPropagationGuard childPath siblingPath updateSibling updateParent userInput = do
        let userInputOnParent = propagatedUp childPath userInput
        when siblingPropagationGuard $ maybe mempty updateSibling (propagatedDown siblingPath userInputOnParent)
        updateParent userInputOnParent
      propagateToChild :: forall a . Path -> (UserInput a -> Effect Unit) -> UserInput a -> Effect Unit
      propagateToChild childPath updateChild userInput = maybe mempty updateChild (propagatedDown childPath userInput)
  zero = wrapC
    { builder: mempty
    , tag: mempty
    }

-- Component constructors

component :: forall a . ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> Component a
component builder = Component
  { builder: \callback -> do
      update <- builder $ callback <<< userInput
      pure $ update <<< userInputValue
  , tag: mempty}

-- Component polymorhphic combinators (wrappers)

inside :: forall a . TagName -> Component a -> Component a
inside tagName = inside' tagName mempty mempty

inside' :: forall a . TagName -> (Unit -> Attrs) -> (Node -> (UserInput a -> Effect Unit) -> Effect Unit) -> Component a -> Component a
inside' tagName attrs event c = Component
  { builder: \callback -> do
    Tuple node f <- elAttr tagName (attrs unit) $ (unwrapC c).builder callback
    liftEffect $ event node callback
    pure \a -> do
      f a
  , tag: (unwrapC c).tag
  }

-- Component runners

runComponent :: forall a. Component a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
runComponent c callback = do
  update <- (unwrapC c).builder $ callback <<< userInputValue
  pure $ update <<< userInput

runMainComponent :: forall a. Component a -> Effect (a -> Effect Unit)
runMainComponent app = runMainBuilderInBody $ runComponent app mempty
