module Web
  ( WebComponent
  , WebComponentWrapper
  , div
  , div'
  , inside
  , inside'
  , label
  , label'
  , runComponent
  , runMainComponent
  , span
  , span'
  , static
  , wrapWebComponent
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Either (Either(..))
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant)
import Data.Invariant.Transformers.Scoped (Scope(..), Scoped(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Plus (class Plus, class Plusoid)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Specular.Dom.Browser (Attrs, Node, TagName)
import Specular.Dom.Builder (Builder, runMainBuilderInBody)
import Specular.Dom.Builder.Class (elAttr)

type WebComponentWrapper a = WebComponent (Scoped a)

newtype WebComponent a = WebComponent ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit))

-- derive instance Newtype (WebComponent a) _

wrap :: forall a. ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> WebComponent a
wrap = WebComponent

unwrap :: forall a. WebComponent a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
unwrap (WebComponent c) = c

static :: forall a b. a -> WebComponentWrapper a -> WebComponentWrapper b
static a c = wrap \_ -> do
    update <- unwrap c mempty
    liftEffect $ update (Scoped All a)
    pure $ mempty

instance Invariant WebComponent where
  invmap pre post c = wrap \callback -> do
    f <- unwrap c $ callback <<< pre
    pure $ f <<< post

instance Cartesian WebComponent where
  invfirst c = wrap \abcallback -> do
    bref <- liftEffect $ Ref.new Nothing
    update <- unwrap c \a -> do
      mb <- liftEffect $ Ref.read bref
      maybe mempty (\b -> abcallback $ Tuple a b) mb
    pure $ \ab -> do
      Ref.write (Just (snd ab)) bref
      update $ fst ab
  invsecond c = wrap \abcallback -> do
    aref <- liftEffect $ Ref.new Nothing
    update <- unwrap c \b -> do
      ma <- liftEffect $ Ref.read aref
      maybe mempty (\a -> abcallback $ Tuple a b) ma
    pure $ \ab -> do
      Ref.write (Just (fst ab)) aref
      update $ snd ab

instance CoCartesian WebComponent where
  invleft c = wrap \abcallback -> do
    slot <- newSlot
    mUpdateRef <- liftEffect $ Ref.new Nothing
    pure \aorb -> case aorb of
      Left a -> do
        mUpdate <- liftEffect $ Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> do
            newUpdate <- liftEffect $ replaceSlot slot $ unwrap c (abcallback <<< Left)
            liftEffect $ Ref.write (Just newUpdate) mUpdateRef
            pure newUpdate
        update $ a
      _ -> do
        liftEffect $ destroySlot slot
        Ref.write Nothing mUpdateRef
        -- interestingly, theoretically, here we could call:
        -- abcallback userInput
        -- I don't know whether it would be right, though
        -- is that stil relevant question?
        pure unit
  invright c = wrap \abcallback -> do
    slot <- newSlot
    mUpdateRef <- liftEffect $ Ref.new Nothing
    pure \aorb -> case aorb of
      Right b -> do
        mUpdate <- liftEffect $ Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> do
            newUpdate <- liftEffect $ replaceSlot slot $ unwrap c (abcallback <<< Right)
            liftEffect $ Ref.write (Just newUpdate) mUpdateRef
            pure newUpdate
        update $ b
      _ -> do
        liftEffect $ destroySlot slot
        Ref.write Nothing mUpdateRef
        -- interestingly, theoretically, here we could call:
        -- abcallback userInput
        -- I don't know whether it would be right, though
        -- is that stil relevant question?
        pure unit

instance Plusoid WebComponent where
  plus c1 c2 = wrap \updateParent -> do
    -- TODO how to get rid of this ref?
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    update1 <- unwrap c1 \a -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      update2 a
      updateParent a
    update2 <- unwrap c2 \a -> do
      update1 a
      updateParent a
    liftEffect $ Ref.write (Just update2) mUpdate2Ref
    pure \a -> do
      update1 a
      update2 a

instance Plus WebComponent where
  pzero = wrap mempty

--

wrapWebComponent :: forall a. ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> WebComponentWrapper a
wrapWebComponent c = wrap \callback -> do
  update <- c (callback <<< Scoped All)
  pure case _ of
    (Scoped None a) -> pure unit
    (Scoped _ a) -> update a

-- WebUI polymorhphic combinators

inside :: forall a . TagName -> WebComponent a -> WebComponent a
inside tagName = inside' tagName mempty mempty

inside' :: forall a . TagName -> (Unit -> Attrs) -> (Node -> (a -> Effect Unit) -> Effect Unit) -> WebComponent a -> WebComponent a
inside' tagName attrs event c = wrap \callback -> do
    Tuple node f <- elAttr tagName (attrs unit) $ unwrap c callback
    liftEffect $ event node callback
    pure \a -> do
      f a

div :: forall a. WebComponent a -> WebComponent a
div = inside "div"

div' :: forall a. (Unit -> Attrs) -> (Node -> (a -> Effect Unit) -> Effect Unit) -> WebComponent a -> WebComponent a
div' = inside' "div"

span :: forall a. WebComponent a -> WebComponent a
span = inside "span"

span' :: forall a. (Unit -> Attrs) -> (Node -> (a -> Effect Unit) -> Effect Unit) -> WebComponent a -> WebComponent a
span' = inside' "span"

label :: forall a. WebComponent a -> WebComponent a
label = inside "label"

label' :: forall a. (Unit -> Attrs) -> (Node -> (a -> Effect Unit) -> Effect Unit) -> WebComponent a -> WebComponent a
label' = inside' "label"


-- WebUI runners

runComponent :: forall a. WebComponentWrapper (a) -> Builder Unit (a -> Effect Unit)
runComponent c = do
  update <- (unwrap c) \(Scoped scope _) -> log $ "change in scope: " <> show scope
  pure $ \a -> update (Scoped All a)

runMainComponent :: forall a. WebComponentWrapper (a) -> Effect (a -> Effect Unit)
runMainComponent = runMainBuilderInBody <<< runComponent
