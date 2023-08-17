module Web
  ( WebComponent
  , WebComponentWrapper
  , checkbox
  , div
  , div'
  , dynamic
  , inside
  , inside'
  , label
  , label'
  , onClick
  , radio
  , runComponent
  , runMainComponent
  , span
  , span'
  , text
  , textInput
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Either (Either(..))
import Data.Invariant.Transformers.Scoped (Part(..), Scoped(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Plus (class ProPlus, class ProPlusoid)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Optics (class ProCartesian, class ProCocartesian)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Specular.Dom.Browser (Attrs, Node, TagName, onDomEvent, (:=))
import Specular.Dom.Builder (Builder, runMainBuilderInBody)
import Specular.Dom.Builder.Class (elAttr)
import Specular.Dom.Builder.Class as S

type WebComponentWrapper i o = WebComponent (Scoped i) (Scoped o)

newtype WebComponent i o = WebComponent ((o -> Effect Unit) -> Builder Unit (i -> Effect Unit))

-- derive instance Newtype (WebComponent a) _

wrap :: forall i o. ((o -> Effect Unit) -> Builder Unit (i -> Effect Unit)) -> WebComponent i o
wrap = WebComponent

unwrap :: forall i o. WebComponent i o -> (o -> Effect Unit) -> Builder Unit (i -> Effect Unit)
unwrap (WebComponent c) = c

dynamic :: forall a b. (a -> WebComponent b b) -> WebComponentWrapper a b -- WebComponentWrapper == DynamicWebComponent (with scope?)
dynamic f = wrapWebComponent \_ -> do
  slot <- newSlot
  pure $ \a -> replaceSlot slot $ void $ unwrap (f a) mempty


instance Profunctor WebComponent where
  dimap post pre c = wrap \callback -> do
    f <- unwrap c $ callback <<< pre
    pure $ f <<< post

instance ProCartesian WebComponent where
  profirst c = wrap \abcallback -> do
    bref <- liftEffect $ Ref.new Nothing
    update <- unwrap c \a -> do
      mb <- liftEffect $ Ref.read bref
      maybe mempty (\b -> abcallback $ Tuple a b) mb
    pure $ \ab -> do
      Ref.write (Just (snd ab)) bref
      update $ fst ab
  prosecond c = wrap \abcallback -> do
    aref <- liftEffect $ Ref.new Nothing
    update <- unwrap c \b -> do
      ma <- liftEffect $ Ref.read aref
      maybe mempty (\a -> abcallback $ Tuple a b) ma
    pure $ \ab -> do
      Ref.write (Just (fst ab)) aref
      update $ snd ab

instance ProCocartesian WebComponent where
  proleft c = wrap \abcallback -> do
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
        void $ liftEffect $ replaceSlot slot $ pure unit
        Ref.write Nothing mUpdateRef
        -- interestingly, theoretically, here we could call:
        -- abcallback userInput
        -- I don't know whether it would be right, though.
        -- Is that stil relevant question?
  proright c = wrap \abcallback -> do
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
        void $ liftEffect $ replaceSlot slot $ pure unit
        Ref.write Nothing mUpdateRef
        -- interestingly, theoretically, here we could call:
        -- abcallback userInput
        -- I don't know whether it would be right, though.
        -- Is that stil relevant question?

instance ProPlusoid WebComponent where
  proplus c1 c2 = wrap \updateParent -> do
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
  proplusfirst c1 c2 = wrap \updateParent -> do
    update1 <- unwrap c1 updateParent
    update2 <- unwrap c2 mempty
    pure \a -> do
      update1 a
      update2 a
  proplussecond c1 c2 = wrap \updateParent -> do
    update1 <- unwrap c1 mempty
    update2 <- unwrap c2 updateParent
    pure \a -> do
      update1 a
      update2 a

instance ProPlus WebComponent where
  prozero = wrap mempty

--

wrapWebComponent :: forall i o. ((o -> Effect Unit) -> Builder Unit (i -> Effect Unit)) -> WebComponentWrapper i o
wrapWebComponent c = wrap \callback -> do
  update <- c \a -> callback (Scoped MoreThanOnePart a)
  pure \(Scoped scope a) -> do
    case scope of
      NoPart -> do
        pure unit
      _ -> update a

-- WebUI polymorhphic combinators

inside :: forall a b. TagName -> WebComponent a b -> WebComponent a b
inside tagName = inside' tagName mempty mempty

inside' :: forall a b. TagName -> (Unit -> Attrs) -> (Node -> (b -> Effect Unit) -> Effect Unit) -> WebComponent a b -> WebComponent a b
inside' tagName attrs event c = wrap \callback -> do
    Tuple node f <- elAttr tagName (attrs unit) $ unwrap c callback
    liftEffect $ event node callback
    pure \a -> do
      f a

text :: forall a b. String -> WebComponent a b
text s = wrap \_ -> do
  S.text s
  pure $ mempty

div :: forall a b. WebComponent a b -> WebComponent a b
div = inside "div"

div' :: forall a b. (Unit -> Attrs) -> (Node -> (b -> Effect Unit) -> Effect Unit) -> WebComponent a b -> WebComponent a b
div' = inside' "div"

span :: forall a b. WebComponent a b -> WebComponent a b
span = inside "span"

span' :: forall a b. (Unit -> Attrs) -> (Node -> (b -> Effect Unit) -> Effect Unit) -> WebComponent a b -> WebComponent a b
span' = inside' "span"

label :: forall a b. WebComponent a b -> WebComponent a b
label = inside "label"

label' :: forall a b. (Unit -> Attrs) -> (Node -> (b -> Effect Unit) -> Effect Unit) -> WebComponent a b -> WebComponent a b
label' = inside' "label"


textInput :: Attrs -> WebComponentWrapper String String
textInput attrs = wrapWebComponent \callback -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node $ const $ getValue node >>= callback
  pure $ setValue node

checkbox :: Attrs -> WebComponentWrapper Boolean Boolean
checkbox attrs = wrapWebComponent \callback -> do
  Tuple node _ <- elAttr "input" ("type" := "checkbox" <> attrs) (pure unit)
  onDomEvent "input" node $ const $ getChecked node >>= callback
  pure $ setChecked node

radio :: Attrs -> WebComponentWrapper Boolean Unit
radio attrs = wrapWebComponent \callback -> do
  Tuple node _ <- elAttr "input" ("type" := "radio" <> attrs) (pure unit)
  onDomEvent "change" node $ const $ callback unit
  pure $ setChecked node

-- TODO
onClick ∷ forall a. Node → (a -> Effect Unit) -> Effect Unit
onClick _ _ = mempty -- void $ DOM.addEventListener "click" (\_ -> callback a) node
-- WebUI runners

runComponent :: forall i o. WebComponentWrapper i o -> Builder Unit (i -> Effect Unit)
runComponent c = do
  update <- (unwrap c) \(Scoped scope _) -> log $ "change in scope: " <> show scope
  pure $ \a -> update (Scoped MoreThanOnePart a)

runMainComponent :: forall i o. WebComponentWrapper i o -> i -> Effect Unit
runMainComponent c i = do
  update <- runMainBuilderInBody $ runComponent c
  update i

foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
