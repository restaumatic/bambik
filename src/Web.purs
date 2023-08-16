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
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant)
import Data.Invariant.Transformers.Scoped (Part(..), Scoped(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Plus (class Plus, class Plusoid, pzero)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Specular.Dom.Browser (Attrs, Node, TagName, onDomEvent, (:=))
import Specular.Dom.Builder (Builder, runMainBuilderInBody)
import Specular.Dom.Builder.Class (elAttr)
import Specular.Dom.Builder.Class as S

type WebComponentWrapper a = WebComponent (Scoped a)

newtype WebComponent a = WebComponent ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit))

-- derive instance Newtype (WebComponent a) _

wrap :: forall a. ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> WebComponent a
wrap = WebComponent

unwrap :: forall a. WebComponent a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
unwrap (WebComponent c) = c

dynamic :: forall a b. (a -> WebComponent b) -> WebComponentWrapper a -- WebComponentWrapper == DynamicWebComponent (with scope?)
dynamic f = wrapWebComponent \_ -> do
  slot <- newSlot
  pure $ \a -> replaceSlot slot $ void $ unwrap (f a) mempty


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
  update <- c \a -> do
    let scope = MoreThanOnePart
    log $ "calling back scope" <> show scope
    callback (Scoped scope a)
  pure \(Scoped scope a) -> do
    log $ "updating scope" <> show scope
    case scope of
      NoPart -> do
        pure unit
      _ -> update a

-- WebUI polymorhphic combinators

inside :: forall a . TagName -> WebComponent a -> WebComponent a
inside tagName = inside' tagName mempty mempty

inside' :: forall a . TagName -> (Unit -> Attrs) -> (Node -> (a -> Effect Unit) -> Effect Unit) -> WebComponent a -> WebComponent a
inside' tagName attrs event c = wrap \callback -> do
    Tuple node f <- elAttr tagName (attrs unit) $ unwrap c callback
    liftEffect $ event node callback
    pure \a -> do
      f a

text :: forall a. String -> WebComponent a
text s = wrap \_ -> do
  S.text s
  pure $ mempty

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

foreign import getTextInputValue :: Node -> Effect String
foreign import setTextInputValue :: Node -> String -> Effect Unit
foreign import getCheckboxChecked :: Node -> Effect Boolean
foreign import setCheckboxChecked :: Node -> Boolean -> Effect Unit

textInput :: Attrs -> WebComponentWrapper String
textInput attrs = wrapWebComponent \callback -> do
  Tuple node a <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node \event -> do
    getTextInputValue node >>= callback
  pure $ setTextInputValue node

checkbox :: Attrs -> WebComponentWrapper Boolean
checkbox attrs = wrapWebComponent \callback -> do
  Tuple node a <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node \event -> do
    getCheckboxChecked node >>= callback
  pure $ setCheckboxChecked node

-- TODO
radio :: (Boolean -> Attrs) -> WebComponent Boolean
radio attrs = pzero # inside' "input" (\_ -> let enabled = false in ("type" := "radio") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \node callback -> do
  mempty
  -- setCheckboxChecked node value
  -- onDomEvent "change" node (\_ -> getCheckboxChecked node >>= callback)
-- radio :: forall a b f. Applicative f => (a -> Attrs) -> ComponentWrapper f a b
-- radio attrs = pzero # (inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \_ node -> do
--   domEventWithSample (\_ -> getCheckboxChecked node <#> \value -> { path: [], value }) "change" node)

-- TODO
onClick ∷ forall a. Node → (a -> Effect Unit) -> Effect Unit
onClick node callback = mempty -- void $ DOM.addEventListener "click" (\_ -> callback a) node
-- WebUI runners

runComponent :: forall a. WebComponentWrapper (a) -> Builder Unit (a -> Effect Unit)
runComponent c = do
  update <- (unwrap c) \(Scoped scope _) -> log $ "change in scope: " <> show scope
  pure $ \a -> update (Scoped MoreThanOnePart a)

runMainComponent :: forall a. WebComponentWrapper (a) -> Effect (a -> Effect Unit)
runMainComponent = runMainBuilderInBody <<< runComponent
