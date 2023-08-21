module Web
  ( Component
  , Listener
  , Widget
  , button
  , button'
  , chars
  , checkbox
  , div
  , div'
  , element
  , element'
  , label
  , label'
  , module Data.Profunctor.Plus
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

import Data.Either (Either(..))
import Data.Invariant.Transformers.Changed (Change(..), Changed(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, proplusfirst, proplussecond, pzero, (<^), (^), (^>))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn2)
import Specular.Dom.Builder (Attrs, Builder, Node, TagName, addEventListener, attr, elAttr, getChecked, getValue, newSlot, onDomEvent, replaceSlot, runMainBuilderInBody, setAttributesImpl, setChecked, setValue)
import Specular.Dom.Builder as Builder

newtype Widget i o = Widget (i -> (o -> Effect Unit) -> Builder Unit (i -> Effect Unit))

instance Profunctor Widget where
  dimap pre post c = Widget \a callback -> do
    f <- unwrapWidget c (pre a) $ callback <<< post
    pure $ f <<< pre

instance Strong Widget where
  first c = Widget \ab abcallback -> do
    bref <- liftEffect $ Ref.new (snd ab)
    update <- unwrapWidget c (fst ab) \a -> do
      b <- liftEffect $ Ref.read bref
      abcallback $ Tuple a b
    pure $ \ab -> do
      Ref.write (snd ab) bref
      update $ fst ab
  second c = Widget \ab abcallback -> do
    aref <- liftEffect $ Ref.new (fst ab)
    update <- unwrapWidget c (snd ab) \b -> do
      a <- liftEffect $ Ref.read aref
      abcallback $ Tuple a b
    pure $ \ab -> do
      Ref.write (fst ab) aref
      update $ snd ab

instance Choice Widget where
  left c = Widget \ab abcallback -> do
    slot <- newSlot
    mUpdateRef <- liftEffect $ Ref.new Nothing
    case ab of
      Left a -> void $ liftEffect $ makeUpdate slot abcallback mUpdateRef a
      Right _ -> pure unit
    pure \aorb -> case aorb of
      Left a -> do
        mUpdate <- Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> makeUpdate slot abcallback mUpdateRef a
        update a
      _ -> do
        void $ replaceSlot slot $ pure unit
        Ref.write Nothing mUpdateRef
      -- doing here instead:
      -- Right b -> do
      --   void $ replaceSlot slot $ pure unit
      --   Ref.write Nothing mUpdateRef
      --   abcallback (Right b)
      -- would type check, yet it would be wrong as we don't allow a component to pass intput though to output
      -- TODO EC make it not type check
      where
        makeUpdate slot abcallback mUpdateRef a = do
          newUpdate <- replaceSlot slot $ unwrapWidget c a (abcallback <<< Left)
          Ref.write (Just newUpdate) mUpdateRef
          pure newUpdate
  right c = Widget \ab abcallback -> do
    slot <- newSlot
    mUpdateRef <- liftEffect $ Ref.new Nothing
    case ab of
      Left _ -> pure unit
      Right b -> void $ liftEffect $ makeUpdate slot abcallback mUpdateRef b
    pure \aorb -> case aorb of
      Right b -> do
        mUpdate <- Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> makeUpdate slot abcallback mUpdateRef b
        update b
      _ -> do
        void $ replaceSlot slot $ pure unit
        Ref.write Nothing mUpdateRef
      -- doing here instead:
      -- Left a -> do
      --   void $ replaceSlot slot $ pure unit
      --   Ref.write Nothing mUpdateRef
      --   abcallback (Left a)
      -- would type check, yet it would be wrong as we don't allow a component to pass intput though to output
      -- TODO EC make it not type check
      where
        makeUpdate slot abcallback mUpdateRef a = do
          newUpdate <- replaceSlot slot $ unwrapWidget c a (abcallback <<< Right)
          Ref.write (Just newUpdate) mUpdateRef
          pure newUpdate

instance ProfunctorPlus Widget where
  proplus c1 c2 = Widget \initial updateParent -> do
    -- TODO how to get rid of this ref?
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    update1 <- unwrapWidget c1 initial \a -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      update2 a
      updateParent a
    update2 <- unwrapWidget c2 initial \a -> do
      update1 a
      updateParent a
    liftEffect $ Ref.write (Just update2) mUpdate2Ref
    pure \a -> do
      update1 a
      update2 a
  proplusfirst c1 c2 = Widget \initial updateParent -> do
    update1 <- unwrapWidget c1 initial updateParent
    update2 <- unwrapWidget c2 initial mempty
    pure \a -> do
      update1 a
      update2 a
  proplussecond c1 c2 = Widget \initial updateParent -> do
    update1 <- unwrapWidget c1 initial mempty
    update2 <- unwrapWidget c2 initial updateParent
    pure \a -> do
      update1 a
      update2 a

instance ProfunctorZero Widget where
  pzero = Widget mempty

-- Widgets

text :: forall a. Widget (Changed String) (Changed a)
text = Widget \str _ -> do
  slot <- newSlot
  let (Changed _ str') = str
  liftEffect $ replaceSlot slot $ Builder.text str'
  pure $ case _ of
    Changed None _ -> pure unit
    Changed _ s -> replaceSlot slot $ Builder.text s
-- TODO: Why this in below causes that no static text is displayed
  -- liftEffect $ update slot str
  -- pure $ update slot
  --   where
  --     update slot = case _ of
  --       Changed None _ -> pure unit
  --       Changed _ s -> replaceSlot slot $ Builder.text s

chars :: forall a b. String -> Widget a b
chars s = Widget \_ _ -> do
  Builder.text s
  pure $ mempty

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> Listener a -> Widget a b -> Widget a b
element tagName attrs dynAttrs listener c = Widget \a callback -> do
    aRef <- liftEffect $ Ref.new a
    Tuple node update <- elAttr tagName attrs $ unwrapWidget c a callback
    liftEffect $ runEffectFn2 setAttributesImpl node (show <$> attrs <> dynAttrs a)
    liftEffect $ listener node (Ref.read aRef)
    pure \a -> do
      Ref.write a aRef
      runEffectFn2 setAttributesImpl node (show <$> attrs <> dynAttrs a)
      update a

element' :: forall a b. TagName -> Widget a b -> Widget a b
element' tagName = element tagName mempty mempty mempty

div' :: forall a b. Widget a b -> Widget a b
div' = element' "div"

div :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
div = element "div"

span' :: forall a b. Widget a b -> Widget a b
span' = element' "span"

span :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
span = element "span"

label' :: forall a b. Widget a b -> Widget a b
label' = element' "label"

label :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
label = element "label"

button :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
button = element "button"

button' :: forall a b. Widget a b -> Widget a b
button' = element' "button"

-- Components
-- ... are Widgets that have same input and output type and carry metadata about the scope of a change in input and output.
-- Components preserve Profunctor, Strong, Choice, ProfunctorPlus and ProfunctorZero instances of Widget.

type Component a = Widget (Changed a) (Changed a)

textInput :: Attrs -> Component String
textInput attrs = component $ widget \a callback -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  liftEffect $ setValue node a
  onDomEvent "input" node $ const $ getValue node >>= callback
  pure $ setValue node

checkbox :: Attrs -> Component Boolean
checkbox attrs = component $ widget \a callback -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
  liftEffect $ setChecked node a
  onDomEvent "input" node $ const $ getChecked node >>= callback
  pure $ setChecked node

-- input:
-- Nothing -> turn off button
-- Just a -> turn on (if turned off) button and remember a
-- output:
-- Nothing -> button was clicked but button doesn't remember any a
-- Just a -> button was clicked and button does remember an a
radio :: forall a. Attrs -> Component (Maybe a)
radio attrs = component $ widget \ma callbackma -> do
  maRef <- liftEffect $ Ref.new Nothing -- TODO EC - Just a?
  Tuple node _ <- elAttr "input" (attr "type" "radio" <> attrs) (pure unit)
  liftEffect $ setChecked node (isJust ma)
  onDomEvent "change" node $ const $ Ref.read maRef >>= callbackma
  pure case _ of
    Nothing -> setChecked node false
    Just a -> do
      Ref.write (Just a) maRef
      setChecked node true

-- Listeners

type Listener a = Node -> Effect a -> Effect Unit

onClick ∷ forall a. (a -> Effect Unit) -> Listener (Changed a)
onClick callback node eca = void $ addEventListener node "click" $ const do
  (Changed _ a) <- eca
  callback a

-- Running

runComponent :: forall a. Component a -> a -> Builder Unit Unit
runComponent c a = void $ unwrapWidget c (Changed Some a) \(Changed scope _) -> log $ "change in scope: " <> show scope

runMainComponent :: forall a. Component a -> a -> Effect Unit
runMainComponent c a = runMainBuilderInBody $ runComponent c a

-- Private

widget :: forall i o. (i -> (o -> Effect Unit) -> Builder Unit (i -> Effect Unit)) -> Widget i o
widget = Widget

unwrapWidget :: forall i o. Widget i o -> i -> (o -> Effect Unit) -> Builder Unit (i -> Effect Unit)
unwrapWidget (Widget w) = w

component :: forall a. Widget a a -> Component a
component w = widget \(Changed _ a) callback -> do
  update <- unwrapWidget w a \a -> callback (Changed Some a)
  pure \(Changed scope a) -> do
    case scope of
      None -> do
        pure unit
      _ -> update a


