module Web
  ( Widget
  , aside
  , aside'
  , button
  , button'
  , checkbox
  , div
  , div'
  , element
  , element'
  , element_
  , h1
  , h1'
  , h2
  , h2'
  , h3
  , h3'
  , h4
  , h4'
  , label
  , label'
  , module Data.Profunctor.Plus
  , radioButton
  , runWidgetInBody
  , runWidgetInBuilder
  , span
  , span'
  , text
  , textInput
  )
  where

import Prelude hiding (zero)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Change (class ChProfunctor, Change(..), Changed(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, proplusfirst, proplussecond, pzero, (<^), (^), (^>))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Specular.Dom.Builder (Attrs, Builder, Node, TagName, addEventListener, appendSlot, attr, elAttr, getChecked, getValue, newSlot, populateBody, replaceSlot, setAttributes, setChecked, setValue)
import Specular.Dom.Builder as Builder

-- type Context = { slot :: Slot (Builder Context)}

newtype Widget i o = Widget (i -> (Changed o -> Effect Unit) -> Builder Unit (Changed i -> Effect Unit))

-- Capabilites

instance Profunctor Widget where
  dimap pre post w = Widget \a callback -> do
    f <- unwrapWidget w (pre a) $ callback <<< map post
    pure $ f <<< map pre

instance Strong Widget where
  first w = Widget \ab callbackchab -> do
    sndRef <- liftEffect $ Ref.new (snd ab)
    update <- unwrapWidget w (fst ab) \(Changed ch a) -> do
      snd <- liftEffect $ Ref.read sndRef
      callbackchab $ Changed ch $ Tuple a snd
    pure case _ of
      Changed None newab -> Ref.write (snd newab) sndRef
      Changed ch newab -> do
        Ref.write (snd newab) sndRef
        update $ Changed ch $ fst newab
  second w = Widget \ab callbackchab -> do
    fstRef <- liftEffect $ Ref.new (fst ab)
    update <- unwrapWidget w (snd ab) \(Changed ch b) -> do
      fst <- liftEffect $ Ref.read fstRef
      callbackchab $ Changed ch $ Tuple fst b
    pure case _ of
      Changed None newab -> Ref.write (fst newab) fstRef
      Changed ch newab -> do
        Ref.write (fst newab) fstRef
        update $ Changed ch $ snd newab

instance Choice Widget where
  left w = Widget \aorb callbackchaorb -> do
    slot <- newSlot
    mUpdate <- liftEffect $ replaceSlot slot $ case aorb of
      Left a -> do
        update <- unwrapWidget w a (callbackchaorb <<< map Left)
        pure $ Just update
      Right _ -> pure Nothing
    mUpdateRef <- liftEffect $ Ref.new mUpdate
    pure \chaorb -> case chaorb of
      Changed None _ -> pure unit
      Changed ch (Left a) -> do
        mUpdate <- Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> makeUpdate slot callbackchaorb mUpdateRef a
        update $ Changed ch a
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
        makeUpdate slot callbackchaorb mUpdateRef a = do
          newUpdate <- replaceSlot slot $ unwrapWidget w a (callbackchaorb <<< map Left)
          Ref.write (Just newUpdate) mUpdateRef
          pure newUpdate
  right w = Widget \aorb callbackchaorb -> do
    slot <- newSlot
    mUpdate <- liftEffect $ replaceSlot slot $ case aorb of
      Right a -> do
        update <- unwrapWidget w a (callbackchaorb <<< map Right)
        pure $ Just update
      Left _ -> pure Nothing
    mUpdateRef <- liftEffect $ Ref.new mUpdate
    case aorb of
      Left _ -> pure unit
      Right b -> void $ liftEffect $ makeUpdate slot callbackchaorb mUpdateRef b
    pure \chaorb -> case chaorb of
      Changed None _ -> pure unit
      Changed c (Right b) -> do
        mUpdate <- Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> makeUpdate slot callbackchaorb mUpdateRef b
        update $ Changed c b
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
          newUpdate <- replaceSlot slot $ unwrapWidget w a (abcallback <<< map Right)
          Ref.write (Just newUpdate) mUpdateRef
          pure newUpdate

instance ProfunctorPlus Widget where
  proplus c1 c2 = Widget \initial updateParent -> do
    -- TODO how to get rid of thess refs?
    mUpdate1Ref <- liftEffect $ Ref.new Nothing
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    update1 <- unwrapWidget c1 initial \cha@(Changed _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      mUpdate1 <- Ref.read mUpdate1Ref
      let update1 = maybe mempty identity mUpdate1
      update1 (Changed None a)
      update2 cha
      updateParent cha
    liftEffect $ Ref.write (Just update1) mUpdate1Ref
    update2 <- unwrapWidget c2 initial \cha@(Changed _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      update2 (Changed None a)
      update1 cha
      updateParent cha
    liftEffect $ Ref.write (Just update2) mUpdate2Ref
    pure $ update1 <> update2
  proplusfirst c1 c2 = Widget \initial updateParent -> do
    update1 <- unwrapWidget c1 initial updateParent
    update2 <- unwrapWidget c2 initial mempty
    pure $ update1 <> update2
  proplussecond c1 c2 = Widget \initial updateParent -> do
    update1 <- unwrapWidget c1 initial mempty
    update2 <- unwrapWidget c2 initial updateParent
    pure $ update1 <> update2

instance ProfunctorZero Widget where
  pzero = Widget mempty

instance ChProfunctor Widget where
  chmap mapin mapout w = Widget \initial callback -> do
    update <- unwrapWidget w initial \(Changed c a) -> do
      callback $ Changed (mapout c) a
    pure \(Changed c a) -> update $ Changed (mapin c) a
  static a w = Widget \_ _ -> do
    _ <- unwrapWidget w a mempty
    pure mempty

instance Semigroupoid Widget where
  compose w2 w1 = Widget \inita callbackc -> do
    slot <- newSlot
    liftEffect $ replaceSlot slot $ unwrapWidget w1 inita \(Changed _ b) -> do
      spawnedSlot <- appendSlot slot
      void $ replaceSlot spawnedSlot $ unwrapWidget w2 b callbackc
      -- note: w2 cannot be updated not destroyed externally, w2 should itself take care of its scope destroy

-- Primitives

text :: forall a. Widget String a
text = Widget \s _ -> do
  slot <- newSlot
  _ <- liftEffect $ update slot s
  pure case _ of
    Changed None _ -> pure unit
    Changed _ news -> update slot news
    where
      update slot s = replaceSlot slot $ Builder.text s

textInput :: Attrs -> Widget String String -- TODO EC incorporate validation here? The id would be plain Widget?
textInput attrs = Widget \a callbackcha -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  liftEffect $ setValue node a
  liftEffect $ addEventListener "input" node $ const $ getValue node >>= Changed Some >>> callbackcha
  pure case _ of
    Changed None _ -> mempty
    Changed _ newa -> setValue node newa

checkbox :: Attrs -> Widget Boolean Boolean
checkbox attrs = Widget \a callbackcha -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
  liftEffect $ setChecked node a
  liftEffect $ addEventListener "input" node $ const $ getChecked node >>= Changed Some >>> callbackcha
  pure case _ of
    Changed None _ -> mempty
    Changed _ newa -> setChecked node newa

-- input:
-- Nothing -> turns off button
-- Just a -> turns on (if turned off) button and remembers `a`
-- output:
-- Nothing -> on button clicked when button doesn't remember any `a`
-- Just a -> on button clicked when button does remember an `a`
radioButton :: forall a. Attrs -> Widget (Maybe a) (Maybe a)
radioButton attrs = Widget \ma callbackchma -> do
  maRef <- liftEffect $ Ref.new ma
  Tuple node _ <- elAttr "input" (attr "type" "radio" <> attrs) (pure unit)
  liftEffect $ setChecked node (isJust ma)
  liftEffect $ addEventListener "change" node $ const $ Ref.read maRef >>= Changed Some >>> callbackchma
  pure case _ of
    Changed None _ -> mempty
    Changed _ Nothing -> setChecked node false
    Changed _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Optics

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
element tagName attrs dynAttrs listener w = Widget \a callbackb -> do
  aRef <- liftEffect $ Ref.new a
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w a callbackb
  liftEffect $ setAttributes node (attrs <> dynAttrs a)
  liftEffect $ listener node (Ref.read aRef)
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      Ref.write newa aRef
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

-- Element that cleans up after first output emitted, TODO EC: clean it up
element_ :: forall a b. TagName -> Attrs -> (a -> Attrs) -> (Node -> Effect a -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
element_ tagName attrs dynAttrs listener w = Widget \a callbackb -> do
  aRef <- liftEffect $ Ref.new a
  cleanupRef <- liftEffect $ Ref.new Nothing
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w a \chb -> do
    mCleanup <- Ref.read cleanupRef
    callbackb chb
    fromMaybe mempty mCleanup
  liftEffect $ setAttributes node (attrs <> dynAttrs a)
  cleanup <- liftEffect $ listener node (Ref.read aRef) (callbackb <<< Changed Some)
  liftEffect $ Ref.write (Just cleanup) cleanupRef
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      Ref.write newa aRef
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

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

aside' :: forall a b. Widget a b -> Widget a b
aside' = element' "aside"

aside :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
aside = element_ "aside"

label' :: forall a b. Widget a b -> Widget a b
label' = element' "label"

label :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
label = element "label"

button :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
button = element_ "button"

button' :: forall a b. Widget a b -> Widget a b
button' = element' "button"

h1 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h1 = element "h1"

h1' :: forall a b. Widget a b -> Widget a b
h1' = element' "h1"

h2 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h2 = element "h2"

h2' :: forall a b. Widget a b -> Widget a b
h2' = element' "h2"

h3 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h3 = element "h3"

h3' :: forall a b. Widget a b -> Widget a b
h3' = element' "h3"

h4 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h4 = element "h4"

h4' :: forall a b. Widget a b -> Widget a b
h4' = element' "h4"

-- Entry point

runWidgetInBody :: forall i o. Widget i o -> i -> Effect Unit
runWidgetInBody w a = populateBody $ void $ unwrapWidget w a mempty

runWidgetInBuilder :: forall i o. Widget i o -> i -> (o -> Effect Unit) -> Builder Unit (i -> Effect Unit)
runWidgetInBuilder widget initialInViewModel outViewModelCallback = do
  update <- unwrapWidget widget initialInViewModel \(Changed _ o) -> outViewModelCallback o
  pure $ update <<< Changed Some

-- Private

unwrapWidget :: forall i o. Widget i o -> i -> (Changed o -> Effect Unit) -> Builder Unit (Changed i -> Effect Unit)
unwrapWidget (Widget w) = w
