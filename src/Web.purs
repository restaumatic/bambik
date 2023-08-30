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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Change (class ChProfunctor, Change(..), Changed(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, proplusfirst, proplussecond, pzero, (<^), (^), (^>))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Specular.Dom.Builder (Attrs, Builder, Node, TagName, addEventListener, appendSlot, attr, elAttr, getChecked, getValue, newSlot, populateBody, populateSlot, setAttributes, setChecked, setValue)
import Specular.Dom.Builder as Builder

newtype Widget i o = Widget ((Changed o -> Effect Unit) -> Builder Unit (Changed i -> Effect Unit))

-- Capabilites

instance Profunctor Widget where
  dimap pre post w = Widget \callback -> do
    f <- unwrapWidget w $ callback <<< map post
    pure $ f <<< map pre

instance Strong Widget where
  first w = Widget \callback -> do
    aandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \cha@(Changed _ a) -> do
      aandb <- Ref.read aandbRef
      case aandb of
        Just (Tuple _ b) -> callback $ (\a -> Tuple a b) <$> cha
        _ -> mempty
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) aandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ fst <$> chab
  second w = Widget \callback -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \chb@(Changed _ b) -> do
      maandb <- Ref.read maandbRef
      case maandb of
        Just (Tuple a _) -> callback $ (\b -> Tuple a b) <$> chb
        _ -> mempty
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ snd <$> chab

instance Choice Widget where
  left w = Widget \callback -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \cha@(Changed _ a) -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Left _) -> callback $ Left <$> cha
        _ -> mempty
    pure \chaorb@(Changed _ aorb) -> do
      Ref.write (Just aorb) maorbRef
      case chaorb of
        Changed None _ -> mempty
        Changed _ (Left a) -> update $ a <$ chaorb
        _ -> mempty
  right w = Widget \callback -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \chb@(Changed _ b) -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Right _) -> callback $ Right <$> chb
        _ -> mempty
    pure \chaorb@(Changed _ aorb) -> do
      Ref.write (Just aorb) maorbRef
      case chaorb of
        Changed None _ -> mempty
        Changed _ (Right b) -> update $ b <$ chaorb
        _ -> mempty

instance ProfunctorPlus Widget where
  proplus c1 c2 = Widget \updateParent -> do
    -- TODO how to get rid of thess refs?
    mUpdate1Ref <- liftEffect $ Ref.new Nothing
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    update1 <- unwrapWidget c1 \cha@(Changed _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      mUpdate1 <- Ref.read mUpdate1Ref
      let update1 = maybe mempty identity mUpdate1
      update1 (Changed None a)
      update2 cha
      updateParent cha
    liftEffect $ Ref.write (Just update1) mUpdate1Ref
    update2 <- unwrapWidget c2 \cha@(Changed _ a) -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      update2 (Changed None a)
      update1 cha
      updateParent cha
    liftEffect $ Ref.write (Just update2) mUpdate2Ref
    pure $ update1 <> update2
  proplusfirst c1 c2 = Widget \updateParent -> do
    update1 <- unwrapWidget c1 updateParent
    update2 <- unwrapWidget c2 mempty
    pure $ update1 <> update2
  proplussecond c1 c2 = Widget \updateParent -> do
    update1 <- unwrapWidget c1 mempty
    update2 <- unwrapWidget c2 updateParent
    pure $ update1 <> update2

instance ProfunctorZero Widget where
  pzero = Widget mempty

instance ChProfunctor Widget where
  chmap mapin mapout w = Widget \callback -> do
    update <- unwrapWidget w \(Changed c a) -> do
      callback $ Changed (mapout c) a
    pure \(Changed c a) -> update $ Changed (mapin c) a
  fixed a w = Widget \_ -> do
    update <- unwrapWidget w mempty
    liftEffect $ update $ Changed Some a
    pure mempty

instance Semigroupoid Widget where
  compose w2 w1 = Widget \callbackc -> do
    slot <- newSlot
    liftEffect $ populateSlot slot $ unwrapWidget w1 \chb@(Changed _ b) -> do
      spawnedSlot <- appendSlot slot
      update <- populateSlot spawnedSlot $ unwrapWidget w2 callbackc
      update chb
      -- note: w2 cannot be updated not destroyed externally, w2 should itself take care of its scope destroy

-- Primitives

text :: forall a. Widget String a
text = Widget \_ -> do
  slot <- newSlot
  pure case _ of
    Changed None _ -> pure unit
    Changed _ news -> update slot news
    where
      update slot s = populateSlot slot $ Builder.text s

textInput :: Attrs -> Widget String String -- TODO EC incorporate validation here? The id would be plain Widget?
textInput attrs = Widget \callbackcha -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  liftEffect $ addEventListener "input" node $ const $ getValue node >>= Changed Some >>> callbackcha
  pure case _ of
    Changed None _ -> mempty
    Changed _ newa -> setValue node newa

checkbox :: Attrs -> Widget Boolean Boolean
checkbox attrs = Widget \callbackcha -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
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
radioButton attrs = Widget \callbackchma -> do
  maRef <- liftEffect $ Ref.new Nothing
  Tuple node _ <- elAttr "input" (attr "type" "radio" <> attrs) (pure unit)
  liftEffect $ addEventListener "change" node $ const $ Ref.read maRef >>= Changed Some >>> callbackchma
  pure case _ of
    Changed None _ -> mempty
    Changed _ Nothing -> setChecked node false
    Changed _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Optics

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
element tagName attrs dynAttrs listener w = Widget \callbackb -> do
  aRef <- liftEffect $ Ref.new Nothing
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w callbackb
  liftEffect $ listener node (Ref.read aRef)
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      Ref.write (Just newa) aRef
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

-- Element that cleans up after first output emitted, TODO EC: clean it up
element_ :: forall a b. TagName -> Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
element_ tagName attrs dynAttrs listener w = Widget \callbackb -> do
  aRef <- liftEffect $ Ref.new Nothing
  cleanupRef <- liftEffect $ Ref.new Nothing
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w \chb -> do
    mCleanup <- Ref.read cleanupRef
    callbackb chb
    fromMaybe mempty mCleanup
  cleanup <- liftEffect $ listener node (Ref.read aRef) (callbackb <<< Changed Some)
  liftEffect $ Ref.write (Just cleanup) cleanupRef
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      Ref.write (Just newa) aRef
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

element' :: forall a b. TagName -> Widget a b -> Widget a b
element' tagName = element tagName mempty mempty mempty

div' :: forall a b. Widget a b -> Widget a b
div' = element' "div"

div :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
div = element "div"

span' :: forall a b. Widget a b -> Widget a b
span' = element' "span"

span :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
span = element "span"

aside' :: forall a b. Widget a b -> Widget a b
aside' = element' "aside"

aside :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
aside = element_ "aside"

label' :: forall a b. Widget a b -> Widget a b
label' = element' "label"

label :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
label = element "label"

button :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
button = element_ "button"

button' :: forall a b. Widget a b -> Widget a b
button' = element' "button"

h1 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
h1 = element "h1"

h1' :: forall a b. Widget a b -> Widget a b
h1' = element' "h1"

h2 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
h2 = element "h2"

h2' :: forall a b. Widget a b -> Widget a b
h2' = element' "h2"

h3 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
h3 = element "h3"

h3' :: forall a b. Widget a b -> Widget a b
h3' = element' "h3"

h4 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
h4 = element "h4"

h4' :: forall a b. Widget a b -> Widget a b
h4' = element' "h4"

-- Entry point

runWidgetInBody :: forall i o. Widget i o -> i -> Effect Unit
runWidgetInBody w i = do
  update <- populateBody $ unwrapWidget w mempty
  update (Changed Some i)

runWidgetInBuilder :: forall i o. Widget i o -> (o -> Effect Unit) -> Builder Unit (i -> Effect Unit)
runWidgetInBuilder widget outViewModelCallback = do
  update <- unwrapWidget widget \(Changed _ o) -> outViewModelCallback o
  pure $ update <<< Changed Some

-- Private

unwrapWidget :: forall i o. Widget i o -> (Changed o -> Effect Unit) -> Builder Unit (Changed i -> Effect Unit)
unwrapWidget (Widget w) = w
