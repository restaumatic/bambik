module Web
  ( Widget
  , aside
  , aside'
  , bracket
  , button
  , button'
  , checkbox
  , div
  , div'
  , h1
  , h1'
  , h2
  , h2'
  , h3
  , h3'
  , h4
  , h4'
  , h5
  , h5'
  , h6
  , h6'
  , html
  , input
  , input'
  , label
  , label'
  , module Data.Profunctor.Plus
  , p
  , p'
  , path
  , radioButton
  , runWidgetInBody
  , runWidgetInNode
  , span
  , span'
  , svg
  , text
  , textInput
  , unwrapWidget
  )
  where

import Prelude hiding (zero, div)

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Change (class ChProfunctor, Change(..), Changed(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, proplusfirst, proplussecond, pzero, (<^), (^), (^>))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Attrs, DOM, Node, TagName, addEventCallback, attachComponent, attr, createComponent, createTextValue, detachComponent, elAttr, getChecked, getValue, initializeInBody, initializeInNode, rawHtml, setAttributes, setChecked, setValue, writeTextValue)

newtype Widget i o = Widget ((Changed o -> Effect Unit) -> DOM (Changed i -> Effect Unit))

unwrapWidget :: forall i o. Widget i o -> (Changed o -> Effect Unit) -> DOM (Changed i -> Effect Unit)
unwrapWidget (Widget w) = w

-- Capabilites

instance Profunctor Widget where
  dimap pre post w = Widget \callback -> do
    f <- unwrapWidget w $ callback <<< map post
    pure $ f <<< map pre

instance Strong Widget where
  first w = Widget \callback -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \cha -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple _ b) -> callback $ (\a -> Tuple a b) <$> cha
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ fst <$> chab
  second w = Widget \callback -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \chb -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple a _) -> callback $ (\b -> Tuple a b) <$> chb
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ snd <$> chab

instance Choice Widget where
  left w = Widget \callback -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    Tuple fragment update <- createComponent $ unwrapWidget w \cha -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Left _) -> callback $ Left <$> cha
        _ -> mempty
    pure \chaorb@(Changed _ aorb) -> do
      moldaorb <- Ref.modify' (\oldState -> { state: Just aorb, value: oldState}) maorbRef
      case chaorb of
        Changed None _ -> mempty
        Changed _ (Left a) -> do
          update $ a <$ chaorb -- first update and only then possibly attach
          case moldaorb of
            (Just (Left _)) -> mempty
            _ -> attachComponent fragment
        Changed _ (Right _) -> do
          case moldaorb of
            (Just (Left _)) -> detachComponent fragment
            _ -> mempty
  right w = Widget \callback -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    Tuple fragment update <- createComponent $ unwrapWidget w \chb -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Right _) -> callback $ Right <$> chb
        _ -> mempty
    pure \chaorb@(Changed _ aorb) -> do
      moldaorb <- Ref.modify' (\oldState -> { state: Just aorb, value: oldState}) maorbRef
      case chaorb of
        Changed None _ -> mempty
        Changed _ (Right b) -> do
          update $ b <$ chaorb  -- first update and only then possibly attach
          case moldaorb of
            (Just (Right _)) -> mempty
            _ -> attachComponent fragment
        Changed _ (Left _) -> do
          case moldaorb of
            (Just (Right _)) -> detachComponent fragment
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
  compose w2 w1 = Widget \callback -> do
    update2Ref <- liftEffect $ Ref.new $ unsafeCoerce unit
    update1 <- unwrapWidget w1 \(Changed _ b) -> do
      update2 <- Ref.read update2Ref
      update2 $ Changed Some b -- we force w2 to update cause w2 is updated only via callback by w1
    update2 <- unwrapWidget w2 callback
    liftEffect $ Ref.write update2 update2Ref
    pure update1

instance Category Widget where
  identity = Widget pure -- update triggers callback

-- Primitive widgets

text :: forall a. Widget String a
text = Widget \_ -> do
  textValue <- createTextValue
  pure case _ of
    Changed None _ -> mempty
    Changed _ string -> writeTextValue textValue string

html :: forall a b. String -> Widget a b
html h = Widget \_ -> do
  rawHtml h
  mempty

textInput :: Attrs -> Widget String String -- TODO EC incorporate validation here? The id would be plain Widget?
textInput attrs = Widget \callbackcha -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  liftEffect $ addEventCallback "input" node $ const $ getValue node >>= Changed Some >>> callbackcha
  pure case _ of
    Changed None _ -> mempty
    Changed _ newa -> setValue node newa

checkbox :: Attrs -> Widget Boolean Boolean
checkbox attrs = Widget \callbackcha -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
  liftEffect $ addEventCallback "input" node $ const $ getChecked node >>= Changed Some >>> callbackcha
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
  liftEffect $ addEventCallback "change" node $ const $ Ref.read maRef >>= Changed Some >>> callbackchma
  pure case _ of
    Changed None _ -> mempty
    Changed _ Nothing -> setChecked node false
    Changed _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Widget transformers

bracket :: forall ctx a b. DOM ctx -> (ctx -> Changed a -> Effect Unit) -> (ctx -> Changed b -> Effect Unit) -> Widget a b -> Widget a b
bracket afterInit afterUpdate beforeCallback w = Widget \callback -> do
  ctxRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  update <- unwrapWidget w $ (\chb -> do
    ctx <- Ref.read ctxRef
    beforeCallback ctx chb) <> callback
  ctx <- afterInit
  liftEffect $ Ref.write ctx ctxRef
  pure $ update <> afterUpdate ctx

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
element tagName attrs dynAttrs w = Widget \callbackb -> do
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w callbackb
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

input' :: forall a b. Widget a b -> Widget a b
input' = input mempty mempty

input :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
input = element "div"

div' :: forall a b. Widget a b -> Widget a b
div' = div mempty mempty

div :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
div = element "div"

span' :: forall a b. Widget a b -> Widget a b
span' = span mempty mempty

span :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
span = element "span"

aside' :: forall a b. Widget a b -> Widget a b
aside' = aside mempty mempty

aside :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
aside = element "aside"

label' :: forall a b. Widget a b -> Widget a b
label' = label mempty mempty

label :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
label = element "label"

-- TODO EC refactor to using `element`, `bracket` etc functions
button :: forall a. Attrs -> (a -> Attrs) -> Widget a a -> Widget a a
button attrs dynAttrs w = Widget \callbacka -> do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  Tuple node update <- elAttr "button" attrs $ unwrapWidget w mempty
  liftEffect $ addEventCallback "click" node $ const $ Ref.read aRef >>= callbacka
  pure case _ of
    Changed None _ -> mempty
    cha@(Changed _ newa) -> do
      setAttributes node (attrs <> dynAttrs newa)
      Ref.write cha aRef
      update cha

button' :: forall a. Widget a a -> Widget a a
button' = button mempty mempty

svg :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
svg = element "svg"

path :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
path = element "path"

p :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
p = element "p"

p' :: forall a b. Widget a b -> Widget a b
p' = p mempty mempty

h1 :: forall a b. Attrs -> (a -> Attrs) -> (Widget String String -> Widget a b) -> Widget a b
h1 attrs dynAttrs content = element "h1" attrs dynAttrs $ text # content

h1' :: forall a b. (Widget String String -> Widget a b) -> Widget a b
h1' content = h1 mempty mempty content

h2 :: forall a b. Attrs -> (a -> Attrs) -> (Widget String String -> Widget a b) -> Widget a b
h2 attrs dynAttrs content = element "h2" attrs dynAttrs $ text # content

h2' :: forall a b. (Widget String String -> Widget a b) -> Widget a b
h2' content = h2 mempty mempty content

h3 :: forall a b. Attrs -> (a -> Attrs) -> (Widget String String -> Widget a b) -> Widget a b
h3 attrs dynAttrs content = element "h3" attrs dynAttrs $ text # content

h3' :: forall a b. (Widget String String -> Widget a b) -> Widget a b
h3' content = h3 mempty mempty content

h4 :: forall a b. Attrs -> (a -> Attrs) -> (Widget String String -> Widget a b) -> Widget a b
h4 attrs dynAttrs content = element "h4" attrs dynAttrs $ text # content

h4' :: forall a b. (Widget String String -> Widget a b) -> Widget a b
h4' content = h4 mempty mempty content

h5 :: forall a b. Attrs -> (a -> Attrs) -> (Widget String String -> Widget a b) -> Widget a b
h5 attrs dynAttrs content = element "h5" attrs dynAttrs $ text # content

h5' :: forall a b. (Widget String String -> Widget a b) -> Widget a b
h5' content = h5 mempty mempty content

h6 :: forall a b. Attrs -> (a -> Attrs) -> (Widget String String -> Widget a b) -> Widget a b
h6 attrs dynAttrs content = element "h6" attrs dynAttrs $ text # content

h6' :: forall a b. (Widget String String -> Widget a b) -> Widget a b
h6' content = h6 mempty mempty content

-- Entry point

runWidgetInBody :: forall i o. Widget i o -> i -> Effect Unit
runWidgetInBody widget i = initializeInBody (unwrapWidget widget mempty) (Changed Some i)

runWidgetInNode :: forall i o. Node -> Widget i o -> (o -> Effect Unit) -> Effect (i -> Effect Unit)
runWidgetInNode node widget callback = do
  update <- initializeInNode node (unwrapWidget widget \(Changed _ o) -> callback o)
  pure \i -> update (Changed Some i)



