module Web
  ( Widget
  , aside
  , aside'
  , bracket
  , button
  , button'
  , checkbox
  , clickable
  , div
  , div'
  , effect
  , fixed
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
  , hush
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
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, prozero, (^))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Attrs, DOM, Node, TagName, addEventCallback, attachComponent, attr, createComponent, createTextValue, detachComponent, elAttr, getChecked, getCurrentNode, getValue, initializeInBody, initializeInNode, rawHtml, setAttributes, setChecked, setValue, writeTextValue)

type Occurence a = Changed a -- new can be changed or not changed

type Propagation a = Occurence a -> Effect Unit

type Propagator m i o = Propagation o -> m (Propagation i)

-- Reactive? Reactor? Actor? - too generic, doesn't relate to DOM
-- WebActor? SiteActor? DOMActor?
-- Propagator?
newtype Widget i o = Widget (Propagator DOM i o)
--                           -- outward --         -- inward ---
-- Important: outward propagation should never be trigerred by inward propagation (TODO: how to encode it on type level? By allowing
-- update to perform only a subset of effects?) otherwise w1 ^ w2, where w1 and w2 call back on on input will enter infinite loop
-- of mutual updates.

unwrapWidget :: forall i o. Widget i o -> Propagation o -> DOM (Propagation i)
unwrapWidget (Widget w) = w

-- Capabilites

instance Profunctor Widget where
  dimap pre post w = Widget \outward -> do
    f <- unwrapWidget w $ outward <<< map post
    pure $ f <<< map pre

instance Strong Widget where
  first w = Widget \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \cha -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple _ b) -> outward $ (\a -> Tuple a b) <$> cha
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ fst <$> chab
  second w = Widget \outward -> do
    maandbRef <- liftEffect $ Ref.new Nothing
    update <- unwrapWidget w \chb -> do
      maandb <- Ref.read maandbRef
      for_ maandb \(Tuple a _) -> outward $ (\b -> Tuple a b) <$> chb
    pure \chab@(Changed _ aandb) -> do
      Ref.write (Just aandb) maandbRef
      case chab of
        Changed None _ -> mempty
        Changed _ _ -> update $ snd <$> chab

instance Choice Widget where
  left w = Widget \outward -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    Tuple fragment update <- createComponent $ unwrapWidget w \cha -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Left _) -> outward $ Left <$> cha
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
  right w = Widget \outward -> do
    maorbRef <- liftEffect $ Ref.new Nothing
    Tuple fragment update <- createComponent $ unwrapWidget w \chb -> do
      maorb <- Ref.read maorbRef
      case maorb of
        Just (Right _) -> outward $ Right <$> chb
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

instance ProfunctorZero Widget where
  prozero = Widget mempty

instance ChProfunctor Widget where
  chmap mapin mapout w = Widget \outward -> do
    update <- unwrapWidget w \(Changed c a) -> do
      outward $ Changed (mapout c) a
    pure \(Changed c a) -> update $ Changed (mapin c) a

instance Semigroupoid Widget where
  compose w2 w1 = Widget \outward -> do
    update2Ref <- liftEffect $ Ref.new $ unsafeCoerce unit
    update1 <- unwrapWidget w1 \(Changed _ b) -> do
      update2 <- Ref.read update2Ref
      update2 $ Changed Some b -- we force w2 to update cause w2 is updated only via outward by w1
    update2 <- unwrapWidget w2 outward
    liftEffect $ Ref.write update2 update2Ref
    pure update1

class ProductProfunctor p where
  purePP :: forall a b. b -> p a b

instance ProductProfunctor Widget where
  purePP b = Widget \outward -> pure case _ of
    Changed None _ -> pure unit
    _ -> outward (Changed Some b)

effect :: forall i o. (i -> Effect Unit) -> Widget i o
effect f = Widget \_ -> pure \(Changed _ a) -> f a -- outward is never called

-- Makes `Widget a b` fixed on `a` - no matter what `s` from the context of `Widget s t` is, so the `s`s are not listened to at all
fixed :: forall a b s t. a -> Widget a b -> Widget s t
fixed a w = Widget \_ -> do
  update <- unwrapWidget w mempty
  liftEffect $ update $ Changed Some a
  pure mempty

hush :: forall a b c. Widget a b -> Widget a c
hush w = Widget \_ -> unwrapWidget w mempty -- outward is never called

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

textInput :: Attrs -> Widget String String
textInput attrs = Widget \outward -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  liftEffect $ addEventCallback "input" node $ const $ getValue node >>= Changed Some >>> outward
  pure case _ of
    Changed None _ -> mempty
    Changed _ newa -> setValue node newa

checkbox :: Attrs -> Widget Boolean Boolean
checkbox attrs = Widget \outward -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
  liftEffect $ addEventCallback "input" node $ const $ getChecked node >>= Changed Some >>> outward
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
radioButton attrs = Widget \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  Tuple node _ <- elAttr "input" (attr "type" "radio" <> attrs) (pure unit)
  liftEffect $ addEventCallback "change" node $ const $ Ref.read maRef >>= Changed Some >>> outward
  pure case _ of
    Changed None _ -> mempty
    Changed _ Nothing -> setChecked node false
    Changed _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Widget optics

bracket :: forall ctx a b. DOM ctx -> (ctx -> Changed a -> Effect Unit) -> (ctx -> Changed b -> Effect Unit) -> Widget a b -> Widget a b
bracket afterInit afterInward beforeOutward w = Widget \outward -> do
  ctxRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  update <- unwrapWidget w $ (\chb -> do
    ctx <- Ref.read ctxRef
    beforeOutward ctx chb) <> outward
  ctx <- afterInit
  liftEffect $ Ref.write ctx ctxRef
  pure $ update <> afterInward ctx

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
element tagName attrs dynAttrs w = Widget \outward -> do
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w outward
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

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

button' :: forall a b. Widget a b -> Widget a b
button' = button mempty mempty

button :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
button = element "button"

clickable :: forall a b. Widget a b -> Widget a a
clickable w = Widget \outward -> do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  let buttonWidget = w # bracket (getCurrentNode >>= \node -> liftEffect $ addEventCallback "click" node $ const $ Ref.read aRef >>= outward) mempty mempty
  update <- unwrapWidget buttonWidget mempty
  pure case _ of
    Changed None _ -> mempty
    cha -> do
      Ref.write cha aRef
      update cha

svg :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
svg = element "svg"

path :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
path = element "path"

p :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
p = element "p"

p' :: forall a b. Widget a b -> Widget a b
p' = p mempty mempty

h1 :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
h1 attrs dynAttrs = element "h1" attrs dynAttrs

h1' :: forall a b. Widget a b -> Widget a b
h1' = h1 mempty mempty

h2 :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
h2 attrs dynAttrs = element "h2" attrs dynAttrs

h2' :: forall a b. Widget a b -> Widget a b
h2' = h2 mempty mempty

h3 :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
h3 attrs dynAttrs = element "h3" attrs dynAttrs

h3' :: forall a b. Widget a b -> Widget a b
h3' = h3 mempty mempty

h4 :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
h4 attrs dynAttrs = element "h4" attrs dynAttrs

h4' :: forall a b. Widget a b -> Widget a b
h4' = h4 mempty mempty

h5 :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
h5 attrs dynAttrs = element "h5" attrs dynAttrs

h5' :: forall a b. Widget a b -> Widget a b
h5' = h5 mempty mempty

h6 :: forall a b. Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
h6 attrs dynAttrs content = element "h6" attrs dynAttrs content

h6' :: forall a b. Widget a b -> Widget a b
h6' = h6 mempty mempty

-- Entry point

runWidgetInBody :: forall i o. Widget i o -> i -> Effect Unit
runWidgetInBody widget i = initializeInBody (unwrapWidget widget mempty) (Changed Some i)

runWidgetInNode :: forall i o. Node -> Widget i o -> (o -> Effect Unit) -> Effect (i -> Effect Unit)
runWidgetInNode node widget outward = do
  update <- initializeInNode node (unwrapWidget widget \(Changed _ o) -> outward o)
  pure \i -> update (Changed Some i)



