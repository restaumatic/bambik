module Web
  ( Widget
  , aside
  , aside'
  , button
  , button'
  , checkbox
  , clickable
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
  )
  where

import Prelude hiding (zero, div)

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Change (Change(..))
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, prozero, (^))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Propagator (Occurrence(..), Propagator(..), bracket)
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Attrs, DOM, Node, TagName, addEventCallback, attr, createTextValue, elAttr, getChecked, getCurrentNode, getValue, initializeInBody, initializeInNode, rawHtml, setAttributes, setChecked, setValue, writeTextValue)


-- Widget

type Widget i o = Propagator DOM i o

-- Primitive widgets

text :: forall a. Widget String a
text = Propagator \_ -> do
  textValue <- createTextValue
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ string -> writeTextValue textValue string

html :: forall a b. String -> Widget a b
html h = Propagator \_ -> do
  rawHtml h
  mempty

textInput :: Attrs -> Widget String String
textInput attrs = Propagator \outward -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  liftEffect $ addEventCallback "input" node $ const $ getValue node >>= Occurrence Some >>> outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ newa -> setValue node newa

checkbox :: Attrs -> Widget Boolean Boolean
checkbox attrs = Propagator \outward -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
  liftEffect $ addEventCallback "input" node $ const $ getChecked node >>= Occurrence Some >>> outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ newa -> setChecked node newa

-- input:
-- Nothing -> turns off button
-- Just a -> turns on (if turned off) button and remembers `a`
-- output:
-- Nothing -> on button clicked when button doesn't remember any `a`
-- Just a -> on button clicked when button does remember an `a`
radioButton :: forall a. Attrs -> Widget (Maybe a) (Maybe a)
radioButton attrs = Propagator \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  Tuple node _ <- elAttr "input" (attr "type" "radio" <> attrs) (pure unit)
  liftEffect $ addEventCallback "change" node $ const $ Ref.read maRef >>= Occurrence Some >>> outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ Nothing -> setChecked node false
    Occurrence _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Widget optics

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> Widget a b -> Widget a b
element tagName attrs dynAttrs w = Propagator \outward -> do
  Tuple node update <- elAttr tagName attrs $ unwrap w outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence ch newa -> do
      setAttributes node (attrs <> dynAttrs newa)
      update $ Occurrence ch newa

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
clickable w = Propagator \outward -> do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  let buttonWidget = w # bracket (getCurrentNode >>= \node -> liftEffect $ addEventCallback "click" node $ const $ Ref.read aRef >>= outward) mempty mempty
  update <- unwrap buttonWidget mempty
  pure case _ of
    Occurrence None _ -> mempty
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
runWidgetInBody widget i = initializeInBody (unwrap widget mempty) (Occurrence Some i)

runWidgetInNode :: forall i o. Node -> Widget i o -> i -> (o -> Effect Unit) -> Effect Unit
runWidgetInNode node widget i outward = initializeInNode node (unwrap widget \(Occurrence _ o) -> outward o) (Occurrence Some i)

