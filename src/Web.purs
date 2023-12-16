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
  , input
  )
  where

import Prelude hiding (zero, div)

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Propagator (Change(..), Occurrence(..), Propagator(..), bracket)
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Node, TagName, addEventListener, attr, getChecked, getValue, setAttributes, setChecked, setValue)
import Web.Internal.DOMBuilder (DOMBuilder, getCurrentNode, initializeInBody, initializeInNode)
import Web.Internal.DOMBuilder as Web.Internal.DOMBuilder


-- Widget

type Widget i o = Propagator DOMBuilder i o

-- Primitive widgets

text :: forall a. Widget String a
text = Propagator \_ -> do
  textValue <- Web.Internal.DOMBuilder.text
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ string -> textValue.write string

html :: forall a b. String -> Widget a b
html h = Propagator \_ -> do
  Web.Internal.DOMBuilder.html h
  pure $ mempty

input :: Object String -> Widget String String
input attrs = Propagator \outward -> do
  void $ Web.Internal.DOMBuilder.element "input" attrs (pure unit)
  node <- getCurrentNode
  void $ liftEffect $ addEventListener "input" node $ const $ getValue node >>= Occurrence Some >>> outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ newa -> setValue node newa

checkbox :: forall a . Object String -> Widget (Maybe a) (Maybe (Maybe a))
checkbox attrs = Propagator \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  void $ Web.Internal.DOMBuilder.element "input" (attr "type" "checkbox" <> attrs) (pure unit)
  node <- getCurrentNode
  void $ liftEffect $ addEventListener "input" node $ const do
    checked <- getChecked node
    ma <- Ref.read maRef
    outward $ Occurrence Some (if checked then Just ma else Nothing)
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ newma -> do
      setChecked node (isJust newma)
      for_ newma \newa -> Ref.write (Just newa) maRef

-- input:
-- Nothing -> turns off button
-- Just a -> turns on (if turned off) button and remembers `a`
-- output:
-- Nothing -> on button clicked when button doesn't remember any `a`
-- Just a -> on button clicked when button does remember an `a`
radioButton :: forall a. Object String -> Widget (Maybe a) (Maybe a)
radioButton attrs = Propagator \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  -- TODO pass listeners to element function?
  void $ Web.Internal.DOMBuilder.element "input" (attr "type" "radio" <> attrs) (pure unit)
  node <- getCurrentNode
  void $ liftEffect $ addEventListener "change" node $ const $ Ref.read maRef >>= Occurrence Some >>> outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ Nothing -> setChecked node false
    Occurrence _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Widget optics

element :: forall a b. TagName -> Object String -> (a -> Object String) -> Widget a b -> Widget a b
element tagName attrs dynAttrs w = Propagator \outward -> do
  update <- Web.Internal.DOMBuilder.element tagName attrs $ unwrap w outward
  node <- getCurrentNode
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence ch newa -> do
      setAttributes node (attrs <> dynAttrs newa)
      update $ Occurrence ch newa

div' :: forall a b. Widget a b -> Widget a b
div' = div mempty mempty

div :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
div = element "div"

span' :: forall a b. Widget a b -> Widget a b
span' = span mempty mempty

span :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
span = element "span"

aside' :: forall a b. Widget a b -> Widget a b
aside' = aside mempty mempty

aside :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
aside = element "aside"

label' :: forall a b. Widget a b -> Widget a b
label' = label mempty mempty

label :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
label = element "label"

button' :: forall a b. Widget a b -> Widget a b
button' = button mempty mempty

button :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
button = element "button"

clickable :: forall a b. Widget a b -> Widget a a
clickable w = Propagator \outward -> do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  let buttonWidget = w # bracket (getCurrentNode >>= \node -> liftEffect $ addEventListener "click" node $ const $ Ref.read aRef >>= outward) (const $ pure) (const $ pure)
  update <- unwrap buttonWidget mempty
  pure case _ of
    Occurrence None _ -> mempty
    cha -> do
      Ref.write cha aRef
      update cha

svg :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
svg = element "svg"

path :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
path = element "path"

p :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
p = element "p"

p' :: forall a b. Widget a b -> Widget a b
p' = p mempty mempty

h1 :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
h1 attrs dynAttrs = element "h1" attrs dynAttrs

h1' :: forall a b. Widget a b -> Widget a b
h1' = h1 mempty mempty

h2 :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
h2 attrs dynAttrs = element "h2" attrs dynAttrs

h2' :: forall a b. Widget a b -> Widget a b
h2' = h2 mempty mempty

h3 :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
h3 attrs dynAttrs = element "h3" attrs dynAttrs

h3' :: forall a b. Widget a b -> Widget a b
h3' = h3 mempty mempty

h4 :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
h4 attrs dynAttrs = element "h4" attrs dynAttrs

h4' :: forall a b. Widget a b -> Widget a b
h4' = h4 mempty mempty

h5 :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
h5 attrs dynAttrs = element "h5" attrs dynAttrs

h5' :: forall a b. Widget a b -> Widget a b
h5' = h5 mempty mempty

h6 :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
h6 attrs dynAttrs content = element "h6" attrs dynAttrs content

h6' :: forall a b. Widget a b -> Widget a b
h6' = h6 mempty mempty

-- Entry point

runWidgetInBody :: forall i o. Widget i o -> i -> Effect Unit
runWidgetInBody widget i = initializeInBody (unwrap widget mempty) (Occurrence Some i)

runWidgetInNode :: forall i o. Node -> Widget i o -> i -> (o -> Effect Unit) -> Effect Unit
runWidgetInNode node widget i outward = initializeInNode node (unwrap widget \(Occurrence _ o) -> outward o) (Occurrence Some i)

