module Web
  ( Widget
  , aside
  , at'
  , ats'
  , button
  , checkbox
  , cl'
  , clickable
  , dat'
  , dcl'
  , div
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , html
  , input
  , label
  , p
  , path
  , radioButton
  , runWidgetInBody
  , runWidgetInNode
  , span
  , svg
  , text
  )
  where

import Prelude hiding (zero, div)

import Control.Monad.State (gets)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Propagator (Change(..), Occurrence(..), Propagator(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Node, TagName, addClass, addEventListener, getChecked, getValue, removeClass, setAttribute, setAttributes, setChecked, setTextNodeValue, setValue)
import Web.Internal.DOMBuilder (DOMBuilder, initializeInBody, initializeInNode)
import Web.Internal.DOMBuilder as Web.Internal.DOMBuilder


-- Widget

type Widget i o = Propagator DOMBuilder i o

-- Primitive widgets

text :: forall a. Widget String a
text = Propagator \_ -> do
  Web.Internal.DOMBuilder.text
  node <- gets _.sibling
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ string -> setTextNodeValue node string

html :: forall a b. String -> Widget a b
html h = Propagator \_ -> do
  Web.Internal.DOMBuilder.html h
  pure $ mempty

input :: Widget String String
input = Propagator \outward -> do
  void $ Web.Internal.DOMBuilder.element "input" (pure unit)
  node <- gets _.sibling
  void $ liftEffect $ addEventListener "input" node $ const $ getValue node >>= Occurrence Some >>> outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ newa -> setValue node newa

checkbox :: forall a . Widget (Maybe a) (Maybe (Maybe a))
checkbox = Propagator \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  void $ Web.Internal.DOMBuilder.element "input" (pure unit)
  node <- gets _.sibling
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
radioButton :: forall a. Widget (Maybe a) (Maybe a)
radioButton = Propagator \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  void $ Web.Internal.DOMBuilder.element "input" (pure unit)
  node <- gets _.sibling
  void $ liftEffect $ addEventListener "change" node $ const $ Ref.read maRef >>= Occurrence Some >>> outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence _ Nothing -> setChecked node false
    Occurrence _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Widget optics

element :: forall a b. TagName -> Widget a b -> Widget a b
element tagName w = Propagator \outward -> do
  update <- Web.Internal.DOMBuilder.element tagName do
    unwrap w outward
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence ch newa -> do
      update $ Occurrence ch newa

ats' :: forall a b. Object String -> (a -> Object String) -> Widget a b -> Widget a b
ats' attrs dynAttrs w = Propagator \outward -> do
  update <- unwrap w outward
  Web.Internal.DOMBuilder.ats attrs
  node <- gets _.sibling
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence ch newa -> do
      setAttributes node (attrs <> dynAttrs newa)
      update $ Occurrence ch newa

at' :: forall a b. String -> String -> Widget a b -> Widget a b
at' name value w = Propagator \outward -> do
  update <- unwrap w outward
  Web.Internal.DOMBuilder.at name value
  pure update

dat' :: forall a b. String -> (a -> String) -> Widget a b -> Widget a b
dat' name valuef w = Propagator \outward -> do
  update <- unwrap w outward
  node <- gets _.sibling
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence ch newa -> do
      setAttribute node name (valuef newa) -- TODO do not use directly DOM API
      update $ Occurrence ch newa

cl' :: forall a b. String -> Widget a b -> Widget a b
cl' name w = Propagator \outward -> do
  update <- unwrap w outward
  Web.Internal.DOMBuilder.cl name
  pure update

dcl' :: forall a b. String -> (a -> Boolean) -> Widget a b -> Widget a b
dcl' name pred w = Propagator \outward -> do
  update <- unwrap w outward
  node <- gets _.sibling
  pure case _ of
    Occurrence None _ -> mempty
    Occurrence ch newa -> do
      (if pred newa then addClass else removeClass) node name -- TODO do not use directly DOM API
      update $ Occurrence ch newa

div :: forall a b. Widget a b -> Widget a b
div = element "div"

span :: forall a b. Widget a b -> Widget a b
span = element "span"

aside :: forall a b. Widget a b -> Widget a b
aside = element "aside"

label :: forall a b. Widget a b -> Widget a b
label = element "label"

button :: forall a b. Widget a b -> Widget a b
button = element "button"

clickable :: forall a. Widget a Void -> Widget a a
clickable w = Propagator \outward -> do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  update <- unwrap w mempty
  node <- gets _.sibling
  liftEffect $ void $ addEventListener "click" node $ const $ Ref.read aRef >>= outward
  pure case _ of
    Occurrence None _ -> mempty
    cha -> do
      Ref.write cha aRef
      update cha

svg :: forall a b. Widget a b -> Widget a b
svg = element "svg"

path :: forall a b. Widget a b -> Widget a b
path = element "path"

p :: forall a b. Widget a b -> Widget a b
p = element "p"

h1 :: forall a b. Widget a b -> Widget a b
h1 = element "h1"

h2 :: forall a b. Widget a b -> Widget a b
h2 = element "h2"

h3 :: forall a b. Widget a b -> Widget a b
h3 = element "h3"

h4 :: forall a b. Widget a b -> Widget a b
h4 = element "h4"

h5 :: forall a b. Widget a b -> Widget a b
h5 = element "h5"

h6 :: forall a b. Widget a b -> Widget a b
h6 = element "h6"

-- Entry point

runWidgetInBody :: forall i o. Widget i o -> i -> Effect Unit
runWidgetInBody widget i = initializeInBody (unwrap widget mempty) (Occurrence Some i)

runWidgetInNode :: forall i o. Node -> Widget i o -> i -> (o -> Effect Unit) -> Effect Unit
runWidgetInNode node widget i outward = initializeInNode node (unwrap widget \(Occurrence _ o) -> outward o) (Occurrence Some i)

