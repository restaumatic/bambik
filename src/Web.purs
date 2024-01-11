module Web
  ( Widget
  , aside
  , at'
  , button
  , checkboxInput
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
  , label
  , p
  , path
  , radioButton
  , runWidgetInBody
  , runWidgetInNode
  , span
  , svg
  , text
  , textInput
  )
  where

import Prelude hiding (zero, div)

import Control.Monad.State (gets)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (debug)
import Effect.Ref as Ref
import Propagator (Change(..), Occurrence(..), Propagator(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Node, TagName, addClass, getChecked, getValue, removeAttribute, removeClass, setAttribute, setChecked, setTextNodeValue, setValue)
import Web.Internal.DOMBuilder (DOMBuilder, initializeInBody, initializeInNode, listener, speaker)
import Web.Internal.DOMBuilder as Web.Internal.DOMBuilder


-- Widget

type Widget i o = Propagator DOMBuilder i o

-- Primitive widgets

text :: forall a. Widget String a
text = Propagator \_ -> do
  Web.Internal.DOMBuilder.text
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence ch string -> do
      debug $ "setTextNodeValue " <> show ch
      liftEffect $ setTextNodeValue node string

-- TODO make it Widget String a
html :: forall a b. String -> Widget a b
html h = Propagator \_ -> do
  Web.Internal.DOMBuilder.html h
  pure $ const $ pure unit

textInput :: Widget String String
textInput = Propagator \outward -> do
  Web.Internal.DOMBuilder.element "input" (pure unit)
  Web.Internal.DOMBuilder.at "type" "text"
  listener "input" \_ -> do
    node <- gets _.sibling
    liftEffect (getValue node) >>= Occurrence Some >>> outward
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence _ newa -> do
      liftEffect $ setValue node newa

checkboxInput :: forall a . Widget (Maybe a) (Maybe (Maybe a))
checkboxInput = Propagator \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  Web.Internal.DOMBuilder.element "input" (pure unit)
  Web.Internal.DOMBuilder.at "type" "checkbox"
  listener "input" \_ -> do
    node <- gets _.sibling
    checked <- liftEffect $ getChecked node
    ma <- liftEffect $ Ref.read maRef
    outward $ Occurrence Some (if checked then Just ma else Nothing)
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence _ newma -> do
      liftEffect $ setChecked node (isJust newma)
      for_ newma \newa -> liftEffect $ Ref.write (Just newa) maRef

-- input:
-- Nothing -> turns off button
-- Just a -> turns on (if turned off) button and remembers `a`
-- output:
-- Nothing -> on button clicked when button doesn't remember any `a`
-- Just a -> on button clicked when button does remember an `a`
radioButton :: forall a. Widget (Maybe a) (Maybe a)
radioButton = Propagator \outward -> do
  maRef <- liftEffect $ Ref.new Nothing
  Web.Internal.DOMBuilder.element "input" (pure unit)
  Web.Internal.DOMBuilder.at "type" "radio"
  listener "change" \_ -> liftEffect (Ref.read maRef) >>= Occurrence Some >>> outward
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect $ setChecked node false
    Occurrence _ newma@(Just _) -> do
      liftEffect $ Ref.write newma maRef
      liftEffect $ setChecked node true

-- Widget optics

element :: forall a b. TagName -> Widget a b -> Widget a b
element tagName w = Propagator \outward -> do
  update <- Web.Internal.DOMBuilder.element tagName do
    unwrap w outward
  pure case _ of
    Occurrence None _ -> pure unit
    Occurrence ch newa -> do
      update $ Occurrence ch newa

at' :: forall a b. String -> String -> Widget a b -> Widget a b
at' name value w = Propagator \outward -> do
  update <- unwrap w outward
  Web.Internal.DOMBuilder.at name value
  pure update

dat' :: forall a b. String -> String -> (a -> Boolean) -> Widget a b -> Widget a b
dat' name value pred w = Propagator \outward -> do
  update <- unwrap w outward
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence ch newa -> do
      liftEffect $ if pred newa then setAttribute node name value else removeAttribute node name -- TODO do not use directly DOM API
      update $ Occurrence ch newa

cl' :: forall a b. String -> Widget a b -> Widget a b
cl' name w = Propagator \outward -> do
  update <- unwrap w outward
  Web.Internal.DOMBuilder.cl name
  pure update

dcl' :: forall a b. String -> (a -> Boolean) -> Widget a b -> Widget a b
dcl' name pred w = Propagator \outward -> do
  update <- unwrap w outward
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence ch newa -> do
      liftEffect $ (if pred newa then addClass else removeClass) node name -- TODO do not use directly DOM API
      update $ Occurrence ch newa

clickable :: forall a. Widget a Void -> Widget a a
clickable w = Propagator \outward -> do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  update <- unwrap w (const $ pure unit)
  listener "click" \_ -> liftEffect (Ref.read aRef) >>= outward
  pure case _ of
    Occurrence None _ -> pure unit
    cha -> do
      liftEffect $ Ref.write cha aRef
      update cha

--

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
runWidgetInBody widget i = initializeInBody (unwrap widget (const $ pure unit)) (Occurrence Some i)

runWidgetInNode :: forall i o. Node -> Widget i o -> i -> (o -> DOMBuilder Unit) -> Effect Unit
runWidgetInNode node widget i outward = initializeInNode node (unwrap widget \(Occurrence _ o) -> outward o) (Occurrence Some i)

