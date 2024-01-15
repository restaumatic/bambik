module SafeWeb where

import Prelude

import Control.Monad.State (gets)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String (null)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Propagator (Change(..), Occurrence(..))
import SafePropagator (SafePropagator)
import Unsafe.Coerce (unsafeCoerce)
import Web.Internal.DOM (Node, addClass, addEventListener, documentBody, getChecked, getValue, removeAttribute, removeClass, setAttribute, setChecked, setTextNodeValue, setValue)
import Web.Internal.DOMBuilder (DOMBuilder, runDomInNode)
import Web.Internal.DOMBuilder as DOMBuilder

type Widget i o = SafePropagator DOMBuilder i o

-- how Maybe _ input is handled by SafeWidgets
--                Nothing           Just _         default
-- text           empty text        text
-- html           no html           html
-- textInput      disables          enables
-- checkboxInput  deselects         selects        default select provided
-- radioButton    deselects         selects        default select provided
-- button         disables          enables
-- element        no-op             no-op
-- ?              detaches          attaches

text :: forall a . Widget String a -- TODO is default needed?
text = wrap do
  DOMBuilder.text
  node <- gets (_.sibling)
  pure
    { speak: case _ of
      Occurrence None _ -> pure unit
      Occurrence _ Nothing -> pure unit -- TODO is this correct?
      Occurrence _ (Just string) -> liftEffect $ setTextNodeValue node string
    , listen: \_ -> pure unit
    }

-- TODO make it Widget String a
html :: forall a b. String -> Widget a b
html h = wrap do
  DOMBuilder.html h
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

textInput :: Widget String String
textInput = wrap do
  DOMBuilder.element "input" (pure unit)
  DOMBuilder.at "type" "text"
  node <- gets _.sibling
  pure
    { speak: case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect do
      setAttribute node "disabled" "true"
      setValue node ""
    Occurrence _ (Just newa) -> liftEffect do
      setAttribute node "disabled" "false"
      setValue node newa
    , listen: \prop -> void $ liftEffect $ addEventListener "input" node $ const $ runDomInNode node do
      value <- liftEffect $ getValue node
      prop $ Occurrence Some (if null value then Nothing else Just value) -- TODO how to handle null value?
    }

checkboxInput :: forall a . a -> Widget (Maybe a) (Maybe a)
checkboxInput default = wrap do
  aRef <- liftEffect $ Ref.new default
  DOMBuilder.element "input" (pure unit)
  DOMBuilder.at "type" "checkbox"
  node <- gets _.sibling
  pure
    { speak: case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect $ setAttribute node "disabled" "true"
    Occurrence _ (Just Nothing) -> liftEffect $ do
      setAttribute node "disabled" "false"
      setChecked node false
    Occurrence _ (Just (Just newa)) -> liftEffect do
      setAttribute node "disabled" "false"
      setChecked node true
      Ref.write newa aRef
    , listen: \prop -> void $ liftEffect $ addEventListener "input" node $ const $ runDomInNode node do
      checked <- liftEffect $ getChecked node
      a <- liftEffect $ Ref.read aRef
      prop $ Occurrence Some $ Just $ if checked then (Just a) else Nothing
    }

radioButton :: forall a. a -> Widget a a
radioButton default = wrap do
  aRef <- liftEffect $ Ref.new default
  DOMBuilder.element "input" (pure unit)
  DOMBuilder.at "type" "radio"
  node <- gets _.sibling
  pure
    { speak: case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect $ setChecked node false
    Occurrence _ (Just newa) -> do
      liftEffect $ setChecked node true
      liftEffect $ Ref.write newa aRef
    , listen: \prop -> void $ liftEffect $ addEventListener "change" node $ const $ runDomInNode node do
    a <- liftEffect $ Ref.read aRef
    prop $ Occurrence Some (Just a)
    }

element :: forall i o. String -> Widget i o -> Widget i o
element tagName = wrap <<< DOMBuilder.element tagName <<< unwrap

at' :: forall a b. String -> String -> Widget a b -> Widget a b
at' name value w = wrap do
  w' <- unwrap w
  DOMBuilder.at name value
  pure w'

dat' :: forall a b. String -> String -> (a -> Boolean) -> Widget a b -> Widget a b
dat' name value pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { speak: \occur -> do
      w'.speak occur
      case occur of
        Occurrence None _ -> pure unit
        Occurrence _ Nothing -> pure unit -- TODO pred :: Maybe a -> Bool?
        Occurrence _ (Just newa) -> do
          liftEffect $ if pred newa then setAttribute node name value else removeAttribute node name -- TODO do not use directly DOM API
    , listen: w'.listen
    }

cl' :: forall a b. String -> Widget a b -> Widget a b
cl' name w = wrap do
  w' <- unwrap w
  DOMBuilder.cl name
  pure
    { speak: w'.speak
    , listen: w'.listen
    }

dcl' :: forall a b. String -> (a -> Boolean) -> Widget a b -> Widget a b
dcl' name pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { speak: \occur -> do
    w'.speak occur
    case occur of
      Occurrence None _ -> pure unit
      Occurrence _ Nothing -> pure unit -- TODO pred :: Maybe a -> Bool?
      Occurrence _ (Just newa) -> do
        liftEffect $ (if pred newa then addClass else removeClass) node name -- TODO do not use directly DOM API
    , listen: w'.listen
    }

clickable :: forall a. Widget a Void -> Widget a a
clickable w = wrap do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { speak: \occur -> do
    w'.speak occur
    case occur of
      Occurrence None _ -> pure unit
      Occurrence _ Nothing -> pure unit
      Occurrence _ (Just cha) -> do
        liftEffect $ Ref.write cha aRef
    , listen: \prop -> void $ liftEffect $ addEventListener "click" node $ const $ runDomInNode node do
    a <- liftEffect $ Ref.read aRef
    prop $ Occurrence Some (Just a)
    }

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

runWidgetInBody :: forall i o. Widget i o -> Maybe i -> Effect Unit
runWidgetInBody w mi = do
  node <- documentBody
  runWidgetInNode node w mi $ const $ pure unit

runWidgetInNode :: forall i o. Node -> Widget i o -> Maybe i -> (Maybe o -> DOMBuilder Unit) -> Effect Unit
runWidgetInNode node w mi outward = runDomInNode node do
  { speak, listen } <- unwrap w
  listen case _ of
    Occurrence None _ -> pure unit
    Occurrence _ mo -> outward mo
  speak (Occurrence Some mi)
