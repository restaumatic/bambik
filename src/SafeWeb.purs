module SafeWeb where

import Prelude

import Control.Monad.State (gets)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String (null)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Propagator (Change(..), Occurrence(..))
import SafePropagator (SafePropagator)
import Web.Internal.DOM (Node, addEventListener, getChecked, getValue, setAttribute, setChecked, setTextNodeValue, setValue)
import Web.Internal.DOMBuilder (DOMBuilder, initializeInBody, initializeInNode, runDomInNode)
import Web.Internal.DOMBuilder as DOMBuilder

type SafeWidget i o = SafePropagator DOMBuilder i o

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

text :: String -> SafeWidget String Void
text default = wrap do
  DOMBuilder.text
  node <- gets (_.sibling)
  pure
    { speak: case _ of
      Occurrence None _ -> pure unit
      Occurrence _ mstring -> liftEffect $ setTextNodeValue node $ fromMaybe default mstring
    , listen: \_ -> pure unit
    }

-- TODO make it SafeWidget String a
html :: forall a b. String -> SafeWidget a b
html h = wrap do
  DOMBuilder.html h
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

textInput :: SafeWidget String String
textInput = wrap do
  DOMBuilder.element "input" (pure unit)
  DOMBuilder.at "type" "text"
  node <- gets _.sibling
  pure
    { speak: case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect $ setAttribute node "disabled" "true"
    Occurrence _ (Just newa) -> liftEffect do
      setAttribute node "disabled" "false"
      setValue node newa
    , listen: \prop -> void $ liftEffect $ addEventListener "input" node $ const $ runDomInNode node do
      value <- liftEffect $ getValue node
      prop $ Occurrence Some (if null value then Nothing else Just value) -- TODO how to handle null value?
    }

checkboxInput :: forall a . a -> SafeWidget a a
checkboxInput default = wrap do
  aRef <- liftEffect $ Ref.new default
  DOMBuilder.element "input" (pure unit)
  DOMBuilder.at "type" "checkbox"
  node <- gets _.sibling
  pure
    { speak: case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect $ setChecked node false
    Occurrence _ (Just newa) -> do
      liftEffect $ setChecked node true
      liftEffect $ Ref.write newa aRef
    , listen: \prop -> void $ liftEffect $ addEventListener "input" node $ const $ runDomInNode node do
      checked <- liftEffect $ getChecked node
      a <- liftEffect $ Ref.read aRef
      prop $ Occurrence Some $ if checked then (Just a) else Nothing
    }

radioButton :: forall a. a -> SafeWidget a a
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

element :: forall i o. String -> SafeWidget i o -> SafeWidget i o
element tagName = wrap <<< DOMBuilder.element tagName <<< unwrap

at' :: forall a b. String -> String -> SafeWidget a b -> SafeWidget a b
at' name value w = wrap do
  w' <- unwrap w
  DOMBuilder.at name value
  pure w'

dat' :: forall a b. String -> String -> (a -> Boolean) -> SafeWidget a b -> SafeWidget a b
dat' name value pred w = wrap do
  w' <- unwrap w
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence ch newa -> do
      liftEffect $ if pred newa then setAttribute node name value else removeAttribute node name -- TODO do not use directly DOM API
      update $ Occurrence ch newa

cl' :: forall a b. String -> SafeWidget a b -> SafeWidget a b
cl' name w = Propagator \outward -> do
  update <- unwrap w outward
  DOMBuilder.cl name
  pure update

dcl' :: forall a b. String -> (a -> Boolean) -> SafeWidget a b -> SafeWidget a b
dcl' name pred w = Propagator \outward -> do
  update <- unwrap w outward
  speaker \node -> case _ of
    Occurrence None _ -> pure unit
    Occurrence ch newa -> do
      liftEffect $ (if pred newa then addClass else removeClass) node name -- TODO do not use directly DOM API
      update $ Occurrence ch newa

clickable :: forall a. SafeWidget a Void -> SafeWidget a a
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

div :: forall a b. SafeWidget a b -> SafeWidget a b
div = element "div"

span :: forall a b. SafeWidget a b -> SafeWidget a b
span = element "span"

aside :: forall a b. SafeWidget a b -> SafeWidget a b
aside = element "aside"

label :: forall a b. SafeWidget a b -> SafeWidget a b
label = element "label"

button :: forall a b. SafeWidget a b -> SafeWidget a b
button = element "button"

svg :: forall a b. SafeWidget a b -> SafeWidget a b
svg = element "svg"

path :: forall a b. SafeWidget a b -> SafeWidget a b
path = element "path"

p :: forall a b. SafeWidget a b -> SafeWidget a b
p = element "p"

h1 :: forall a b. SafeWidget a b -> SafeWidget a b
h1 = element "h1"

h2 :: forall a b. SafeWidget a b -> SafeWidget a b
h2 = element "h2"

h3 :: forall a b. SafeWidget a b -> SafeWidget a b
h3 = element "h3"

h4 :: forall a b. SafeWidget a b -> SafeWidget a b
h4 = element "h4"

h5 :: forall a b. SafeWidget a b -> SafeWidget a b
h5 = element "h5"

h6 :: forall a b. SafeWidget a b -> SafeWidget a b
h6 = element "h6"

-- Entry point

runWidgetInBody :: forall i o. SafeWidget i o -> i -> Effect Unit
runWidgetInBody widget i = initializeInBody (unwrap widget (const $ pure unit)) (Occurrence Some i)

runWidgetInNode :: forall i o. Node -> SafeWidget i o -> i -> (o -> DOMBuilder Unit) -> Effect Unit
runWidgetInNode node widget i outward = initializeInNode node (unwrap widget \(Occurrence _ o) -> outward o) (Occurrence Some i)
