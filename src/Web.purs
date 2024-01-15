module Web where

import Prelude

import Control.Monad.State (gets)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String (null)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Propagator (Propagator, Change(..), Occurrence(..))
import Unsafe.Coerce (unsafeCoerce) -- TODO not relying on unsafe stuff
import Web.Internal.DocumentBuilder (DocumentBuilder, runDomInNode, Node, addClass, addEventListener, appendChild, createCommentNode, createDocumentFragment, documentBody, getChecked, getValue, insertAsFirstChild, insertBefore, moveAllNodesBetweenSiblings, removeAllNodesBetweenSiblings, removeAttribute, removeClass, setAttribute, setChecked, setTextNodeValue, setValue)
import Web.Internal.DocumentBuilder as DocumentBuilder

type Widget i o = Propagator DocumentBuilder i o

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
  DocumentBuilder.text
  node <- gets (_.sibling)
  pure
    { speak: case _ of
      Occurrence None _ -> pure unit
      Occurrence _ Nothing -> liftEffect $ setTextNodeValue node "" -- TODO is this correct?
      Occurrence _ (Just string) -> liftEffect $ setTextNodeValue node string
    , listen: \_ -> pure unit
    }

-- TODO make it Widget String a
html :: forall a b. String -> Widget a b
html h = wrap do
  DocumentBuilder.html h
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

textInput :: Widget String String
textInput = wrap do
  DocumentBuilder.element "input" (pure unit)
  DocumentBuilder.at "type" "text"
  node <- gets _.sibling
  pure
    { speak: case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect do
      setAttribute node "disabled" "true"
      setValue node ""
    Occurrence _ (Just newa) -> liftEffect do
      removeAttribute node "disabled"
      setValue node newa
    , listen: \prop -> void $ liftEffect $ addEventListener "input" node $ const $ runDomInNode node do
      value <- liftEffect $ getValue node
      prop $ Occurrence Some (if null value then Nothing else Just value) -- TODO how to handle null value?
    }

checkboxInput :: forall a . a -> Widget (Maybe a) (Maybe a)
checkboxInput default = wrap do
  aRef <- liftEffect $ Ref.new default
  DocumentBuilder.element "input" (pure unit)
  DocumentBuilder.at "type" "checkbox"
  node <- gets _.sibling
  pure
    { speak: case _ of
    Occurrence None _ -> pure unit
    Occurrence _ Nothing -> liftEffect $ setAttribute node "disabled" "true"
    Occurrence _ (Just Nothing) -> liftEffect $ do
      removeAttribute node "disabled"
      setChecked node false
    Occurrence _ (Just (Just newa)) -> liftEffect do
      removeAttribute node "disabled"
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
  DocumentBuilder.element "input" (pure unit)
  DocumentBuilder.at "type" "radio"
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
element tagName = wrap <<< DocumentBuilder.element tagName <<< unwrap

at' :: forall a b. String -> String -> Widget a b -> Widget a b
at' name value w = wrap do
  w' <- unwrap w
  DocumentBuilder.at name value
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
  DocumentBuilder.cl name
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

slot :: forall a b. Widget a b -> Widget a b
slot w = wrap do
  {update: { speak, listen}, attach, detach} <- attachable' false $ unwrap w
  pure
    { speak: \occur -> do
      speak occur
      case occur of
        (Occurrence None _) -> pure unit
        (Occurrence _ Nothing) -> detach
        (Occurrence _ (Just _)) -> attach
    , listen: listen
    }

attachable' :: forall a. Boolean -> DocumentBuilder a -> DocumentBuilder { update :: a, attach :: DocumentBuilder Unit, detach :: DocumentBuilder Unit }
attachable' removePrecedingSiblingNodes dom = do
  parent <- gets _.parent
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do

    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo

    (if removePrecedingSiblingNodes then insertAsFirstChild else appendChild) placeholderBefore parent
    appendChild placeholderAfter parent

    initialDocumentFragment <- createDocumentFragment
    update <- runDomInNode initialDocumentFragment dom

    detachedDocumentFragmentRef <- Ref.new $ Just initialDocumentFragment

    let
      attach :: DocumentBuilder Unit
      attach = measured' slotNo "attached" $ liftEffect do
        removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
        mDocumentFragment <- Ref.modify' (\documentFragment -> { state: Nothing, value: documentFragment}) detachedDocumentFragmentRef
        for_ mDocumentFragment \documentFragment -> documentFragment `insertBefore` placeholderAfter

      detach :: DocumentBuilder Unit
      detach = measured' slotNo "detached" $ liftEffect do
        mDocumentFragment <- Ref.read detachedDocumentFragmentRef
        case mDocumentFragment of
          Nothing -> do
            documentFragment <- createDocumentFragment
            moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
            Ref.write (Just documentFragment) detachedDocumentFragmentRef
          Just _ -> pure unit

    pure $ { attach, detach, update }
    where
      measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
      measured' slotNo actionName = measured $ "component " <> show slotNo <> " " <> actionName

measured :: forall a m. MonadEffect m ⇒ String → m a → m a
measured actionName action = do
  start <- liftEffect now
  _ <- liftEffect $ Ref.modify (_ + 1) logIndent
  a <- action
  currentIndent <- liftEffect $ Ref.modify (_ - 1) logIndent
  stop <- liftEffect now
  info $ "[DocumentBuilder] " <> repeatStr currentIndent "." <> actionName <> " in " <> show (unwrap (unInstant stop) - unwrap (unInstant start)) <> " ms"
  pure a
    where
      repeatStr i s
        | i <= 0 = ""
        | otherwise = s <> repeatStr (i - 1) s

logIndent :: Ref.Ref Int -- TODO ST?
logIndent = unsafePerformEffect $ Ref.new 0

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0

newPlaceholderBefore :: forall a. Show a ⇒ a → Effect Node
newPlaceholderBefore slotNo = createCommentNode $ "begin component " <> show slotNo

newPlaceholderAfter :: forall a. Show a ⇒ a → Effect Node
newPlaceholderAfter slotNo = createCommentNode $ "end component " <> show slotNo

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

runWidgetInNode :: forall i o. Node -> Widget i o -> Maybe i -> (Maybe o -> DocumentBuilder Unit) -> Effect Unit
runWidgetInNode node w mi outward = runDomInNode node do
  { speak, listen } <- unwrap w
  listen case _ of
    Occurrence None _ -> pure unit
    Occurrence _ mo -> outward mo
  speak (Occurrence Some mi)
