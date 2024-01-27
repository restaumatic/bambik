module Web
  ( Web
  , DocumentEnv
  , Node
  , aside
  , at'
  , button
  , checkboxInput
  , cl'
  , clickable
  , dat'
  , dcl'
  , div
  , element
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
  , slot
  , span
  , svg
  , text
  , textInput
  , uniqueId
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)
import Widget (Widget, Change(..))

-- Builds Web Document and keeping track of parent/last sibling node
newtype Web a = Web (StateT DocumentEnv Effect a)

type DocumentEnv =
  { parent :: Node
  , sibling :: Node -- last sibling
  }

derive newtype instance Functor Web
derive newtype instance Apply Web
derive newtype instance Applicative Web
derive newtype instance Bind Web
derive newtype instance Monad Web
derive newtype instance MonadEffect Web
derive newtype instance MonadState DocumentEnv Web

element :: forall a. TagName -> Web a -> Web a
element tagName contents = do
  newNode <- liftEffect $ createElement tagName
  parentNode <- gets _.parent
  liftEffect $ appendChild newNode parentNode
  modify_ _ { parent = newNode}
  result <- contents
  modify_ _ { parent = parentNode, sibling = newNode}
  pure result

at :: String -> String -> Web Unit
at name value = do
  node <- gets _.sibling
  liftEffect $ setAttribute node name value

ats :: Object String -> Web Unit
ats attrs = do
  node <- gets _.sibling
  liftEffect $ setAttributes node attrs

cl :: String -> Web Unit
cl name = do
  node <- gets _.sibling
  liftEffect $ addClass node name
  pure unit

listener :: String -> (Event -> Web Unit) -> Web Unit
listener eventType callback = do
  node <- gets _.sibling
  void $ liftEffect $ addEventListener eventType node (\evt -> runDomInNode node $ callback evt)

speaker :: forall a. (Node -> a) -> Web a
speaker action = do
  node <- gets _.sibling
  pure $ action node

uniqueId :: Effect String
uniqueId = randomElementId

runDomInNode :: forall a. Node -> Web a -> Effect a
runDomInNode node (Web domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node }

foreign import randomElementId :: Effect String

type TagName = String

-- | XML namespace URI.
type Namespace = String

-- | Web node.
foreign import data Node :: Type

-- | Web event.
foreign import data Event :: Type

foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import documentBody :: Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElementNS :: Namespace -> TagName -> Effect Node
foreign import createElement :: TagName -> Effect Node
foreign import insertBefore :: Node -> Node -> Effect Unit
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import removeAllNodesBetweenSiblings :: Node -> Node -> Effect Unit
foreign import appendRawHtml :: String -> Node -> Effect Node
foreign import moveAllNodesBetweenSiblings :: Node -> Node -> Node -> Effect Unit
foreign import addEventListener :: String -> Node -> (Event -> Effect Unit) -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import setAttributes :: Node -> Object String -> Effect Unit
foreign import setAttribute :: Node -> String -> String -> Effect Unit
foreign import removeAttribute :: Node -> String -> Effect Unit
foreign import addClass :: Node -> String -> Effect Unit
foreign import removeClass :: Node -> String -> Effect Unit
foreign import insertAsFirstChild :: Node -> Node -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
text :: forall a . Widget Web String a -- TODO is default needed?
text = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode mempty
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  node <- gets (_.sibling)
  pure
    { speak: case _ of
      None -> pure unit
      Removal -> liftEffect $ setTextNodeValue node "" -- TODO is this correct?
      Update _ string -> liftEffect $ setTextNodeValue node string
    , listen: \_ -> pure unit
    }

-- TODO make it Widget Web String a
html :: forall a b. String -> Widget Web a b
html htmlString = wrap do
  parent <- gets _.parent
  lastNode <- liftEffect $ appendRawHtml htmlString parent
  modify_ _ { sibling = lastNode}
  pure
    { speak: const $ pure unit
    , listen: const $ pure unit
    }

textInput :: Widget Web String String
textInput = wrap do
  element "input" (pure unit)
  at "type" "text"
  node <- gets _.sibling
  pure
    { speak: case _ of
    None -> pure unit
    Removal -> liftEffect do
      setAttribute node "disabled" "true"
      setValue node ""
    Update _ newa -> liftEffect do
      removeAttribute node "disabled"
      setValue node newa
    , listen: \prop -> void $ liftEffect $ addEventListener "input" node $ const $ runDomInNode node do
      value <- liftEffect $ getValue node
      prop $ Update [] value
    }

checkboxInput :: forall a . a -> Widget Web (Maybe a) (Maybe a)
checkboxInput default = wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  at "type" "checkbox"
  node <- gets _.sibling
  pure
    { speak: case _ of
    None -> pure unit
    Removal -> liftEffect $ setAttribute node "disabled" "true"
    Update _ Nothing -> liftEffect $ do
      removeAttribute node "disabled"
      setChecked node false
    Update _ (Just newa) -> liftEffect do
      removeAttribute node "disabled"
      setChecked node true
      Ref.write newa aRef
    , listen: \prop -> void $ liftEffect $ addEventListener "input" node $ const $ runDomInNode node do
      checked <- liftEffect $ getChecked node
      a <- liftEffect $ Ref.read aRef
      prop $ Update [] $ if checked then (Just a) else Nothing
    }

radioButton :: forall a. a -> Widget Web a a
radioButton default = wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  at "type" "radio"
  node <- gets _.sibling
  pure
    { speak: case _ of
    None -> pure unit
    Removal -> liftEffect $ setChecked node false
    Update _ newa -> do
      liftEffect $ setChecked node true
      liftEffect $ Ref.write newa aRef
    , listen: \prop -> void $ liftEffect $ addEventListener "change" node $ const $ runDomInNode node do
    a <- liftEffect $ Ref.read aRef
    prop $ Update [] a
    }

element' :: forall i o. String -> Widget Web i o -> Widget Web i o
element' tagName = wrap <<< element tagName <<< unwrap

at' :: forall a b. String -> String -> Widget Web a b -> Widget Web a b
at' name value w = wrap do
  w' <- unwrap w
  at name value
  pure w'

dat' :: forall a b. String -> String -> (a -> Boolean) -> Widget Web a b -> Widget Web a b
dat' name value pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { speak: \occur -> do
      w'.speak occur
      case occur of
        None -> pure unit
        Removal -> pure unit -- TODO pred :: Maybe a -> Bool?
        Update _ newa -> do
          liftEffect $ if pred newa then setAttribute node name value else removeAttribute node name -- TODO do not use directly DOM API
    , listen: w'.listen
    }

cl' :: forall a b. String -> Widget Web a b -> Widget Web a b
cl' name w = wrap do
  w' <- unwrap w
  cl name
  pure
    { speak: w'.speak
    , listen: w'.listen
    }

dcl' :: forall a b. String -> (a -> Boolean) -> Widget Web a b -> Widget Web a b
dcl' name pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { speak: \occur -> do
    w'.speak occur
    case occur of
      None -> pure unit
      Removal -> pure unit -- TODO pred :: Maybe a -> Bool?
      Update _ newa -> do
        liftEffect $ (if pred newa then addClass else removeClass) node name -- TODO do not use directly DOM API
    , listen: w'.listen
    }

clickable :: forall a. Widget Web a Void -> Widget Web a a
clickable w = wrap do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { speak: \occur -> do
    w'.speak occur
    case occur of
      None -> pure unit
      Removal -> pure unit
      Update _ a -> do
        liftEffect $ Ref.write a aRef
    , listen: \prop -> void $ liftEffect $ addEventListener "click" node $ const $ runDomInNode node do
    a <- liftEffect $ Ref.read aRef
    prop $ Update [] a
    }

slot :: forall a b. Widget Web a b -> Widget Web a b
slot w = wrap do
  {update: { speak, listen}, attach, detach} <- attachable' false $ unwrap w
  pure
    { speak: \occur -> do
      case occur of
        None -> pure unit
        Removal -> detach
        Update _ _ -> do
          speak occur
          attach
    , listen: listen
    }
  where
  attachable' :: forall x. Boolean -> Web x -> Web { update :: x, attach :: Web Unit, detach :: Web Unit }
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
        attach :: Web Unit
        attach = measured' slotNo "attached" $ liftEffect do
          detachedDocumentFragment <- Ref.modify' (\documentFragment -> { state: Nothing, value: documentFragment}) detachedDocumentFragmentRef
          for_ detachedDocumentFragment \documentFragment -> do
            removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
            documentFragment `insertBefore` placeholderAfter

        detach :: Web Unit
        detach = measured' slotNo "detached" $ liftEffect do
          detachedDocumentFragment <- Ref.read detachedDocumentFragmentRef
          when (isNothing detachedDocumentFragment) do
            documentFragment <- createDocumentFragment
            moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
            Ref.write (Just documentFragment) detachedDocumentFragmentRef

      pure $ { attach, detach, update }
      where
        measured' :: forall y m. MonadEffect m => Int -> String → m y → m y
        measured' slotNo actionName = measured $ "component " <> show slotNo <> " " <> actionName

measured :: forall a m. MonadEffect m ⇒ String → m a → m a
measured actionName action = do
  start <- liftEffect now
  _ <- liftEffect $ Ref.modify (_ + 1) logIndent
  a <- action
  currentIndent <- liftEffect $ Ref.modify (_ - 1) logIndent
  stop <- liftEffect now
  info $ "[Web] " <> repeatStr currentIndent "." <> actionName <> " in " <> show (unwrap (unInstant stop) - unwrap (unInstant start)) <> " ms"
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

div :: forall a b. Widget Web a b -> Widget Web a b
div = element' "div"

span :: forall a b. Widget Web a b -> Widget Web a b
span = element' "span"

aside :: forall a b. Widget Web a b -> Widget Web a b
aside = element' "aside"

label :: forall a b. Widget Web a b -> Widget Web a b
label = element' "label"

button :: forall a b. Widget Web a b -> Widget Web a b
button = element' "button"

svg :: forall a b. Widget Web a b -> Widget Web a b
svg = element' "svg"

path :: forall a b. Widget Web a b -> Widget Web a b
path = element' "path"

p :: forall a b. Widget Web a b -> Widget Web a b
p = element' "p"

h1 :: forall a b. Widget Web a b -> Widget Web a b
h1 = element' "h1"

h2 :: forall a b. Widget Web a b -> Widget Web a b
h2 = element' "h2"

h3 :: forall a b. Widget Web a b -> Widget Web a b
h3 = element' "h3"

h4 :: forall a b. Widget Web a b -> Widget Web a b
h4 = element' "h4"

h5 :: forall a b. Widget Web a b -> Widget Web a b
h5 = element' "h5"

h6 :: forall a b. Widget Web a b -> Widget Web a b
h6 = element' "h6"

-- Entry point

runWidgetInBody :: forall i o. Widget Web i o -> i -> Effect Unit
runWidgetInBody w i = do
  node <- documentBody
  runWidgetInNode node w i $ const $ pure unit

runWidgetInNode :: forall i o. Node -> Widget Web i o -> i -> (o -> Web Unit) -> Effect Unit
runWidgetInNode node w i outward = runDomInNode node do
  { speak, listen } <- unwrap w
  listen case _ of
    None -> pure unit
    Removal -> pure unit
    Update _ mo -> outward mo
  speak (Update [] i)
