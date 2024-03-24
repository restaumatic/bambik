module Web
  ( DocumentBuilderState
  , Node
  , Web
  , aside
  , attr
  , button
  , checkboxInput
  , cl
  , clickable
  , div
  , div'
  , dynAttr
  , dynClass
  , el
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
  , input
  , uniqueId
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)
import Widget (Changed(..), New(..), Widget)

foreign import data Node :: Type

-- Builds Web Document keeping track of parent/last sibling node
newtype Web a = Web (StateT DocumentBuilderState Effect a) -- TODO rename to DocumentBuilder?

type DocumentBuilderState =
  { parent :: Node
  , sibling :: Node -- last sibling
  }

derive newtype instance Functor Web
derive newtype instance Apply Web
derive newtype instance Applicative Web
derive newtype instance Bind Web
derive newtype instance Monad Web
derive newtype instance MonadEffect Web
derive newtype instance MonadState DocumentBuilderState Web

uniqueId :: Effect String
uniqueId = randomElementId

--

type Foo a b s t = Widget Web a b -> Widget Web s t

-- Primitives

text :: Widget Web String Void
text = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode mempty
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  node <- gets (_.sibling)
  pure
    { toUser: case _ of
      Removed -> setTextNodeValue node ""
      Altered (New _ string _) -> setTextNodeValue node string
    , fromUser: \_ -> pure unit
    }

-- TODO make it Widget Web String a?
html :: forall a b. String -> Widget Web a b
html htmlString = wrap do
  parent <- gets _.parent
  lastNode <- liftEffect $ appendRawHtml htmlString parent
  modify_ _ { sibling = lastNode}
  pure
    { toUser: const $ pure unit
    , fromUser: const $ pure unit
    }

input :: String -> Widget Web String String
input type_ = dynAttr "disabled" "true" (maybe true $ case _ of
    Altered _ -> false
    Removed -> true) $ wrap do
  element "input" (pure unit)
  attribute "type" type_
  node <- gets _.sibling
  pure
    { toUser: case _ of
    Removed -> setValue node ""
    Altered (New _ newa _) -> do
      setValue node newa
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      value <- getValue node
      prop $ New [] value true
    }

checkboxInput :: forall a . a -> Widget Web (Maybe a) (Maybe a)
checkboxInput default = dynAttr "disabled" "true" isNothing $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  attribute "type" "checkbox"
  node <- gets _.sibling
  pure
    { toUser: case _ of
    Removed -> pure unit
    Altered (New _ Nothing _) -> do
      setChecked node false
    Altered (New _ (Just newa) _) -> do
      setChecked node true
      Ref.write newa aRef
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      checked <- getChecked node
      a <- Ref.read aRef
      prop $ New [] (if checked then (Just a) else Nothing) false
    }

radioButton :: forall a. a -> Widget Web a a
radioButton default = dynAttr "disabled" "true" isNothing $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  attribute "type" "radio"
  node <- gets _.sibling
  pure
    { toUser: case _ of
    Removed -> setChecked node false
    Altered (New _ newa _) -> do
      setChecked node true
      Ref.write newa aRef
    , fromUser: \prop -> void $ addEventListener "change" node $ const do
    a <- Ref.read aRef
    prop $ New [] a false
    }

-- Optics

attr :: forall a b. String -> String -> Widget Web a b -> Widget Web a b
attr name value w = wrap do
  w' <- unwrap w
  attribute name value
  pure w'

dynAttr :: forall a b. String -> String -> (Maybe (Changed a) -> Boolean) -> Widget Web a b -> Widget Web a b
dynAttr name value pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ updateAttribute node Nothing
  pure
    { toUser: \mch -> do
      updateAttribute node $ Just mch
      w'.toUser mch
    , fromUser: w'.fromUser
    }
    where
      updateAttribute node mnewa = if pred mnewa then setAttribute node name value else removeAttribute node name

cl :: forall a b. String -> Widget Web a b -> Widget Web a b
cl name w = wrap do
  w' <- unwrap w
  clazz name
  pure
    { toUser: w'.toUser
    , fromUser: w'.fromUser
    }

dynClass :: forall a b. String -> (Maybe (Changed a) -> Boolean) -> Widget Web a b -> Widget Web a b
dynClass name pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ (if pred Nothing then addClass else removeClass) node name
  pure
    { toUser: \mch -> do
    (if pred (Just mch) then addClass else removeClass) node name
    w'.toUser mch
    , fromUser: w'.fromUser
    }

clickable :: forall a b. Widget Web a b -> Widget Web a a
clickable w = wrap do
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  w' <- unwrap (w # dynAttr "disabled" "true" (maybe true $ case _ of
    Altered _ -> false
    Removed -> true))
  node <- gets _.sibling
  pure
    { toUser: \occur -> do
    w'.toUser occur
    case occur of
      Removed -> Ref.write (unsafeCoerce unit) aRef
      Altered (New _ a _) -> Ref.write a aRef
    , fromUser: \prop -> void $ addEventListener "click" node $ const do
    a <- Ref.read aRef
    -- w'.toUser Nothing -- TODO check
    prop $ New [] a false
    }

slot :: forall a b. Widget Web a b -> Widget Web a b
slot w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable' false $ unwrap w
  pure
    { toUser: case _ of
      Removed -> ensureDetached
      updated@(Altered _) -> do
        toUser updated
        ensureAttached
    , fromUser: fromUser
    }
  where
  attachable' :: forall r. Boolean -> Web r -> Web { result :: r, ensureAttached :: Effect Unit, ensureDetached :: Effect Unit }
  attachable' removePrecedingSiblingNodes dom = do
    parent <- gets _.parent
    slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
    liftEffect do
      placeholderBefore <- newPlaceholderBefore slotNo
      placeholderAfter <- newPlaceholderAfter slotNo

      (if removePrecedingSiblingNodes then insertAsFirstChild else appendChild) placeholderBefore parent
      appendChild placeholderAfter parent

      initialDocumentFragment <- createDocumentFragment
      result <- runDomInNode initialDocumentFragment dom

      detachedDocumentFragmentRef <- Ref.new $ Just initialDocumentFragment

      let
        ensureAttached :: Effect Unit
        ensureAttached = do
          detachedDocumentFragment <- Ref.modify' (\documentFragment -> { state: Nothing, value: documentFragment}) detachedDocumentFragmentRef
          for_ detachedDocumentFragment \documentFragment -> do
            removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
            documentFragment `insertBefore` placeholderAfter

        ensureDetached :: Effect Unit
        ensureDetached = do
          detachedDocumentFragment <- Ref.read detachedDocumentFragmentRef
          when (isNothing detachedDocumentFragment) do
            documentFragment <- createDocumentFragment
            moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
            Ref.write (Just documentFragment) detachedDocumentFragmentRef

      pure $ { ensureAttached, ensureDetached, result }

el :: forall a b. String -> Widget Web a b -> Widget Web a b
el tagName = wrap <<< element tagName <<< unwrap

div :: forall a b. Widget Web a b -> Widget Web a b
div = el "div"

div' :: forall a b. { style :: String } -> Widget Web a b -> Widget Web a b
div' { style } w = el "div" w # attr "style" style

span :: forall a b. Widget Web a b -> Widget Web a b
span = el "span"

aside :: forall a b. Widget Web a b -> Widget Web a b
aside = el "aside"

label :: forall a b. Widget Web a b -> Widget Web a b
label = el "label"

button :: forall a b. Widget Web a b -> Widget Web a b
button = el "button"

svg :: forall a b. Widget Web a b -> Widget Web a b
svg = el "svg"

path :: forall a b. Widget Web a b -> Widget Web a b
path = el "path"

p :: forall a b. Widget Web a b -> Widget Web a b
p = el "p"

h1 :: forall a b. Widget Web a b -> Widget Web a b
h1 = el "h1"

h2 :: forall a b. Widget Web a b -> Widget Web a b
h2 = el "h2"

h3 :: forall a b. Widget Web a b -> Widget Web a b
h3 = el "h3"

h4 :: forall a b. Widget Web a b -> Widget Web a b
h4 = el "h4"

h5 :: forall a b. Widget Web a b -> Widget Web a b
h5 = el "h5"

h6 :: forall a b. Widget Web a b -> Widget Web a b
h6 = el "h6"

-- Entry point

runWidgetInBody :: forall o. Widget Web Unit o -> Effect Unit
runWidgetInBody w = do
  node <- documentBody
  runWidgetInNode node w unit mempty

runWidgetInNode :: forall i o. Node -> Widget Web i o -> i -> (o -> Effect Unit) -> Effect Unit
runWidgetInNode node w i outward = runDomInNode node do
  { toUser, fromUser } <- unwrap w
  liftEffect $ fromUser \(New _ mo _) -> outward mo
  liftEffect $ toUser $ Altered $ New [] i false

--- private

foreign import data Event :: Type
foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import documentBody :: Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElement :: String -> Effect Node
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
foreign import randomElementId :: Effect String

element :: forall a. String -> Web a -> Web a
element tagName contents = do
  newNode <- liftEffect $ createElement tagName
  parentNode <- gets _.parent
  liftEffect $ appendChild newNode parentNode
  modify_ _ { parent = newNode}
  result <- contents
  modify_ _ { parent = parentNode, sibling = newNode}
  pure result

attribute :: String -> String -> Web Unit
attribute name value = do
  node <- gets _.sibling
  liftEffect $ setAttribute node name value

-- read: class
clazz :: String -> Web Unit
clazz name = do
  node <- gets _.sibling
  liftEffect $ addClass node name
  pure unit

runDomInNode :: forall a. Node -> Web a -> Effect a
runDomInNode node (Web domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node }

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0

newPlaceholderBefore :: forall a. Show a ⇒ a → Effect Node
newPlaceholderBefore slotNo = createCommentNode $ "begin component " <> show slotNo

newPlaceholderAfter :: forall a. Show a ⇒ a → Effect Node
newPlaceholderAfter slotNo = createCommentNode $ "end component " <> show slotNo
