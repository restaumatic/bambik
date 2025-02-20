module Web
  ( DocumentBuilderState
  , Node
  , Web
  , a
  , aside
  , attr
  , body
  , button
  , checkboxInput
  , cl
  , clDyn
  , div
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , html
  , i
  , init
  , input
  , label
  , li
  , ol
  , p
  , path
  , radioButton
  , runWidgetInNode
  , runWidgetInSelectedNode
  , slot
  , span
  , staticText
  , svg
  , text
  , textArea
  , ul
  , uniqueId
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import UI (New(..), PropagationStatus, UI, UIOcular)
import Unsafe.Coerce (unsafeCoerce)

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

-- UIs

text :: forall a. UI Web String a
text = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode ""
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  node <- gets (_.sibling)
  pure
    { toUser: case _ of
      New _ s _ -> setTextNodeValue node s
    , fromUser: \_ -> pure unit
    }

input :: String -> UI Web String String
input type_ = attr "type" type_ $ wrap do
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New _ newa _ -> setValue node newa
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      value <- getValue node
      void $ prop $ New [] value true
    }

textArea :: UI Web String String
textArea = wrap do
  element "textArea" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    (New _ newa _) -> setValue node newa
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      value <- getValue node
      void $ prop $ New [] value true
    }


checkboxInput :: forall a . a -> UI Web (Maybe a) (Maybe a)
checkboxInput default = attrDyn "disabled" "true" isNothing $ attr "type" "checkbox" $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New _ Nothing _ -> setChecked node false
    New _ (Just newa) _ -> do
      setChecked node true
      Ref.write newa aRef
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      checked <- getChecked node
      a <- Ref.read aRef
      void $ prop $ New [] (if checked then (Just a) else Nothing) false
    }

radioButton :: forall a. a -> UI Web (Maybe a) a
radioButton default = attr "type" "radio" $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New _ Nothing _ -> setChecked node false
    New _ (Just newa) _ -> do
      setChecked node true
      Ref.write newa aRef
    , fromUser: \prop -> void $ addEventListener "change" node $ const do
    a <- Ref.read aRef
    void $ prop $ New [] a false
    }

button :: forall a. UI Web a Void -> UI Web a a
button w = wrap do
  w' <- unwrap (el "button" >>> attrDyn "disabled" "true" isNothing $ w)
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  node <- gets _.sibling
  pure
    { toUser: \occur -> do
    status <- w'.toUser occur
    case occur of
      New _ a _ -> Ref.write a aRef
    pure status
    , fromUser: \prop -> void $ addEventListener "click" node $ const do
    a <- Ref.read aRef
    -- w'.toUser Nothing -- TODO check
    void $ prop $ New [] a false
    }

staticText :: forall a b. String -> UI Web a b
staticText text = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode text
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: mempty
    }

html :: forall a. String -> UI Web a a
html htmlString = wrap do
  parent <- gets _.parent
  lastNode <- liftEffect $ appendRawHtml htmlString parent
  modify_ _ { sibling = lastNode}
  pure
    { toUser: mempty
    , fromUser: mempty
    }

-- UIOculars

attr :: String -> String -> UIOcular Web
attr name value w = wrap do
  w' <- unwrap w
  attribute name value
  pure w'

cl :: String -> UIOcular Web
cl name w = wrap do
  w' <- unwrap w
  clazz name
  pure
    { toUser: w'.toUser
    , fromUser: w'.fromUser
    }

init :: forall a. (Node -> Effect a) -> (a -> Effect Unit) -> (a -> PropagationStatus -> Effect Unit) -> UIOcular Web
init nodeInitializer pre post w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  ctx <- liftEffect $ nodeInitializer node
  pure
    { toUser: \new -> do
        pre ctx
        w'.toUser new
    , fromUser: \prop -> do
      w'.fromUser \change -> do
        status <- prop change
        post ctx status
        pure status
    }

div :: UIOcular Web
div = el "div"

span :: UIOcular Web
span = el "span"

aside :: UIOcular Web
aside = el "aside"

label :: UIOcular Web
label = el "label"

svg :: UIOcular Web
svg = el "svg"

path :: UIOcular Web
path = el "path"

p :: UIOcular Web
p = el "p"

i :: UIOcular Web
i = el "i"

a :: UIOcular Web
a = el "a"

ul :: UIOcular Web
ul = el "ul"

ol :: UIOcular Web
ol = el "ol"

li :: UIOcular Web
li = el "li"

h1 :: UIOcular Web
h1 = el "h1"

h2 :: UIOcular Web
h2 = el "h2"

h3 :: UIOcular Web
h3 = el "h3"

h4 :: UIOcular Web
h4 = el "h4"

h5 :: UIOcular Web
h5 = el "h5"

h6 :: UIOcular Web
h6 = el "h6"

attrDyn :: String -> String -> (Maybe (New Unit) -> Boolean) -> UIOcular Web
attrDyn name value pred w = wrap do
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
      updateAttribute node mnewa = if pred (map (_ $> unit) $ mnewa) then setAttribute node name value else removeAttribute node name

clDyn :: String -> (Maybe (New Unit) -> Boolean) -> UIOcular Web
clDyn name pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ (if pred Nothing then addClass else removeClass) node name
  pure
    { toUser: \mch -> do
    (if pred (Just (mch $> unit)) then addClass else removeClass) node name
    w'.toUser mch
    , fromUser: w'.fromUser
    }

-- others

slot :: forall a b. UI Web a b -> UI Web (Maybe a) b
slot w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable false $ unwrap w
  pure
    { toUser: case _ of
      (New _ Nothing _) -> ensureDetached
      new@(New _ (Just y) _) -> do
        status <- toUser (new $> y)
        ensureAttached
        pure status
    , fromUser
    }
  where
  attachable :: forall r. Boolean -> Web r -> Web { result :: r, ensureAttached :: Effect Unit, ensureDetached :: Effect Unit }
  attachable removePrecedingSiblingNodes dom = do
    parent <- gets _.parent
    slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
    { ensureAttached, ensureDetached, initialDocumentFragment } <- liftEffect do
      placeholderBefore <- placeholderBeforeSlot slotNo
      placeholderAfter <- placeholderAfterSlot slotNo

      (if removePrecedingSiblingNodes then insertAsFirstChild else appendChild) placeholderBefore parent
      appendChild placeholderAfter parent

      initialDocumentFragment <- createDocumentFragment
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

      pure $ { ensureAttached, ensureDetached, initialDocumentFragment }
    modify_ _ { parent = initialDocumentFragment }
    result <- dom
    newSibling <- liftEffect $ lastChild initialDocumentFragment
    modify_ _ { parent = parent, sibling = newSibling}
    pure { ensureAttached, ensureDetached, result }
  placeholderBeforeSlot :: Int -> Effect Node
  placeholderBeforeSlot slotNo = createCommentNode $ "begin slot " <> show slotNo

  placeholderAfterSlot :: Int -> Effect Node
  placeholderAfterSlot slotNo = createCommentNode $ "end slot " <> show slotNo

-- Entry point

body :: forall t. UI Web Unit t -> Effect Unit
body w = do
  node <- documentBody
  runWidgetInNode node w

runWidgetInSelectedNode :: forall t. String -> UI Web Unit t -> Effect Unit
runWidgetInSelectedNode selector w = do
  node <- selectedNode selector
  runWidgetInNode node w

runWidgetInNode :: forall t. Node -> UI Web Unit t -> Effect Unit
runWidgetInNode node w = runDomInNode node do
  { toUser, fromUser } <- unwrap w
  liftEffect $ fromUser case _ of
    New _ mo _ -> pure Nothing
  void $ liftEffect $ toUser $ New [] unit false

--- private

el :: String -> UIOcular Web
el tagName = wrap <<< element tagName <<< unwrap

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

foreign import data Event :: Type
foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import documentBody :: Effect Node
foreign import selectedNode :: String -> Effect Node
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
foreign import lastChild :: Node -> Effect Node

runDomInNode :: forall a. Node -> Web a -> Effect a
runDomInNode node (Web domBuilder) = fst <$> runStateT domBuilder { sibling: node, parent: node }

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0
