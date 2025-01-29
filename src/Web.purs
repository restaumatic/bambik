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
  , div
  , dynAttr
  , dynClass
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , html
  , img
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
import Unsafe.Coerce (unsafeCoerce)
import Widget (New(..), PropagationStatus, Widget, WidgetOcular, devoid)

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

-- Primitives

text :: forall a. Widget Web String a
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
      New _ s _ -> do
        setTextNodeValue node s
        pure Nothing
    , fromUser: \_ -> pure unit
    }

input :: String -> Widget Web String String
input type_ = dynAttr "disabled" "true" isNothing $ attr "type" type_ $ wrap do
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New _ newa _ -> do
      setValue node newa
      pure Nothing
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      value <- getValue node
      void $ prop $ New [] value true
    }

textArea :: Widget Web String String
textArea = dynAttr "disabled" "true" isNothing $ wrap do
  element "textArea" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    (New _ newa _) -> do
      setValue node newa
      pure Nothing
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      value <- getValue node
      void $ prop $ New [] value true
    }


checkboxInput :: forall a . a -> Widget Web (Maybe a) (Maybe a)
checkboxInput default = dynAttr "disabled" "true" isNothing $ attr "type" "checkbox" $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New _ Nothing _ -> do
      setChecked node false
      pure Nothing
    New _ (Just newa) _ -> do
      setChecked node true
      Ref.write newa aRef
      pure Nothing
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      checked <- getChecked node
      a <- Ref.read aRef
      void $ prop $ New [] (if checked then (Just a) else Nothing) false
    }

radioButton :: forall a. a -> Widget Web (Maybe a) a
radioButton default = dynAttr "disabled" "true" isNothing $ attr "type" "radio" $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New _ Nothing _ -> do
      setChecked node false
      pure Nothing
    New _ (Just newa) _ -> do
      setChecked node true
      Ref.write newa aRef
      pure Nothing
    , fromUser: \prop -> void $ addEventListener "change" node $ const do
    a <- Ref.read aRef
    void $ prop $ New [] a false
    }

button :: forall a. Widget Web a Void -> Widget Web a a
button w = wrap do
  w' <- unwrap (el "button" >>> dynAttr "disabled" "true" isNothing $ w)
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

-- Statics

html :: forall a. String -> Widget Web a a
html htmlString = wrap do
  parent <- gets _.parent
  lastNode <- liftEffect $ appendRawHtml htmlString parent
  modify_ _ { sibling = lastNode}
  pure
    { toUser: mempty
    , fromUser: mempty
    }

img :: forall a. String -> Widget Web a a
img src = el "img" >>> attr "src" src $ devoid

-- Oculars

attr :: String -> String -> WidgetOcular Web
attr name value w = wrap do
  w' <- unwrap w
  attribute name value
  pure w'

cl :: String -> WidgetOcular Web
cl name w = wrap do
  w' <- unwrap w
  clazz name
  pure
    { toUser: w'.toUser
    , fromUser: w'.fromUser
    }

init :: forall a. (Node -> Effect a) -> (a -> Effect Unit) -> (a -> PropagationStatus -> Effect Unit) -> WidgetOcular Web
init nodeInitializer pre post w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  mCtxRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: case _ of
      altered -> do
        status <- w'.toUser altered
        mCtx <- liftEffect $ Ref.read mCtxRef
        liftEffect $ case mCtx of
          Nothing -> do
            ctx <- nodeInitializer node
            Ref.write (Just ctx) mCtxRef
            pre ctx
          Just ctx -> pre ctx
        pure status
    , fromUser: \prop -> do
      w'.fromUser \change -> do
        mCtx <- liftEffect $ Ref.read mCtxRef
        liftEffect $ case mCtx of
          Nothing -> unsafeCoerce unit -- should never happen
          Just ctx -> do
            status <- prop change
            post ctx status
            pure status
    }

div :: WidgetOcular Web
div = el "div"

span :: WidgetOcular Web
span = el "span"

aside :: WidgetOcular Web
aside = el "aside"

label :: WidgetOcular Web
label = el "label"

svg :: WidgetOcular Web
svg = el "svg"

path :: WidgetOcular Web
path = el "path"

p :: WidgetOcular Web
p = el "p"

a :: WidgetOcular Web
a = el "a"

ul :: WidgetOcular Web
ul = el "ul"

ol :: WidgetOcular Web
ol = el "ol"

li :: WidgetOcular Web
li = el "li"

h1 :: WidgetOcular Web
h1 = el "h1"

h2 :: WidgetOcular Web
h2 = el "h2"

h3 :: WidgetOcular Web
h3 = el "h3"

h4 :: WidgetOcular Web
h4 = el "h4"

h5 :: WidgetOcular Web
h5 = el "h5"

h6 :: WidgetOcular Web
h6 = el "h6"

dynAttr :: String -> String -> (Maybe (New Unit) -> Boolean) -> WidgetOcular Web
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
      updateAttribute node mnewa = if pred (map (_ $> unit) $ mnewa) then setAttribute node name value else removeAttribute node name

dynClass :: String -> (Maybe (New Unit) -> Boolean) -> WidgetOcular Web
dynClass name pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ (if pred Nothing then addClass else removeClass) node name
  pure
    { toUser: \mch -> do
    (if pred (Just (mch $> unit)) then addClass else removeClass) node name
    w'.toUser mch
    , fromUser: w'.fromUser
    }

-- Entry point

body :: Widget Web Unit Void -> Effect Unit
body w = do
  node <- documentBody
  runWidgetInNode node w

runWidgetInSelectedNode :: String -> Widget Web Unit Void -> Effect Unit
runWidgetInSelectedNode selector w = do
  node <- selectedNode selector
  runWidgetInNode node w

runWidgetInNode :: Node -> Widget Web Unit Void -> Effect Unit
runWidgetInNode node w = runDomInNode node do
  { toUser, fromUser } <- unwrap w
  liftEffect $ fromUser case _ of
    New _ mo _ -> pure Nothing
  void $ liftEffect $ toUser $ New [] unit false

--- private

el :: String -> WidgetOcular Web
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

slot :: forall a b. Widget Web a b -> Widget Web (Maybe a) b
slot w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable false $ unwrap w
  pure
    { toUser: case _ of
      (New _ Nothing _) -> do
        ensureDetached
        pure Nothing
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
