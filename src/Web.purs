module Web
  ( (:=)
  , (:=>)
  , DOM
  , Node
  , Web
  , a
  , aside
  , attr
  , attrDyn
  , body
  , button
  , checkboxInput
  , cl
  , clDyn
  , variant
  , div
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
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
  , span
  , staticHTML
  , staticText
  , svg
  , text
  , textArea
  , transient
  , ul
  , uniqueId
  )
  where

import Prelude

import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Default (class Default, default)
import Data.Foldable (for_)
import Data.Lens.Extra.Types (Ocular)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import UI (New(..), PropagationStatus, UI)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Node :: Type

-- Builds Web Document keeping track of parent/last sibling node
newtype Web a = Web (StateT DOM Effect a) -- TODO rename to DocumentBuilder?

type DOM =
  { parent :: Node
  , sibling :: Node -- last sibling
  }

derive newtype instance Functor Web
derive newtype instance Apply Web
derive newtype instance Applicative Web
derive newtype instance Bind Web
derive newtype instance Monad Web
derive newtype instance MonadEffect Web
derive newtype instance MonadState DOM Web

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
      New s _ -> setTextNodeValue node s
    , fromUser: \_ -> pure unit
    }

input :: String -> UI Web String String
input type_ = "type" := type_ $ wrap do
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New newa _ -> setValue node newa
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      value <- getValue node
      void $ prop $ New value true
    }

textArea :: UI Web String String
textArea = wrap do
  element "textArea" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    (New newa _) -> setValue node newa
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      value <- getValue node
      void $ prop $ New value true
    }


checkboxInput :: forall a . Default a => UI Web (Maybe a) (Maybe a)
checkboxInput = "disabled" :=> (\x -> if isNothing x then Just "true" else Nothing) $ "type" := "checkbox" $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New Nothing _ -> setChecked node false
    New (Just newa) _ -> do
      setChecked node true
      Ref.write newa aRef
    , fromUser: \prop -> void $ addEventListener "input" node $ const do
      checked <- getChecked node
      a <- Ref.read aRef
      void $ prop $ New (if checked then (Just a) else Nothing) false
    }

radioButton :: forall a. Default a => UI Web (Maybe a) a
radioButton = "type" := "radio" $ wrap do
  aRef <- liftEffect $ Ref.new default
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: case _ of
    New Nothing _ -> setChecked node false
    New (Just newa) _ -> do
      setChecked node true
      Ref.write newa aRef
    , fromUser: \prop -> void $ addEventListener "change" node $ const do
    a <- Ref.read aRef
    void $ prop $ New a false
    }

-- TODO disable button after click?
button :: forall a. UI Web a Void -> UI Web a a
button w = wrap do
  w' <- unwrap (el "button" >>> "disabled" :=> (\x -> if isNothing x then Just "true" else Nothing) $ w)
  aRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  node <- gets _.sibling
  pure
    { toUser: \occur -> do
    status <- w'.toUser occur
    case occur of
      New a _ -> Ref.write a aRef
    pure status
    , fromUser: \prop -> void $ addEventListener "click" node $ const do
    a <- Ref.read aRef
    -- w'.toUser Nothing -- TODO check
    setAttribute node "disabled" "true" -- TODO re-think
    void $ prop $ New a false
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

staticHTML :: forall a b. String -> UI Web a b
staticHTML html = wrap do
  parent <- gets _.parent
  newNode <- liftEffect $ appendRawHtml html parent
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: mempty
    }

-- UIOculars

attr :: String -> String -> Ocular (UI Web)
attr name value w = wrap do
  w' <- unwrap w
  attribute name value
  pure w'

infixr 10 attr as :=

cl :: String -> Ocular (UI Web)
cl name w = wrap do
  w' <- unwrap w
  clazz name
  pure
    { toUser: w'.toUser
    , fromUser: w'.fromUser
    }

init :: forall a. (Node -> Effect a) -> (a -> Effect Unit) -> (a -> PropagationStatus -> Effect Unit) -> Ocular (UI Web)
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

div :: Ocular (UI Web)
div = el "div"

span :: Ocular (UI Web)
span = el "span"

aside :: Ocular (UI Web)
aside = el "aside"

label :: Ocular (UI Web)
label = el "label"

svg :: Ocular (UI Web)
svg = el "svg"

path :: Ocular (UI Web)
path = el "path"

p :: Ocular (UI Web)
p = el "p"

i :: Ocular (UI Web)
i = el "i"

a :: Ocular (UI Web)
a = el "a"

ul :: Ocular (UI Web)
ul = el "ul"

ol :: Ocular (UI Web)
ol = el "ol"

li :: Ocular (UI Web)
li = el "li"

h1 :: Ocular (UI Web)
h1 = el "h1"

h2 :: Ocular (UI Web)
h2 = el "h2"

h3 :: Ocular (UI Web)
h3 = el "h3"

h4 :: Ocular (UI Web)
h4 = el "h4"

h5 :: Ocular (UI Web)
h5 = el "h5"

h6 :: Ocular (UI Web)
h6 = el "h6"

attrDyn :: String -> (Maybe (New Unit) -> Maybe String) -> Ocular (UI Web)
attrDyn name valueFunction w = wrap do
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
      updateAttribute node mnewa = case valueFunction (map (_ $> unit) $ mnewa) of
        Just value -> setAttribute node name value
        Nothing -> removeAttribute node name

infixr 10 attrDyn as :=>

clDyn :: String -> (Maybe (New Unit) -> Boolean) -> Ocular (UI Web)
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

-- Transient UI elements that appear temporarily and then disappear, for small content short focused interactions as opposed to long-term use or complex content. 
-- It wraps provided UI element with the following behaviour:
--   - when fed with a value (when `toUser` is called) it's ensured it's appearing
--   - when emiting a value (when `fromUser` is called) it disappears
transient :: Ocular (UI Web)
transient ui = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ unwrap ui
  pure
    { toUser: \new -> do
        status <- toUser new
        log "attaching transient UI"
        ensureAttached
        pure status
    , fromUser: \prop -> fromUser \x -> do
        log "detaching transient UI"
        ensureDetached
        log "detached transient UI"
        prop x
    }

variant :: forall a b. UI Web a b -> UI Web (Maybe a) b
variant w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ unwrap w
  pure
    { toUser: case _ of
      (New Nothing _) -> ensureDetached
      new@(New (Just y) _) -> do
        status <- toUser (new $> y)
        ensureAttached
        pure status
    , fromUser
    }

attachable :: forall r. Web r -> Web { result :: r, ensureAttached :: Effect Unit, ensureDetached :: Effect Unit }
attachable dom = do
  parent <- gets _.parent
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  { ensureAttached, ensureDetached, initialDocumentFragment } <- liftEffect do
    placeholderBefore <- placeholderBeforeSlot slotNo
    placeholderAfter <- placeholderAfterSlot slotNo

    appendChild placeholderBefore parent
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

body :: forall a b. Default a => UI Web a b -> Effect Unit
body w = do
  node <- documentBody
  runWidgetInNode node w

runWidgetInSelectedNode :: forall t. String -> UI Web Unit t -> Effect Unit
runWidgetInSelectedNode selector w = do
  node <- selectedNode selector
  runWidgetInNode node w

runWidgetInNode :: forall a t. Default a => Node -> UI Web a t -> Effect Unit
runWidgetInNode node w = runDomInNode node do
  { toUser, fromUser } <- unwrap w
  liftEffect $ fromUser case _ of
    New mo _ -> pure Nothing
  void $ liftEffect $ toUser $ New default false

--- private

el :: String -> Ocular (UI Web)
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
