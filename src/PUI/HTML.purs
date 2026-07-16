-- | The HTML vocabulary — a 1-1 correspondence with HTML: element oculars
-- | (`div`, `p`, `ul`, `li`, `a`, ...), attribute/class decorators
-- | (`attr`/`:=`, `cl`), the live leaves (`text`, `input`, `textArea`,
-- | `button`, ...), announcing statics (`staticText`, `staticHTML`), the
-- | `body` entry, and the custom-leaf toolkit (`viewEvents`, `escapeHtml`).
-- | The carrier they are built over lives in `PUI.Web`.
module PUI.HTML
  ( (:=)
  , (:=>)
  , a
  , aside
  , attr
  , attrDyn
  , body
  , button
  , checkboxInput
  , cl
  , clWhen
  , clicked
  , clDyn
  , div
  , escapeHtml
  , foreach
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , i
  , init
  , input
  , inputDebounced
  , label
  , li
  , ol
  , p
  , path
  , radioButton
  , runWidgetInNode
  , runWidgetInSelectedNode
  , shownWhen
  , span
  , staticHTML
  , staticText
  , svg
  , table
  , tbody
  , td
  , text
  , textArea
  , th
  , thead
  , tr
  , transient
  , ul
  , variant
  , viewEvents
  )
  where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Default (class Default, default)
import Data.Foldable (for_)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import Record (get) as Record
import Type.Proxy (Proxy(..))
import PUI (PropagationStatus, PUI)
import PUI.Web (Node, Web, addClass, addEventListener, appendChild, appendRawHtml, attachable, attribute, clazz, createTextNode, documentBody, element, getChecked, getValue, isFocused, onInputDebounced, onKeyClick, removeAllChildren, removeAttribute, removeClass, runDomInNode, selectedNode, setAttribute, setChecked, setInnerHTML, setTextNodeValue, setValue)
import Unsafe.Coerce (unsafeCoerce)

-- UIs

text :: PUI Web { value :: String } {}
text = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode ""
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  node <- gets (_.sibling)
  propRef <- liftEffect $ Ref.new $ unsafeCoerce unit
  pure
    { toUser: \s -> do
        setTextNodeValue node s.value
        prop <- Ref.read propRef
        void $ prop {}
    , fromUser: \prop -> Ref.write prop propRef
    }

-- | Model updates never clobber the field the user is typing in: `toUser`
-- | skips `setValue` while the node is focused (but still echoes, so merge
-- | gates and downstream stages keep flowing). The channel stays live for
-- | the field's whole life — an edited field resumes showing model updates
-- | the moment it loses focus.
input :: String -> PUI Web String String
input type_ = "type" := type_ $ wrap do
  element "input" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new $ Nothing
  pure
    { toUser: \newa -> do
      focused <- isFocused node
      unless focused $ setValue node newa
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> void $ prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      void $ addEventListener "input" node $ const do
        value <- getValue node
        void $ prop value
    }

-- | `input` with the DOM events debounced **at the leaf**: keystrokes are
-- | coalesced before they enter the wire, so everything downstream of an
-- | emission stays synchronous and `looped`'s re-entrancy guard still
-- | terminates loop cycles. (Wire-level debouncing inside a loop turns
-- | refeeds into a standing async ping-pong — the delay must sit in front
-- | of the wire, not on it.)
inputDebounced :: Milliseconds -> String -> PUI Web String String
inputDebounced (Milliseconds millis) type_ = "type" := type_ $ wrap do
  element "input" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new $ Nothing
  pure
    { toUser: \newa -> do
      focused <- isFocused node
      unless focused $ setValue node newa
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> void $ prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      onInputDebounced node millis \value -> void $ prop value
    }

-- | See `input` — same focus-guarded protocol.
textArea :: PUI Web String String
textArea = wrap do
  element "textArea" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \newa -> do
      focused <- isFocused node
      unless focused $ setValue node newa
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> void $ prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      void $ addEventListener "input" node $ const do
        value <- getValue node
        void $ prop value
    }


checkboxInput :: forall a . Default a => PUI Web (Maybe a) (Maybe a)
checkboxInput = "disabled" :=> (\x -> if isNothing x then Just "true" else Nothing) $ "type" := "checkbox" $ wrap do
  aRef <- liftEffect $ Ref.new default
  mPropRef <- liftEffect $ Ref.new Nothing
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: \ma -> do
        case ma of
          Nothing -> setChecked node false
          Just newa -> do
            setChecked node true
            Ref.write newa aRef
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop ma
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "input" node $ const do
          checked <- getChecked node
          a <- Ref.read aRef
          void $ prop (if checked then (Just a) else Nothing)
    }

radioButton :: forall a. Default a => PUI Web (Maybe a) a
radioButton = "type" := "radio" $ wrap do
  aRef <- liftEffect $ Ref.new default
  mPropRef <- liftEffect $ Ref.new Nothing
  element "input" (pure unit)
  node <- gets _.sibling
  pure
    { toUser: \ma -> do
        case ma of
          Nothing -> setChecked node false
          Just newa -> do
            setChecked node true
            Ref.write newa aRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \newa -> void $ prop newa
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "change" node $ const do
          a <- Ref.read aRef
          void $ prop a
    }

-- TODO disable button after click?
-- | Content is chrome (`{} → {}`, announcing): a button contains decoration
-- | only; its wiring is the click emitter, replaying the last value fed.
button :: forall a. PUI Web {} {} -> PUI Web a a
button w = wrap do
  w' <- unwrap (el "button" >>> "disabled" :=> (\x -> if isNothing x then Just "true" else Nothing) $ w)
  -- a click before any value arrived has nothing valid to emit — withheld
  mARef <- liftEffect $ Ref.new Nothing
  node <- gets _.sibling
  pure
    { toUser: \occur -> do
        status <- w'.toUser {}
        Ref.write (Just occur) mARef
        pure status
    , fromUser: \prop -> void $ addEventListener "click" node $ const do
        mA <- Ref.read mARef
        for_ mA \a -> do
          setAttribute node "disabled" "true" -- TODO re-think
          void $ prop a
    }

-- | Static text as the announcing record unit with a face (`{} → {}`):
-- | fixed DOM and, like `RecordToRecord.pempty`, it announces its
-- | informationless `{}` on registration — so chrome composes as a gated
-- | record-merge operand without starving anything.
staticText :: String -> PUI Web {} {}
staticText text = wrap do
  parentNode <- gets _.parent
  newNode <- liftEffect $ do
    node <- createTextNode text
    appendChild node parentNode
    pure node
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: \prop -> void $ prop {}
    }

-- | See `staticText` — same announcing chrome typing.
staticHTML :: String -> PUI Web {} {}
staticHTML html = wrap do
  parent <- gets _.parent
  newNode <- liftEffect $ appendRawHtml html parent
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: \prop -> void $ prop {}
    }

-- UIOculars

attr :: String -> String -> Ocular (PUI Web)
attr name value w = wrap do
  w' <- unwrap w
  attribute name value
  pure w'

infixr 10 attr as :=

cl :: String -> Ocular (PUI Web)
cl name w = wrap do
  w' <- unwrap w
  clazz name
  pure
    { toUser: w'.toUser
    , fromUser: w'.fromUser
    }

init :: forall a. (Node -> Effect a) -> (a -> Effect Unit) -> (a -> PropagationStatus -> Effect Unit) -> Ocular (PUI Web)
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

div :: Ocular (PUI Web)
div = el "div"

span :: Ocular (PUI Web)
span = el "span"

aside :: Ocular (PUI Web)
aside = el "aside"

label :: Ocular (PUI Web)
label = el "label"

svg :: Ocular (PUI Web)
svg = el "svg"

path :: Ocular (PUI Web)
path = el "path"

p :: Ocular (PUI Web)
p = el "p"

i :: Ocular (PUI Web)
i = el "i"

a :: Ocular (PUI Web)
a = el "a"

ul :: Ocular (PUI Web)
ul = el "ul"

ol :: Ocular (PUI Web)
ol = el "ol"

li :: Ocular (PUI Web)
li = el "li"

-- table elements get real oculars (not `staticHTML`): the raw-HTML parser
-- drops `tr`/`td`/`thead` fragments outside a table context
table :: Ocular (PUI Web)
table = el "table"

thead :: Ocular (PUI Web)
thead = el "thead"

tbody :: Ocular (PUI Web)
tbody = el "tbody"

tr :: Ocular (PUI Web)
tr = el "tr"

th :: Ocular (PUI Web)
th = el "th"

td :: Ocular (PUI Web)
td = el "td"

h1 :: Ocular (PUI Web)
h1 = el "h1"

h2 :: Ocular (PUI Web)
h2 = el "h2"

h3 :: Ocular (PUI Web)
h3 = el "h3"

h4 :: Ocular (PUI Web)
h4 = el "h4"

h5 :: Ocular (PUI Web)
h5 = el "h5"

h6 :: Ocular (PUI Web)
h6 = el "h6"

attrDyn :: String -> (Maybe Unit -> Maybe String) -> Ocular (PUI Web)
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
      updateAttribute node mnewa = case valueFunction (mnewa $> unit) of
        Just value -> setAttribute node name value
        Nothing -> removeAttribute node name

infixr 10 attrDyn as :=>

clDyn :: String -> (Maybe Unit -> Boolean) -> Ocular (PUI Web)
clDyn name pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ (if pred Nothing then addClass else removeClass) node name
  pure
    { toUser: \mch -> do
    (if pred (Just unit) then addClass else removeClass) node name
    w'.toUser mch
    , fromUser: w'.fromUser
    }

-- | Value-aware visibility for a record-merge operand: the wrapped element
-- | stays in the DOM — detachment would starve the merge gates, since a
-- | detached editor's wiring cannot echo — but is displayed only while the
-- | predicate holds for the value fed. (Single-element content: the toggle
-- | lands on the content's root node.)
shownWhen :: forall i o. (i -> Boolean) -> PUI Web i o -> PUI Web i o
shownWhen pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ setAttribute node "style" "display: none;"
  pure
    { toUser: \i -> do
        if pred i
          then removeAttribute node "style"
          else setAttribute node "style" "display: none;"
        w'.toUser i
    , fromUser: w'.fromUser
    }


-- Transient PUI elements that appear temporarily and then disappear, for small content short focused interactions as opposed to long-term use or complex content.
-- It wraps provided PUI element with the following behaviour:
--   - when fed with a value (when `toUser` is called) it's ensured it's appearing
--   - when emiting a value (when `fromUser` is called) it disappears
transient :: Ocular (PUI Web)
transient ui = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ unwrap ui
  pure
    { toUser: \new -> do
        status <- toUser new
        ensureAttached
        pure status
    , fromUser: \prop -> fromUser \x -> do
        ensureDetached
        prop x
    }

variant :: forall a b. PUI Web a b -> PUI Web (Maybe a) b
variant w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ unwrap w
  pure
    { toUser: case _ of
      Nothing -> ensureDetached
      Just y -> do
        status <- toUser y
        ensureAttached
        pure status
    , fromUser
    }


-- | Value-dependent class for the last-built element: the class is present
-- | exactly while the predicate holds for the value fed. (`shownWhen`'s
-- | pattern, at class granularity.)
clWhen :: forall i o. (i -> Boolean) -> String -> PUI Web i o -> PUI Web i o
clWhen pred name w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        (if pred i then addClass else removeClass) node name
        w'.toUser i
    , fromUser: w'.fromUser
    }

-- | Make the last-built element a click emitter: content is display, the
-- | element replays the last value fed on click — `button`'s protocol for
-- | any element (a click before any value arrived is withheld).
clicked :: forall i o. PUI Web i o -> PUI Web i i
clicked w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  iRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \i -> do
        Ref.write (Just i) iRef
        w'.toUser i
    , fromUser: \prop -> do
        -- content is display-only: give its wiring a sink so echoes flow
        w'.fromUser \_ -> pure Nothing
        void $ addEventListener "click" node $ const do
          mi <- Ref.read iRef
          for_ mi \i -> void $ prop i
    }

-- | The dynamic collection: one instance of the item widget per array
-- | element, rebuilt in the enclosing element on every value fed; every
-- | instance's emissions share the collection's output channel. Wrap it in
-- | a container ocular: `ul $ foreach item`.
foreach :: forall a o. PUI Web a o -> PUI Web (Array a) o
foreach w = wrap do
  parent <- gets _.parent
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \items -> do
        removeAllChildren parent
        for_ items \item -> do
          w' <- runDomInNode parent (unwrap w)
          mProp <- Ref.read propRef
          for_ mProp \prop -> w'.fromUser prop
          void $ w'.toUser item
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- Entry point

-- | The app entry: builds the widget in `<body>` and registers its
-- | wiring — and feeds **nothing**. All initial data enters as seeds
-- | (`with initial`, `announce`, `seeded`) inside the widget itself, so
-- | the standalone app reads `body $ with initial $ ...`; emissions are
-- | simply dropped.
body :: forall i o. PUI Web i o -> Effect Unit
body ui = do
  node <- documentBody
  runDomInNode node do
    { fromUser } <- unwrap ui
    liftEffect $ fromUser \_ -> pure Nothing

-- | Build a `×→+` **view-with-events leaf**: `shell` is the container
-- | markup (appended once), `render` fills it per value fed (no echo —
-- | variant outputs don't echo), and `wire` attaches the event emitters to
-- | the container node. Events carry **bare payloads** — pair them with
-- | the model in an `updates` fold, not in the leaf.
viewEvents :: forall i o. String -> (i -> String) -> (Node -> (o -> Effect Unit) -> Effect Unit) -> PUI Web i o
viewEvents shell render wire = wrap do
  _ <- unwrap (staticHTML shell)
  node <- gets _.sibling
  pure
    { toUser: \i -> setInnerHTML node (render i)
    , fromUser: \prop -> wire node (void <<< prop)
    }

-- | Escape text for interpolation into `viewEvents` render output.
escapeHtml :: String -> String
escapeHtml s =
  replaceAll (Pattern "\"") (Replacement "&quot;")
    (replaceAll (Pattern ">") (Replacement "&gt;")
      (replaceAll (Pattern "<") (Replacement "&lt;")
        (replaceAll (Pattern "&") (Replacement "&amp;") s)))

runWidgetInSelectedNode :: forall a b. String -> a -> (b -> Effect Unit) -> PUI Web a b -> Effect Unit
runWidgetInSelectedNode selector initial callback ui = do
  node <- selectedNode selector
  runWidgetInNode node initial callback ui

runWidgetInNode :: forall a b. Node -> a -> (b -> Effect Unit) -> PUI Web a b -> Effect Unit
runWidgetInNode node initial callback ui = runDomInNode node do
  { toUser, fromUser } <- unwrap ui
  liftEffect $ fromUser \b -> do
    callback b
    pure Nothing
  void $ liftEffect $ toUser initial


--- private

el :: String -> Ocular (PUI Web)
el tagName = wrap <<< element tagName <<< unwrap

