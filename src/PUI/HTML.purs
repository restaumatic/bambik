-- | The HTML vocabulary — a 1-1 correspondence with HTML: element oculars
-- | (`div`, `p`, `ul`, `li`, `a`, ...), attribute/class decorators
-- | (`attr`/`:=`, `cl`), the live leaves (`text`, `input`, `textArea`,
-- | `button`, ...), announcing statics (`staticText`, `staticHTML`, the void
-- | `hr` leaf), the `body` entry, and — for **structure computed from data at
-- | runtime** without a markup DSL — `dynamic` (build a whole widget from the
-- | fed value, rebuilt per feed; the single-value `foreach`). Grid cells emit
-- | their own identity by wrapping each built cell in `clicked` over a
-- | `lcmap (const key)` seed (`# clicked # lcmap (const key)`) — a per-cell
-- | listener, no `data-*` attribute — with `onClickedXY` the pointer-coordinate
-- | sibling for canvases. The carrier they are built over lives in `PUI.Web`.
module PUI.HTML
  ( (:=)
  , (:=>)
  , a
  , aside
  , attr
  , attrDyn
  , attrWith
  , body
  , button
  , checkboxInput
  , cl
  , clWhen
  , clicked
  , clDyn
  , blockquote
  , circle
  , code
  , div
  , dynamic
  , each
  , el
  , em
  , foreach
  , foreachModel
  , foreachWith
  , foreachWithModel
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , hr
  , i
  , img
  , init
  , input
  , inputDebounced
  , label
  , li
  , ol
  , onClickedXY
  , p
  , path
  , radioButton
  , runWidgetInNode
  , runWidgetInSelectedNode
  , span
  , staticHTML
  , staticText
  , strong
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
  , provided
  )
  where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Array (drop, length, zip) as Array
import Data.Default (class Default, default)
import Data.Foldable (for_)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (lcmap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import Record (get) as Record
import Type.Proxy (Proxy(..))
import PUI (PropagationStatus, PUI)
import PUI.Web (Node, Web, addClass, addEventListener, appendChild, appendRawHtml, attachable, attribute, clazz, createElementNS, createTextNode, documentBody, element, getChecked, getValue, htmlNS, isFocused, onClickXY, onInputDebounced, removeAllChildren, removeAttribute, removeClass, runDomInNode, selectedNode, setAttribute, setChecked, setTextNodeValue, setValue)
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

-- | The void `hr` element as announcing chrome (`{} → {}`): a self-closing
-- | rule, no content ocular needed.
hr :: PUI Web {} {}
hr = wrap do
  parent <- gets _.parent
  newNode <- liftEffect $ do
    node <- createElementNS htmlNS "hr"
    appendChild node parent
    pure node
  modify_ _ { sibling = newNode }
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

circle :: Ocular (PUI Web)
circle = el "circle"

img :: Ocular (PUI Web)
img = el "img"

strong :: Ocular (PUI Web)
strong = el "strong"

em :: Ocular (PUI Web)
em = el "em"

code :: Ocular (PUI Web)
code = el "code"

blockquote :: Ocular (PUI Web)
blockquote = el "blockquote"

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

-- | The view-model conditional: visibility is the **presence of data**, not a
-- | predicate. Feed `Just a` and the content is attached and fed `a`; feed
-- | `Nothing` and it is detached. Pair with a named `Maybe`-valued business
-- | projection — `pane # provided # lcmap currentQuestion` reads "shown,
-- | provided there is a current question" — so the pane consumes the payload,
-- | never the whole model, and the visibility logic lives in testable business
-- | code. Detachment means no echoes while absent: a pipeline-stage combinator,
-- | not a gated-merge operand.
provided :: forall a b. PUI Web a b -> PUI Web (Maybe a) b
provided w = wrap do
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
-- | exactly while the predicate holds for the value fed — styling, not
-- | visibility, so it stays a predicate (deliberately last-element-only:
-- | a class spread over several siblings is rarely what is meant).
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

-- | Value-computed attribute for the last-built element: the attribute is set
-- | to `valueOf i` on every value fed. The channel-fed counterpart of the
-- | static `attr`/`:=` — it lets **structure-from-data stay retaining**: a cell
-- | whose style, SVG coordinate, or colour depends on data is built once and
-- | updated in place through its channel (`circle >>> attrWith "cx" (show <<<
-- | _.x)`, `div >>> attrWith "style" cellStyle`), rather than rebuilt wholesale
-- | by a `dynamic`/`foreachWith` closure. Pair with `foreach` for a collection
-- | that never tears its elements down.
attrWith :: forall i o. String -> (i -> String) -> PUI Web i o -> PUI Web i o
attrWith name valueOf w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        setAttribute node name (valueOf i)
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

-- | Pointer-coordinate click emitter: emits the local/viewBox `{ x, y }` of a
-- | click on the container (an `<svg>` gives viewBox coords). A container-level
-- | emitter for canvases, where the coordinate is the payload: `svg [...] $
-- | onClickedXY $ dynamic renderScene`.
onClickedXY :: forall i o. PUI Web i o -> PUI Web i { x :: Number, y :: Number }
onClickedXY content = wrap do
  w' <- unwrap content
  node <- gets _.parent
  pure
    { toUser: w'.toUser
    , fromUser: \prop -> do
        w'.fromUser \_ -> pure Nothing
        onClickXY node \x y -> void $ prop { x, y }
    }

-- | The dynamic collection — the **runtime-sized homogeneous sequence merge**:
-- | one instance of the item widget per array element, all sharing the
-- | collection's output channel. Wrap it in a container ocular: `ul $
-- | foreach item`.
-- |
-- | **Retaining (PoC — sequence-merge branch).** The static row merges keep
-- | each operand's last contribution in a `Ref` and reuse it across feeds;
-- | this is that gate lifted to a runtime vector of element *instances*. On
-- | each fed array the collection **reconciles positionally** instead of
-- | rebuilding wholesale: survivors are re-fed through their channel (no DOM
-- | teardown — the win over the old `removeAllChildren`), entrants are built
-- | and appended, and a shrink falls back to a wholesale rebuild (the one
-- | remaining churn case, a keyed diff away). Because items are **channel-fed**
-- | (`w'.toUser item`), re-feeding a survivor updates it in place — unlike the
-- | closure-built `foreachWith`, whose content lives in the builder closure and
-- | so still rebuilds per value. See doc/collections-sequence-merge.md.
-- |
-- | Still collapses to `o`, so like a variant-output merge it cannot announce
-- | on an empty array (parametricity: no `o` to fabricate). For the
-- | structure-preserving, self-announcing stage use `foreachModel`.
foreach :: forall a o. PUI Web a o -> PUI Web (Array a) o
foreach w = wrap do
  parent <- gets _.parent
  propRef <- liftEffect $ Ref.new Nothing
  instancesRef <- liftEffect $ Ref.new []
  let
    build1 = do
      w' <- runDomInNode parent (unwrap w)
      mProp <- Ref.read propRef
      for_ mProp \prop -> w'.fromUser prop
      pure w'
  pure
    { toUser: \items -> do
        existing <- Ref.read instancesRef
        if Array.length items < Array.length existing then do
          -- shrink: wholesale rebuild (PoC fallback — a keyed diff would retain)
          removeAllChildren parent
          built <- for items \item -> do
            w' <- build1
            void $ w'.toUser item
            pure w'
          Ref.write built instancesRef
        else do
          -- reuse survivors (re-fed in place, no DOM teardown), append entrants
          for_ (Array.zip existing items) \(Tuple inst item) -> inst.toUser item
          entrants <- for (Array.drop (Array.length existing) items) \item -> do
            w' <- build1
            void $ w'.toUser item
            pure w'
          Ref.write (existing <> entrants) instancesRef
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | The **structure-preserving, self-announcing** sequence collection — the
-- | owned-output flavor with a nullary unit, as a self-contained stage. Renders
-- | `proj s` as a retaining `foreach` (element emissions dropped — this is a
-- | display) and **echoes the carrier `s`** on every feed, so it is an
-- | unconditional `PUI Web s s` pass-through that announces even when `proj s`
-- | is empty. This is the collection's answer to the empty-array starvation:
-- | where a terminal collection used to need `… # lcmap proj # displayed`, the
-- | announcing unit is built in — `ul $ foreachModel proj item`. Dissolves the
-- | `displayed` patch (doc/collections-sequence-merge.md).
foreachModel :: forall s a o. (s -> Array a) -> PUI Web a o -> PUI Web s s
foreachModel proj item = wrap do
  parent <- gets _.parent
  propRef <- liftEffect $ Ref.new Nothing
  instancesRef <- liftEffect $ Ref.new []
  let
    build1 = do
      w' <- runDomInNode parent (unwrap item)
      w'.fromUser \_ -> pure Nothing -- display-only: element emissions dropped
      pure w'
  pure
    { toUser: \s -> do
        let items = proj s
        existing <- Ref.read instancesRef
        if Array.length items < Array.length existing then do
          removeAllChildren parent
          built <- for items \i -> do
            w' <- build1
            void $ w'.toUser i
            pure w'
          Ref.write built instancesRef
        else do
          for_ (Array.zip existing items) \(Tuple inst i) -> inst.toUser i
          entrants <- for (Array.drop (Array.length existing) items) \i -> do
            w' <- build1
            void $ w'.toUser i
            pure w'
          Ref.write (existing <> entrants) instancesRef
        -- announcing unit: echo the carrier, so an empty collection never starves
        mProp <- Ref.read propRef
        for_ mProp \prop -> void $ prop s
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | **Structure computed from data at runtime**, in `PUI Web` rather than a
-- | markup DSL: the builder form of `foreach`. Each element of the fed array
-- | is turned into a whole widget *by the builder* — the value is in the
-- | builder's closure, so tags and attributes are ordinary computed strings
-- | (`el ("h" <> show level)`, `circle >>> "cx" := show c.x`) — and the enclosing
-- | element is rebuilt wholesale per value (clear once, append each). Like
-- | `foreach` it owns its container, so wrap it in a container ocular
-- | (`ul $ foreachWith item`, `svg [...] $ foreachWith circle`) and give any
-- | fixed siblings their own container. Built widgets are fed `{}` (content
-- | comes from the closure, not the channel); emissions share the output.
foreachWith :: forall a o. (a -> PUI Web {} o) -> PUI Web (Array a) o
foreachWith build = wrap do
  parent <- gets _.parent
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \items -> do
        removeAllChildren parent
        for_ items \item -> do
          w' <- runDomInNode parent (unwrap (build item))
          mProp <- Ref.read propRef
          for_ mProp \prop -> w'.fromUser prop
          void $ w'.toUser {}
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | The builder analogue of `foreachModel`: the **self-announcing** terminal
-- | display for **structure-from-value** collections, whose element structure
-- | genuinely varies (so it must be closure-built like `foreachWith`, not
-- | channel-fed). Renders `proj s` via the builder and **echoes the carrier
-- | `s`**, so it is an unconditional `PUI Web s s` pass-through that announces
-- | even when `proj s` is empty. Dissolves the `… # lcmap proj (foreachWith b)
-- | # displayed` idiom (`layoutCell $ foreachWithModel parseDoc block`).
foreachWithModel :: forall s a o. (s -> Array a) -> (a -> PUI Web {} o) -> PUI Web s s
foreachWithModel proj build = wrap do
  parent <- gets _.parent
  propRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \s -> do
        removeAllChildren parent
        for_ (proj s) \item -> do
          w' <- runDomInNode parent (unwrap (build item))
          w'.fromUser \_ -> pure Nothing -- display-only: element emissions dropped
          void $ w'.toUser {}
        mProp <- Ref.read propRef
        for_ mProp \prop -> void $ prop s -- announcing: echo the carrier
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | The single-value case of `foreachWith`: rebuild one widget from the fed
-- | value (a `foreachWith` over the one-element array). Owns its container:
-- | `svg [...] $ dynamic renderScene`, `div $ dynamic renderSwatch`.
dynamic :: forall a o. (a -> PUI Web {} o) -> PUI Web a o
dynamic build = lcmap (\a -> [ a ]) (foreachWith build)

-- | Build a **fixed** (closure-known) list of items into the container now —
-- | `foreachWith` fed a constant array, with the input pinned to `{}` so it
-- | drops into a `{} → {}` chrome merge without an annotation: `ul $ each rows
-- | renderRow`, `tr $ each cells cellWidget`.
each :: forall a o. Array a -> (a -> PUI Web {} o) -> PUI Web {} o
each items build = lcmap (\_ -> items) (foreachWith build)

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

