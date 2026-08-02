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
  , article
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
  , code
  , div
  , dynamic
  , each
  , el
  , em
  , footer
  , foreachWith
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , header
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
  , radioButton
  , runWidgetInNode
  , runWidgetInSelectedNode
  , section
  , span
  , staticHTML
  , staticText
  , strong
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
  , atCase
  , provided
  )
  where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Default (class Default, default)
import Data.Foldable (for_)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (class IsSymbol)
import Data.Variant (prj)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import Record (get) as Record
import Type.Proxy (Proxy(..))
import PUI (PUI)
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
        prop {}
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
      for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      void $ addEventListener "input" node $ const do
        value <- getValue node
        prop value
    }

-- | `input` with the DOM events debounced **at the leaf**: keystrokes are
-- | coalesced before they enter the wire, so everything downstream of an
-- | emission stays synchronous and `looped`'s re-entrancy guard still
-- | terminates loop cycles. (Wire-level debouncing inside a loop turns
-- | refeeds into a standing async ping-pong — the delay must sit in front
-- | of the wire, not on it.)
inputDebounced :: { ms :: Number } -> String -> PUI Web String String
inputDebounced { ms: millis } type_ = "type" := type_ $ wrap do
  element "input" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new $ Nothing
  pure
    { toUser: \newa -> do
      focused <- isFocused node
      unless focused $ setValue node newa
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      onInputDebounced node millis \value -> prop value
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
      for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
      Ref.write (Just prop) mPropRef
      void $ addEventListener "input" node $ const do
        value <- getValue node
        prop value
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
        for_ mProp \prop -> prop ma
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "input" node $ const do
          checked <- getChecked node
          a <- Ref.read aRef
          prop (if checked then (Just a) else Nothing)
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
        for_ mProp \prop -> for_ ma \newa -> prop newa
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "change" node $ const do
          a <- Ref.read aRef
          prop a
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
          prop a
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
    , fromUser: \prop -> prop {}
    }

-- | See `staticText` — same announcing chrome typing.
staticHTML :: String -> PUI Web {} {}
staticHTML html = wrap do
  parent <- gets _.parent
  newNode <- liftEffect $ appendRawHtml html parent
  modify_ _ { sibling = newNode}
  pure
    { toUser: mempty
    , fromUser: \prop -> prop {}
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
    , fromUser: \prop -> prop {}
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

init :: forall a. (Node -> Effect a) -> (a -> Effect Unit) -> (a -> Effect Unit) -> Ocular (PUI Web)
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
        prop change
        post ctx
    }

div :: Ocular (PUI Web)
div = el "div"

span :: Ocular (PUI Web)
span = el "span"

aside :: Ocular (PUI Web)
aside = el "aside"

article :: Ocular (PUI Web)
article = el "article"

header :: Ocular (PUI Web)
header = el "header"

footer :: Ocular (PUI Web)
footer = el "footer"

section :: Ocular (PUI Web)
section = el "section"

label :: Ocular (PUI Web)
label = el "label"

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

attrDyn :: String -> (Maybe {} -> Maybe String) -> Ocular (PUI Web)
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
      updateAttribute node mnewa = case valueFunction (mnewa $> {}) of
        Just value -> setAttribute node name value
        Nothing -> removeAttribute node name

infixr 10 attrDyn as :=>

clDyn :: String -> (Maybe {} -> Boolean) -> Ocular (PUI Web)
clDyn name pred w = wrap do
  w' <- unwrap w
  node <- gets _.sibling
  liftEffect $ (if pred Nothing then addClass else removeClass) node name
  pure
    { toUser: \mch -> do
    (if pred (Just {}) then addClass else removeClass) node name
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
-- | predicate. The named `Maybe`-valued business projection is the
-- | mechanism's own argument — `pane # provided currentQuestion` reads
-- | "shown, provided there is a current question": when it yields `Just`
-- | the content is attached and fed the payload, on `Nothing` it is
-- | detached — so the pane consumes the payload, never the whole model,
-- | and the visibility logic lives in testable business code (an input
-- | that already is the `Maybe` reads `provided identity`). Detachment
-- | means no echoes while absent: a pipeline-stage combinator, not a
-- | gated-merge operand.
-- |
-- | The payload row is the pane's row exactly — no subsumption here: the pane
-- | is *fed* the payload, so a `Maybe`-valued business projection returning
-- | precisely what the pane reads keeps both sides pinned (subsumption at the
-- | *stage* boundary is `displayed`/`updated`'s job).
-- | Show the wrapped pane **at case `l`** of the variant the projection
-- | yields — `atField`'s variant-side twin and `provided`'s label-indexed
-- | form. A variant is one-at-a-time, so adopting case `l` at an entity
-- | position *is* conditional existence: attached and fed the case's
-- | payload while the variant sits at `l`, detached otherwise. The
-- | projection is the mechanism's own argument (L16): a stored variant
-- | field reads `pane # atCase @"serving" _.display`; **mutually
-- | exclusive derived states classify once** into a variant-returning
-- | business function and each pane adopts its case —
-- | `# atCase @"taken" usernameStatus` — so exclusivity holds by
-- | construction where sibling `Maybe` projections could accidentally
-- | overlap. Panes whose payload combines a case with sibling fields (the
-- | record-shaped editor state) keep `provided <maybeOf>`.
atCase :: forall @l i a b s o. IsSymbol l => Cons l a b s => (i -> [ | s ]) -> PUI Web a o -> PUI Web i o
atCase f = provided (\i -> prj (Proxy @l) (f i))

provided :: forall i a b. (i -> Maybe a) -> PUI Web a b -> PUI Web i b
provided f w = wrap do
  {result: { toUser, fromUser}, ensureAttached, ensureDetached} <- attachable $ unwrap w
  pure
    { toUser: \i -> case f i of
      Nothing -> ensureDetached
      Just y -> do
        -- attach before feeding: a widget that measures itself on toUser (the
        -- MDC slider positions its thumb from the track width) needs to be in
        -- the document first, or it lays out against a zero-width detached node
        ensureAttached
        toUser y
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
        w'.fromUser \_ -> pure unit
        void $ addEventListener "click" node $ const do
          mi <- Ref.read iRef
          for_ mi \i -> prop i
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
        w'.fromUser \_ -> pure unit
        onClickXY node \x y -> prop { x, y }
    }

-- | The **structure-from-value builder collection**: build a whole widget per
-- | array element from the builder closure (tags/attributes as computed
-- | strings — `el ("h" <> show level)`, `circle >>> "cx" := show c.x`). The
-- | enclosing element is rebuilt wholesale on every value fed. Reach for it
-- | (and its single-value case `dynamic`) only when an element's *structure*
-- | genuinely varies with the data; when only *values* change over a fixed
-- | structure, feed the structure through the keyed retaining `foreach` and
-- | compute per-element attributes with `attrWith` (built once, updated in
-- | place, no rebuild). Owns its container like `foreach`.
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

-- | The single-value case of `foreachWith`: rebuild one widget from the fed
-- | value (a `foreachWith` over the one-element array). Owns its container:
-- | `svg [...] $ dynamic renderScene`, `div $ dynamic renderSwatch`.
dynamic :: forall a o. (a -> PUI Web {} o) -> PUI Web a o
dynamic build = wrap $ unwrap (foreachWith build) <#> \w ->
  { toUser: \a -> w.toUser [ a ], fromUser: w.fromUser }

-- | Build a **fixed** (closure-known) list into the container now — a
-- | `foreachWith` fed a constant array, input pinned to `{}` so it drops into a
-- | `{} → {}` chrome merge without an annotation: `ul $ each rows renderRow`,
-- | `tr $ each cells cellWidget`.
each :: forall a o. Array a -> (a -> PUI Web {} o) -> PUI Web {} o
each items build = wrap $ unwrap (foreachWith build) <#> \w ->
  { toUser: \_ -> w.toUser items, fromUser: w.fromUser }

-- Entry point

-- | The app entry: builds the widget in `<body>` and registers its
-- | wiring — and feeds **nothing**, which is why it demands a **closed**
-- | app: input `{}`, the one record whose initial value is derivable from
-- | its type. A pipeline's residual input row is its initial-state
-- | obligation — the entity fields not yet known at t=0 — so mounting
-- | requires it discharged (`mvu seed`, `with initial`): a missing seed is
-- | a type error here naming exactly the unsupplied fields, not a blank
-- | screen. The standalone app reads `body $ with initial $ ...` or
-- | `body $ ... $ pipeline # mvu seed`; emissions are simply dropped.
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  node <- documentBody
  runDomInNode node do
    { fromUser } <- unwrap ui
    liftEffect $ fromUser \_ -> pure unit

runWidgetInSelectedNode :: forall a b. String -> a -> (b -> Effect Unit) -> PUI Web a b -> Effect Unit
runWidgetInSelectedNode selector initial callback ui = do
  node <- selectedNode selector
  runWidgetInNode node initial callback ui

runWidgetInNode :: forall a b. Node -> a -> (b -> Effect Unit) -> PUI Web a b -> Effect Unit
runWidgetInNode node initial callback ui = runDomInNode node do
  { toUser, fromUser } <- unwrap ui
  liftEffect $ fromUser callback
  void $ liftEffect $ toUser initial

--- private

el :: String -> Ocular (PUI Web)
el tagName = wrap <<< element tagName <<< unwrap

