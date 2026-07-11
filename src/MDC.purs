-- Material Design Components implemented as UI Web/UIOcular (UI Web) datatypes, dogfooding intentional.
-- Every component is **based on row profunctors**: compounds are
-- label-indexed (`filledTextField @l` is a singleton-record editor,
-- `Cons l v () s => … -> UI Web { | s } { | s }`), so they slot into the
-- app-level record merges directly. Internally the live leaf is
-- `property @l`-lifted and its chrome is hand-fused in the `Web` monad
-- (decoration as implementation technique — and a necessity: abstract
-- labels cannot flow through the merges' `Nub`, so a skolem-labeled
-- operand can't be merged); all-chrome groups (button content, progress
-- bars) have concrete rows and stay literal `RecordToRecord.do` merges of
-- announcing chrome (`staticText`/`staticHTML`/`pempty` at `{} → {}`).
-- Code order = DOM order throughout.
module MDC
  ( body1
  , body2
  , button
  , caption
  , card
  , checkbox
  , containedButton
  , dialog
  , elevation1
  , elevation10
  , elevation20
  , filledTextArea
  , filledTextField
  , headline1
  , headline2
  , headline3
  , headline4
  , headline5
  , headline6
  , indeterminateLinearProgress
  , overline
  , radioButton
  , simpleDialog
  , snackbar
  , subtitle1
  , subtitle2
  )
  where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.Default (class Default)
import Data.Foldable (for_)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (pempty, property)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (UI, effAdapter, silence)
import Web (Node, Web, aside, checkboxInput, cl, clDyn, div, h1, h2, h3, h4, h5, h6, i, init, input, label, p, span, staticHTML, staticText, textArea, uniqueId, (:=))
import Web (button, radioButton) as Web

-- UIs

containedButton :: forall a. { label :: Maybe String, icon :: Maybe String } -> UI Web a a
containedButton { label, icon } =
  Web.button >>> cl "mdc-button" >>> cl "mdc-button--raised" >>> cl "initAside-button" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    div >>> cl "mdc-button__ripple" $ pempty
    case icon of
      Just icon' -> i >>> cl "material-icons" >>> cl "mdc-button__icon" >>> "aria-hidden" := "true" $ staticText icon'
      Nothing -> pempty
    case label of
      Just label' -> span >>> cl "mdc-button__label" $ staticText label'
      Nothing -> pempty

-- TODO support input types: email, text, password, number, search, tel, url
filledTextField :: forall @l s. IsSymbol l => Cons l String () s => { floatingLabel :: String } -> UI Web { | s } { | s }
filledTextField { floatingLabel } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--label-floating" >>> init (\node -> do
      comp <- newComponent material.textField."MDCTextField" node
      useNativeValidation comp false
      pure comp) mempty (\node validationStatus -> do
        setValid node (isNothing validationStatus)
        setContent node (fromMaybe "" validationStatus)) $ wrap do
    _ <- unwrap (span >>> cl "mdc-text-field__ripple" $ pempty)
    floating <- unwrap (span >>> cl "mdc-floating-label" >>> "id" := id >>> clDyn "mdc-floating-label--float-above" isJust $ staticText floatingLabel)
    w <- unwrap (property @l $ input "text" # cl "mdc-text-field__input" # "aria-labelledby" := id # "aria-controls" := helperId # "aria-describedby" := helperId)
    _ <- unwrap (div >>> cl "mdc-text-field-helper-line" $
      div >>> cl "mdc-text-field-helper-text" >>> "id" := helperId >>> "aria-hidden" := "true" >>> init mdcTextFieldHelperText mempty mempty $ pempty)
    _ <- unwrap (span >>> cl "mdc-line-ripple" $ pempty)
    pure
      { toUser: \u -> do
          floating.toUser (u $> {})
          w.toUser u
      , fromUser: w.fromUser
      }
  where
    id = unsafePerformEffect uniqueId
    helperId = unsafePerformEffect uniqueId

filledTextArea :: forall @l s. IsSymbol l => Cons l String () s => { columns :: Int, rows :: Int } -> UI Web { | s } { | s }
filledTextArea { columns, rows } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--textarea" >>> cl "mdc-text-field--no-label" $ wrap do
    _ <- unwrap (span >>> cl "mdc-text-field__ripple" $ pempty)
    w <- unwrap (property @l $ span >>> cl "mdc-text-field__resizer" $ textArea # cl "mdc-text-field__input" >>> "rows" := show rows >>> "columns" := show columns >>> "aria-label" := "Label")
    _ <- unwrap (span >>> cl "mdc-line-ripple" $ pempty)
    pure w

-- | Label content is chrome (`{} → {}`, announcing).
checkbox :: forall @l a s. IsSymbol l => Cons l (Maybe a) () s => Default a => UI Web {} {} -> UI Web { | s } { | s }
checkbox label =
  div >>> cl "mdc-form-field" >>> init (newComponent material.formField."MDCFormField") mempty mempty $ wrap do
    w <- unwrap $ div >>> cl "mdc-checkbox" >>> init (newComponent material.checkbox."MDCCheckbox") mempty mempty $ wrap do
      w' <- unwrap (property @l $ checkboxInput # cl "mdc-checkbox__native-control" # "id" := id)
      _ <- unwrap (div >>> cl "mdc-checkbox__background" $ RecordToRecord.do
        staticHTML """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div >>> cl "mdc-checkbox__mixedmark" $ pempty)
      _ <- unwrap (div >>> cl "mdc-checkbox__ripple" $ pempty)
      pure w'
    lbl <- unwrap ("for" := id $ label)
    pure
      { toUser: \u -> do
          lbl.toUser (u $> {})
          w.toUser u
      , fromUser: w.fromUser
      }
    where
      id = unsafePerformEffect uniqueId

-- TODO add staticHTML grouping?
-- | Label content is chrome (`{} → {}`, announcing). Type-changing: the
-- | input field holds the selection state (`Maybe a`), the output field the
-- | bare selection (`a`).
radioButton :: forall @l a si so. IsSymbol l => Cons l (Maybe a) () si => Cons l a () so => Default a => UI Web {} {} -> UI Web { | si } { | so }
radioButton labelContent =
  div >>> cl "mdc-form-field" >>> init (newComponent material.formField."MDCFormField") mempty mempty $ wrap do
    w <- unwrap $ div >>> cl "mdc-radio" >>> init (newComponent material.radio."MDCRadio") mempty mempty $ wrap do
      w' <- unwrap (property @l $ Web.radioButton # cl "mdc-radio__native-control" # "id" := uid)
      _ <- unwrap (div >>> cl "mdc-radio__background" $ RecordToRecord.do
        div >>> cl "mdc-radio__outer-circle" $ pempty
        div >>> cl "mdc-radio__inner-circle" $ pempty)
      _ <- unwrap (div >>> cl "mdc-radio__ripple" $ pempty)
      pure w'
    lbl <- unwrap ("for" := uid $ labelContent)
    pure
      { toUser: \u -> do
          lbl.toUser (u $> {})
          w.toUser u
      , fromUser: w.fromUser
      }
  where
    uid = unsafePerformEffect uniqueId

indeterminateLinearProgress :: forall a. UI Web Boolean a
indeterminateLinearProgress =
  div >>> "role" := "indeterminateLinearProgress" >>> cl "mdc-linear-progress" >>> "aria-label" := "Progress Bar" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" >>> "aria-valuenow" := "0" >>> effAdapter adapter $ lcmap (const {}) $ Semigroupoid.do
    RecordToRecord.do
      div >>> cl "mdc-linear-progress__buffer" $ RecordToRecord.do
        div >>> cl "mdc-linear-progress__buffer-bar" $ pempty
        div >>> cl "mdc-linear-progress__buffer-dots" $ pempty
      div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
        span >>> cl "mdc-linear-progress__bar-inner" $ pempty
      div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
        span >>> cl "mdc-linear-progress__bar-inner" $ pempty
    silence
    where
      adapter = do
        comp <- gets _.sibling >>= (liftEffect <<< newComponent material.linearProgress."MDCLinearProgress")
        liftEffect $ close comp
        liftEffect $ setDeterminate comp false
        pure
          { pre: case _ of
            true -> open comp
            false -> close comp
          , post: pure }

-- UIOculars

headline1 :: Ocular (UI Web)
headline1 w = h1 w # cl "mdc-typography--headline1"

headline2 :: Ocular (UI Web)
headline2 w = h2 w # cl "mdc-typography--headline2"

headline3 :: Ocular (UI Web)
headline3 w = h3 w # cl "mdc-typography--headline3"

headline4 :: Ocular (UI Web)
headline4 w = h4 w # cl "mdc-typography--headline4"

headline5 :: Ocular (UI Web)
headline5 w = h5 w # cl "mdc-typography--headline5"

headline6 :: Ocular (UI Web)
headline6 w = h6 w # cl "mdc-typography--headline6"

subtitle1 :: Ocular (UI Web)
subtitle1 w = p w # cl "mdc-typography--subtitle1"

subtitle2 :: Ocular (UI Web)
subtitle2 w = p w # cl "mdc-typography--subtitle2"

button :: Ocular (UI Web)
button w = span w # cl "mdc-typography--button"

caption :: Ocular (UI Web)
caption w = span w # cl "mdc-typography--caption"

overline :: Ocular (UI Web)
overline w = span w # cl "mdc-typography--overline"

body1 :: Ocular (UI Web)
body1 w = p w # cl"mdc-typography--body1"

body2 :: Ocular (UI Web)
body2 w = p w # cl"mdc-typography--body2"

elevation1 :: Ocular (UI Web)
elevation1 w = div w # cl "mdc-elevation--z1"

elevation10 :: Ocular (UI Web)
elevation10 w = div w # cl "mdc-elevation--z10" # "style" := "padding: 25px"

elevation20 :: Ocular (UI Web)
elevation20 w = div w # cl "mdc-elevation--z20" # "style" := "padding: 25px"

-- | A card with an optional caption — the caption is design-system config
-- | (like `filledTextField`'s `floatingLabel`). The card is content-agnostic
-- | (any polarity), so its caption chrome is hand-fused, not merged.
card :: { caption :: Maybe String } -> Ocular (UI Web)
card { caption: mCaption } content =
  div >>> cl "mdc-card" >>> "style" := "padding: 10px; margin: 15px 0 15px 0; text-align: justify;" $ wrap do
    for_ mCaption \c -> void $ unwrap (caption $ staticText c)
    unwrap content

dialog :: { title :: String } -> Ocular (UI Web)
dialog { title } content =
  aside >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") mempty mempty $ wrap do
    result <- unwrap $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "alertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := "my-dialog-title" >>> "aria-describedby" := "my-dialog-content" $ wrap do
          _ <- unwrap (h2 >>> cl "mdc-dialog__title" >>> "id" := "my-dialog-title" $ staticText title)
          unwrap (div >>> cl "mdc-dialog__content" >>> "id" := "my-dialog-content" $ content)
    _ <- unwrap (div >>> cl "mdc-dialog__scrim" $ pempty)
    pure result

simpleDialog :: { title :: String, confirm :: String } -> Ocular (UI Web)
simpleDialog { title, confirm } content =
  div >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") open (\a propStatus -> close a) $ wrap do
    result <- unwrap $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "altertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := "my-dialog-title" >>> "aria-describedby" := "my-dialog-content" $ Semigroupoid.do
          wrap do
            _ <- unwrap (h2 >>> cl "mdc-dialog__title" >>> "id" := id $ staticText title)
            unwrap (div >>> cl "mdc-dialog__content" >>> "id" := id' $ content)
          div >>> cl "mdc-dialog__actions" $
            Web.button >>> "type" := "button" >>> cl "mdc-button" >>> cl "mdc-dialog__button" $ RecordToRecord.do
              div >>> cl "mdc-button__ripple" $ pempty
              span >>> cl "mdc-button__label" $ staticText confirm
    _ <- unwrap (div >>> cl "mdc-dialog__scrim" $ pempty)
    pure result
    where
      id = unsafePerformEffect uniqueId
      id' = unsafePerformEffect uniqueId

snackbar :: Ocular (UI Web)
snackbar content =
  aside >>> cl "mdc-snackbar" >>> init (newComponent material.snackbar."MDCSnackbar") open (\a propStatus -> close a) $
    div >>> cl "mdc-snackbar__surface" >>> "role" := "status" >>> "aria-relevant" := "additions" $
      div >>> cl "mdc-snackbar__label" >>> "aria-atomic" := "false" $
        content

-- Private

foreign import data Component :: Type
foreign import data ComponentClass :: Type
foreign import open :: Component -> Effect Unit
foreign import close :: Component -> Effect Unit
foreign import newComponent :: ComponentClass -> Node -> Effect Component
foreign import setDeterminate :: Component -> Boolean -> Effect Unit
foreign import material
  :: { textField :: { "MDCTextField" :: ComponentClass }
    --  , textFieldHelperText :: { "MDCTextFieldHelperText" :: ComponentClass }
     , ripple :: { "MDCRipple" :: ComponentClass }
     , drawer :: { "MDCDrawer" :: ComponentClass }
     , tabBar :: { "MDCTabBar" :: ComponentClass }
     , dialog :: { "MDCDialog" :: ComponentClass }
     , snackbar :: { "MDCSnackbar" :: ComponentClass }
     , radio :: { "MDCRadio" :: ComponentClass }
     , chips :: { "MDCChip" :: ComponentClass }
     , select :: { "MDCSelect" :: ComponentClass }
     , list :: { "MDCList" :: ComponentClass }
     , checkbox :: { "MDCCheckbox" :: ComponentClass }
     , formField :: { "MDCFormField" :: ComponentClass }
     , linearProgress :: { "MDCLinearProgress" :: ComponentClass }
     }

foreign import mdcTextFieldHelperText :: Node -> Effect Component
foreign import setValid :: Component -> Boolean -> Effect Unit
foreign import setContent :: Component -> String -> Effect Unit
foreign import useNativeValidation :: Component -> Boolean -> Effect Unit
