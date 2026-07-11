-- Material Design Components implemented as UI Web/UIOcular (UI Web) datatypes, dogfooding intentional.
-- Every compound component is a **row-profunctor merge**: its pieces are
-- `RecordToVariant.do` (`× → +`) operands — the one merge direction that is
-- ungated (variant outputs need no announcement), so silent chrome composes
-- freely — with code order mapping 1-1 to DOM order. Chrome terminates in
-- the merge's own unit `pempty` (self-pinning; input presence still flows,
-- so `clDyn` dynamics keep working), the one live leaf is a `live` operand
-- (input field `value` drives it, its output emits as case `value`),
-- input-consuming displays are `watching` operands, and the composite
-- collapses back to its scalar interface at the component boundary
-- (`scalar`, or `backdrop` when all operands are chrome).
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
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Profunctor.Row (backdrop, live, scalar, watching)
import Data.Profunctor.Row.RecordToVariant (pempty)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import QualifiedDo.Semigroupoid as Flow
import UI (UI, effAdapter)
import Web (Node, Web, aside, checkboxInput, cl, clDyn, div, h1, h2, h3, h4, h5, h6, i, init, input, label, p, span, staticHTML, staticText, textArea, uniqueId, (:=))
import Web (button, radioButton) as Web

-- UIs

containedButton :: forall a. { label :: Maybe String, icon :: Maybe String } -> UI Web a a
containedButton { label, icon } =
  Web.button >>> cl "mdc-button" >>> cl "mdc-button--raised" >>> cl "initAside-button" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ backdrop $ RecordToVariant.do
    div >>> cl "mdc-button__ripple" $ pempty
    case icon of
      Just icon' -> i >>> cl "material-icons" >>> cl "mdc-button__icon" >>> "aria-hidden" := "true" $ staticText icon'
      Nothing -> pempty
    case label of
      Just label' -> span >>> cl "mdc-button__label" $ staticText label'
      Nothing -> pempty

-- TODO support input types: email, text, password, number, search, tel, url
filledTextField :: { floatingLabel :: String } -> UI Web String String
filledTextField { floatingLabel } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--label-floating" >>> init (\node -> do
      comp <- newComponent material.textField."MDCTextField" node
      useNativeValidation comp false
      pure comp) mempty (\node validationStatus -> do
        setValid node (isNothing validationStatus)
        setContent node (fromMaybe "" validationStatus)) $ scalar $ RecordToVariant.do
    span >>> cl "mdc-text-field__ripple" $ pempty
    span >>> cl "mdc-floating-label" >>> "id" := id >>> clDyn "mdc-floating-label--float-above" isJust $ staticText floatingLabel
    live $ input "text" # cl "mdc-text-field__input" # "aria-labelledby" := id # "aria-controls" := helperId # "aria-describedby" := helperId
    div >>> cl "mdc-text-field-helper-line" $
      div >>> cl "mdc-text-field-helper-text" >>> "id" := helperId >>> "aria-hidden" := "true" >>> init mdcTextFieldHelperText mempty mempty $ pempty
    span >>> cl "mdc-line-ripple" $ pempty
  where
    id = unsafePerformEffect uniqueId
    helperId = unsafePerformEffect uniqueId

filledTextArea :: { columns :: Int, rows :: Int } -> UI Web String String
filledTextArea { columns, rows } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--textarea" >>> cl "mdc-text-field--no-label" $ scalar $ RecordToVariant.do
    span >>> cl "mdc-text-field__ripple" $ pempty
    live $ span >>> cl "mdc-text-field__resizer" $ textArea # cl "mdc-text-field__input" >>> "rows" := show rows >>> "columns" := show columns >>> "aria-label" := "Label"
    span >>> cl "mdc-line-ripple" $ pempty

checkbox :: forall a. Default a => UI Web (Maybe a) Void -> UI Web (Maybe a) (Maybe a)
checkbox label =
  div >>> cl "mdc-form-field" >>> init (newComponent material.formField."MDCFormField") mempty mempty $ scalar $ RecordToVariant.do
    live $ div >>> cl "mdc-checkbox" >>> init (newComponent material.checkbox."MDCCheckbox") mempty mempty $ scalar $ RecordToVariant.do
      live $ checkboxInput # cl "mdc-checkbox__native-control" # "id" := id
      div >>> cl "mdc-checkbox__background" $ RecordToVariant.do
        staticHTML """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div >>> cl "mdc-checkbox__mixedmark" $ pempty
      div >>> cl "mdc-checkbox__ripple" $ pempty
    watching $ "for" := id $ label
    where
      id = unsafePerformEffect uniqueId

-- TODO add staticHTML grouping?
radioButton :: forall a. Default a => UI Web (Maybe a) Void -> UI Web (Maybe a) a
radioButton labelContent =
  div >>> cl "mdc-form-field" >>> init (newComponent material.formField."MDCFormField") mempty mempty $ scalar $ RecordToVariant.do
    live $ div >>> cl "mdc-radio" >>> init (newComponent material.radio."MDCRadio") mempty mempty $ scalar $ RecordToVariant.do
      live $ Web.radioButton # cl "mdc-radio__native-control" # "id" := uid
      div >>> cl "mdc-radio__background" $ RecordToVariant.do
        div >>> cl "mdc-radio__outer-circle" $ pempty
        div >>> cl "mdc-radio__inner-circle" $ pempty
      div >>> cl "mdc-radio__ripple" $ pempty
    watching $ "for" := uid $ labelContent
  where
    uid = unsafePerformEffect uniqueId

indeterminateLinearProgress :: forall a. UI Web Boolean a
indeterminateLinearProgress =
  div >>> "role" := "indeterminateLinearProgress" >>> cl "mdc-linear-progress" >>> "aria-label" := "Progress Bar" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" >>> "aria-valuenow" := "0" >>> effAdapter adapter $ backdrop $ RecordToVariant.do
    div >>> cl "mdc-linear-progress__buffer" $ RecordToVariant.do
      div >>> cl "mdc-linear-progress__buffer-bar" $ pempty
      div >>> cl "mdc-linear-progress__buffer-dots" $ pempty
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ pempty
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ pempty
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
-- | (like `filledTextField`'s `floatingLabel`), not business composition;
-- | internally it is chrome in the card's own `× → +` merge.
card :: { caption :: Maybe String } -> Ocular (UI Web)
card { caption: mCaption } content =
  div >>> cl "mdc-card" >>> "style" := "padding: 10px; margin: 15px 0 15px 0; text-align: justify;" $ scalar $ RecordToVariant.do
    case mCaption of
      Just c -> caption $ staticText c
      Nothing -> pempty
    live content

dialog :: { title :: String } -> Ocular (UI Web)
dialog { title } content =
  aside >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") mempty mempty $ scalar $ RecordToVariant.do
    live $ div >>> cl "mdc-dialog__container" $
      div >>> cl "mdc-dialog__surface" >>> "role" := "alertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := "my-dialog-title" >>> "aria-describedby" := "my-dialog-content" $ scalar $ RecordToVariant.do
        h2 >>> cl "mdc-dialog__title" >>> "id" := "my-dialog-title" $ staticText title
        live $ div >>> cl "mdc-dialog__content" >>> "id" := "my-dialog-content" $ content
    div >>> cl "mdc-dialog__scrim" $ pempty

simpleDialog :: { title :: String, confirm :: String } -> Ocular (UI Web)
simpleDialog { title, confirm } content =
  div >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") open (\a propStatus -> close a) $ scalar $ RecordToVariant.do
    live $ div >>> cl "mdc-dialog__container" $
      div >>> cl "mdc-dialog__surface" >>> "role" := "altertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := "my-dialog-title" >>> "aria-describedby" := "my-dialog-content" $ Flow.do
        scalar $ RecordToVariant.do
          h2 >>> cl "mdc-dialog__title" >>> "id" := id $ staticText title
          live $ div >>> cl "mdc-dialog__content" >>> "id" := id' $ content
        div >>> cl "mdc-dialog__actions" $
          Web.button >>> "type" := "button" >>> cl "mdc-button" >>> cl "mdc-dialog__button" $ backdrop $ RecordToVariant.do
            div >>> cl "mdc-button__ripple" $ pempty
            span >>> cl "mdc-button__label" $ staticText confirm
    div >>> cl "mdc-dialog__scrim" $ pempty
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
