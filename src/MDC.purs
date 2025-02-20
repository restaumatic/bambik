-- Material Design Components implemented as UI/UIOcular datatypes, dogfooding intentional.
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
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Profunctor (rmap)
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import UI (UI, UIOcular, effAdapter)
import Web (Node, Web, aside, attr, checkboxInput, cl, clDyn, div, h1, h2, h3, h4, h5, h6, html, i, init, input, label, p, span, staticText, textArea, uniqueId)
import Web (button, radioButton) as Web

-- UIs

containedButton :: forall a. { label :: Maybe String, icon :: Maybe String } -> UI Web a a
containedButton { label, icon } =
  Web.button >>> cl "mdc-button" >>> cl "mdc-button--raised" >>> cl "initAside-button" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ A.do
    div >>> cl "mdc-button__ripple" $ pzero
    case icon of
      Just icon' -> i >>> cl "material-icons" >>> cl "mdc-button__icon" >>> attr "aria-hidden" "true" $ staticText icon'
      Nothing -> pzero
    case label of
      Just label' -> span >>> cl "mdc-button__label" $ staticText label'
      Nothing -> pzero

-- TODO support input types: email, text, password, number, search, tel, url
filledTextField :: { floatingLabel :: String } -> UI Web String String
filledTextField { floatingLabel } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--label-floating" >>> init (\node -> do
      comp <- newComponent material.textField."MDCTextField" node
      useNativeValidation comp false
      pure comp) mempty (\node validationStatus -> do
        setValid node (isNothing validationStatus)
        setContent node (fromMaybe "" validationStatus)) $ S.do
    span >>> cl "mdc-text-field__ripple" $ pzero
    S.do
      span >>> cl "mdc-floating-label" >>> attr "id" id >>> clDyn "mdc-floating-label--float-above" isJust $ staticText floatingLabel
      input "text" # cl "mdc-text-field__input" # attr "aria-labelledby" id # attr "aria-controls" helperId # attr "aria-describedby" helperId
      div >>> cl "mdc-text-field-helper-line" $
        div >>> cl "mdc-text-field-helper-text" >>> attr "id" helperId >>> attr "aria-hidden" "true" >>> init mdcTextFieldHelperText mempty mempty $ pzero
    span >>> cl "mdc-line-ripple" $ pzero
    where
      id = unsafePerformEffect uniqueId
      helperId = unsafePerformEffect uniqueId

filledTextArea :: { columns :: Int, rows :: Int } -> UI Web String String
filledTextArea { columns, rows } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--textarea" >>> cl "mdc-text-field--no-label" $ S.do
    span >>> cl "mdc-text-field__ripple" $ pzero
    span >>> cl "mdc-text-field__resizer" $
      textArea # cl "mdc-text-field__input" >>> attr "rows" (show rows) >>> attr "columns" (show columns) >>> attr "aria-label" "Label"
    span >>> cl "mdc-line-ripple" $ pzero

checkbox :: forall a. a -> UI Web (Maybe a) Void -> UI Web (Maybe a) (Maybe a)
checkbox default label =
  div >>> cl "mdc-form-field" >>> init (newComponent material.formField."MDCFormField") mempty mempty $ S.do
    div >>> cl "mdc-checkbox" >>> init (newComponent material.checkbox."MDCCheckbox") mempty mempty $ S.do
      checkboxInput default # cl "mdc-checkbox__native-control" # attr "id" id
      div >>> cl "mdc-checkbox__background" $ S.do
        html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div >>> cl "mdc-checkbox__mixedmark" $ pzero
      div >>> cl "mdc-checkbox__ripple" $ pzero
    attr "for" id $ rmap absurd label
    where
      id = unsafePerformEffect uniqueId

-- TODO add html grouping?
radioButton :: forall a. a -> UI Web (Maybe a) Void -> UI Web (Maybe a) a
radioButton default labelContent =
  div >>> cl "mdc-form-field" >>> init (newComponent material.formField."MDCFormField") mempty mempty $ A.do
    div >>> cl "mdc-radio" >>> init (newComponent material.radio."MDCRadio") mempty mempty $ A.do
      Web.radioButton default # cl "mdc-radio__native-control" # attr "id" uid
      div >>> cl "mdc-radio__background" $ A.do
        div >>> cl "mdc-radio__outer-circle" $ pzero
        div >>> cl "mdc-radio__inner-circle" $ pzero
      div >>> cl "mdc-radio__ripple" $ pzero
    attr "for" uid $ rmap absurd labelContent
  where
    uid = unsafePerformEffect uniqueId

indeterminateLinearProgress :: forall a. UI Web Boolean a
indeterminateLinearProgress =
  div >>> attr "role" "indeterminateLinearProgress" >>> cl "mdc-linear-progress" >>> attr "aria-label" "Progress Bar" >>> attr "aria-valuemin" "0" >>> attr "aria-valuemax" "1" >>> attr "aria-valuenow" "0" >>> effAdapter adapter $ A.do
    div >>> cl "mdc-linear-progress__buffer" $ A.do
      div >>> cl "mdc-linear-progress__buffer-bar" $ pzero
      div >>> cl "mdc-linear-progress__buffer-dots" $ pzero
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ pzero
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ pzero
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

headline1 :: UIOcular Web
headline1 w = h1 w # cl "mdc-typography--headline1"

headline2 :: UIOcular Web
headline2 w = h2 w # cl "mdc-typography--headline2"

headline3 :: UIOcular Web
headline3 w = h3 w # cl "mdc-typography--headline3"

headline4 :: UIOcular Web
headline4 w = h4 w # cl "mdc-typography--headline4"

headline5 :: UIOcular Web
headline5 w = h5 w # cl "mdc-typography--headline5"

headline6 :: UIOcular Web
headline6 w = h6 w # cl "mdc-typography--headline6"

subtitle1 :: UIOcular Web
subtitle1 w = p w # cl "mdc-typography--subtitle1"

subtitle2 :: UIOcular Web
subtitle2 w = p w # cl "mdc-typography--subtitle2"

button :: UIOcular Web
button w = span w # cl "mdc-typography--button"

caption :: UIOcular Web
caption w = span w # cl "mdc-typography--caption"

overline :: UIOcular Web
overline w = span w # cl "mdc-typography--overline"

body1 :: UIOcular Web
body1 w = p w # cl"mdc-typography--body1"

body2 :: UIOcular Web
body2 w = p w # cl"mdc-typography--body2"

elevation1 :: UIOcular Web
elevation1 w = div w # cl "mdc-elevation--z1"

elevation10 :: UIOcular Web
elevation10 w = div w # cl "mdc-elevation--z10" # attr "style" "padding: 25px"

elevation20 :: UIOcular Web
elevation20 w = div w # cl "mdc-elevation--z20" # attr "style" "padding: 25px"

card :: UIOcular Web
card w = div w # cl "mdc-card" # attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;"

dialog :: { title :: String } -> UIOcular Web
dialog { title } content =
  aside >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") mempty mempty $ A.do
    div >>> cl "mdc-dialog__container" $ A.do
      div >>> cl "mdc-dialog__surface" >>> attr "role" "alertdialog" >>> attr "aria-modal" "true" >>> attr "aria-labelledby" "my-dialog-title" >>> attr "aria-describedby" "my-dialog-content" $ A.do
        h2 >>> cl "mdc-dialog__title" >>> attr "id" "my-dialog-title" $ staticText title
        div >>> cl "mdc-dialog__content" >>> attr "id" "my-dialog-content" $ content
    div >>> cl "mdc-dialog__scrim" $ pzero

simpleDialog :: { title :: String, confirm :: String } -> UIOcular Web
simpleDialog { title, confirm } content =
  div >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") open (\a propStatus -> close a) $ A.do
    div >>> cl "mdc-dialog__container" $
      div >>> cl "mdc-dialog__surface" >>> attr "role" "altertdialog" >>> attr "aria-modal" "true" >>> attr "aria-labelledby" "my-dialog-title" >>> attr "aria-describedby" "my-dialog-content" $ S.do
        T.do
          A.do
            h2 >>> cl "mdc-dialog__title" >>> attr "id" id $ staticText title
            div >>> cl "mdc-dialog__content" >>> attr "id" id' $
              content
          div >>> cl "mdc-dialog__actions" $ S.do
            Web.button >>> attr "type" "button" >>> cl "mdc-button" >>> cl "mdc-dialog__button" $ A.do
              div >>> cl "mdc-button__ripple" $ pzero
              span >>>  cl "mdc-button__label" $ staticText confirm
    div >>> cl "mdc-dialog__scrim" $ pzero
    where
      id = unsafePerformEffect uniqueId
      id' = unsafePerformEffect uniqueId

snackbar :: UIOcular Web
snackbar content =
  aside >>> cl "mdc-snackbar" >>> init (newComponent material.snackbar."MDCSnackbar") open (\a propStatus -> close a) $
    div >>> cl "mdc-snackbar__surface" >>> attr "role" "status" >>> attr "aria-relevant" "additions" $
      div >>> cl "mdc-snackbar__label" >>> attr "aria-atomic" "false" $
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
