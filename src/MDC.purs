-- MDC implemented with Widgets, dogfooding intentional.
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
  , filledTextField
  , headline1
  , headline2
  , headline3
  , headline4
  , headline5
  , headline6
  , overline
  , progressBar
  , radioButton
  , snackbar
  , subtitle1
  , subtitle2
  )
  where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Control.Plus (empty)
import Data.Maybe (Maybe, isNothing, maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import Web (Node, Web, aside, attr, checkboxInput, cl, clickable, div, dynClass, h1, h2, h3, h4, h5, h6, html, label, p, span, text, textInput, uniqueId)
import Web (button, radioButton) as Web
import Widget (Changed(..), Widget, WidgetOptics', effAdapter, effBracket)

-- Primitive widgets

containedButton :: forall a b. { label :: Widget Web a b } -> Widget Web a a
containedButton { label } =
  Web.button (A.do
    div empty # cl "mdc-button__ripple"
    span (label >>> empty) # cl "mdc-button__label") # cl "mdc-button" # cl "mdc-button--raised" # cl "initAside-button" # bracket (gets _.sibling >>= (liftEffect <<< newComponent material.ripple."MDCRipple")) (const $ pure unit) (const $ pure unit) # clickable

filledTextField :: forall a b. { floatingLabel :: Widget Web String Void -> Widget Web a b } -> WidgetOptics' String a -> Widget Web a a
filledTextField { floatingLabel } value =
  label (S.do
    span (empty :: Widget Web a a) # cl "mdc-text-field__ripple"
    (S.do
      (span text # cl "mdc-floating-label" # attr "id" id # dynClass "mdc-floating-label--float-above" (maybe false (case _ of
        Removed -> false
        Altered _ -> true)) # floatingLabel) >>> empty
      textInput # value # cl "mdc-text-field__input" # attr "aria-labelledby" id)
    span (empty :: Widget Web a a) # cl "mdc-line-ripple") # cl "mdc-text-field" # cl "mdc-text-field--filled" # cl "mdc-text-field--label-floating" # dynClass "mdc-text-field--disabled" (maybe true $ case _ of
    Altered _ -> false
    Removed -> true) # bracket (gets _.sibling >>= (liftEffect <<< newComponent material.textField."MDCTextField")) (const $ pure unit) (const $ pure unit)
    where
      id = unsafePerformEffect uniqueId

checkbox :: forall a s c. { labelContent :: Widget Web s c, default :: a } -> WidgetOptics' (Maybe a) s -> Widget Web s s
checkbox { labelContent, default } checked =
  div ( S.do
    div ( S.do
      checkboxInput default # checked # cl "mdc-checkbox__native-control" # attr "id" id
      div (S.do
        html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div empty # cl "mdc-checkbox__mixedmark") # cl "mdc-checkbox__background"
      div empty # cl "mdc-checkbox__ripple") # cl "mdc-checkbox" # bracket (gets _.sibling >>= (liftEffect <<< newComponent material.checkbox."MDCCheckbox")) (const $ pure unit) (const $ pure unit)
    label (labelContent >>> empty) # attr "for" id) # cl "mdc-form-field" # bracket (gets _.sibling >>= (liftEffect <<< newComponent material.formField."MDCFormField")) (const $ pure unit) (const $ pure unit)
    where
      id = unsafePerformEffect uniqueId

-- TODO add html grouping?
radioButton :: forall a s c. { labelContent :: Widget Web s c, default :: a } -> WidgetOptics' a s -> Widget Web s s
radioButton { labelContent, default } value =
  div (S.do
    div (S.do
        Web.radioButton default # value # cl "mdc-radio__native-control" # attr "id" uid
        div (S.do
          div empty # cl "mdc-radio__outer-circle"
          div empty # cl "mdc-radio__inner-circle") # cl "mdc-radio__background"
        div empty # cl "mdc-radio__ripple") # cl "mdc-radio" # dynClass "mdc-radio--disabled" isNothing # bracket (gets _.sibling >>= (liftEffect <<< newComponent material.radio."MDCRadio")) (const $ pure unit) (const $ pure unit)
    label (labelContent >>> empty) # attr "for" uid
  ) # cl "mdc-form-field" # bracket (gets _.sibling >>= (liftEffect <<< newComponent material.formField."MDCFormField")) (const $ pure unit) (const $ pure unit)
    where
      uid = unsafePerformEffect uniqueId

headline1 :: forall a b. Widget Web a b -> Widget Web a b
headline1 w = h1 w # cl "mdc-typography--headline1"

headline2 :: forall a b. Widget Web a b -> Widget Web a b
headline2 w = h2 w # cl "mdc-typography--headline2"

headline3 :: forall a b. Widget Web a b -> Widget Web a b
headline3 w = h3 w # cl "mdc-typography--headline3"

headline4 :: forall a b. Widget Web a b -> Widget Web a b
headline4 w = h4 w # cl "mdc-typography--headline4"

headline5 :: forall a b. Widget Web a b -> Widget Web a b
headline5 w = h5 w # cl "mdc-typography--headline5"

headline6 :: forall a b. Widget Web a b -> Widget Web a b
headline6 w = h6 w # cl "mdc-typography--headline6"

subtitle1 :: forall a b. Widget Web a b -> Widget Web a b
subtitle1 w = p w # cl "mdc-typography--subtitle1"

subtitle2 :: forall a b. Widget Web a b -> Widget Web a b
subtitle2 w = p w # cl "mdc-typography--subtitle2"

button :: forall a b. Widget Web a b -> Widget Web a b
button w = span w # cl "mdc-typography--button"

caption :: forall a b. Widget Web a b -> Widget Web a b
caption w = span w # cl "mdc-typography--caption"

overline :: forall a b. Widget Web a b -> Widget Web a b
overline w = span w # cl "mdc-typography--overline"

body1 :: forall a b. Widget Web a b -> Widget Web a b
body1 w = p w # cl"mdc-typography--body1"

body2 :: forall a b. Widget Web a b -> Widget Web a b
body2 w = p w # cl"mdc-typography--body2"

elevation1 :: forall a b. Widget Web a b -> Widget Web a b
elevation1 w = div w # cl "mdc-elevation--z1"

elevation10 :: forall a b. Widget Web a b -> Widget Web a b
elevation10 w = div w # cl "mdc-elevation--z10" # attr "style" "padding: 25px"

elevation20 :: forall a b. Widget Web a b -> Widget Web a b
elevation20 w = div w # cl "mdc-elevation--z20" # attr "style" "padding: 25px"

card :: forall a b. Widget Web a b -> Widget Web a b
card w = div w # cl "mdc-card" # attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;"

dialog :: forall a b. { title :: Widget Web a b } -> Widget Web a a -> Widget Web a a
dialog { title } content =
  aside (S.do
    div (S.do
      div (S.do
        h2 (title >>> empty) # cl "mdc-dialog__title" # attr "id" "my-dialog-title"
        div content # cl "mdc-dialog__content" # attr "id" "my-dialog-content"
      ) # cl "mdc-dialog__surface" # attr "role" "alertdialog" # attr "aria-modal" "true" # attr "aria-labelledby" "my-dialog-title" # attr "aria-describedby" "my-dialog-content"
    ) # cl "mdc-dialog__container"
    div empty # cl "mdc-dialog__scrim"
  ) # cl "mdc-dialog" # bracket initializeMdcDialog openMdcComponent closeMdcComponent
    where
      initializeMdcDialog = gets _.sibling >>= (liftEffect <<< newComponent material.dialog."MDCDialog")
      openMdcComponent comp = liftEffect $ open comp
      closeMdcComponent comp = liftEffect $ close comp

snackbar :: forall a b. { label :: Widget Web a b } -> Widget Web a b
snackbar { label } =
  aside
    ( div
      ( div
        label # cl "mdc-snackbar__label" # attr "aria-atomic" "false") # attr "role" "status" # attr "aria-relevant" "additions" # cl "mdc-snackbar__surface") # cl "mdc-snackbar" # bracket initializeMdcSnackbar openMdcComponent (const $ pure unit)
    where
      initializeMdcSnackbar = gets _.sibling >>= (liftEffect <<< newComponent material.snackbar."MDCSnackbar")
      openMdcComponent comp = liftEffect $ open comp

progressBar :: Widget Web Boolean Unit -- TODO should be Widget Web Boolean Void
progressBar =
  div ( S.do
    div ( S.do
      div empty # cl "mdc-linear-progress__buffer-bar"
      div empty # cl "mdc-linear-progress__buffer-dots" ) # cl "mdc-linear-progress__buffer"
    div ( S.do
      span empty # cl "mdc-linear-progress__bar-inner" ) # cl "mdc-linear-progress__bar" # cl "mdc-linear-progress__primary-bar"
    div (S.do
      span empty # cl "mdc-linear-progress__bar-inner") # cl "mdc-linear-progress__bar" # cl "mdc-linear-progress__secondary-bar" ) # attr "role" "progressbar" # cl "mdc-linear-progress" # attr "aria-label" "TODO: Example Progress Bar" # attr "aria-valuemin" "0" # attr "aria-valuemax" "1" # attr "aria-valuenow" "0" # effAdapter do
        comp <- gets _.sibling >>= (liftEffect <<< newComponent material.linearProgress."MDCLinearProgress")
        liftEffect $ close comp
        liftEffect $ setDeterminate comp false
        pure
          { pre: case _ of
            true -> open comp
            false -> close comp
          , post: \unit -> pure unit }

-- Private

bracket :: forall a b c m. Monad m => m c -> (c -> Effect Unit) -> (c -> Effect Unit) -> Widget m a b -> Widget m a b
bracket afterInit afterInward beforeOutward = effBracket do
  ctx <- afterInit
  pure
    { beforeInput: mempty
    , afterInput: case _ of
      Removed -> pure unit -- TODO really?
      Altered _ -> afterInward ctx
    , beforeOutput: const $ beforeOutward ctx
    , afterOutput: mempty
    }

foreign import data Component :: Type
foreign import data ComponentClass :: Type
foreign import open :: Component -> Effect Unit
foreign import close :: Component -> Effect Unit
foreign import newComponent :: ComponentClass -> Node -> Effect Component
foreign import setDeterminate :: Component -> Boolean -> Effect Unit
foreign import material
  :: { textField :: { "MDCTextField" :: ComponentClass }
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

