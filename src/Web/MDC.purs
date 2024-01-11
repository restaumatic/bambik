module Web.MDC
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
  , radioButton
  , snackbar
  , subtitle1
  , subtitle2
  )
  where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Control.Plus (empty)
import Data.Maybe (Maybe)
import Data.String (null)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Propagator (bracket)
import QualifiedDo.Alt as A
import QualifiedDo.Semigroup as S
import Web (Widget, aside, at', cl', clickable, dcl', div, h1, h2, h3, h4, h5, h6, html, textInput, label, p, span, text)
import Web (button, checkbox, radioButton) as Web
import Web.Internal.DOM (Node)
import Web.Internal.DOMBuilder (uniqueId)

-- Primitive widgets

containedButton :: forall a b. { label :: Widget a b } -> Widget a a
containedButton { label } =
  Web.button (A.do
    div empty # cl' "mdc-button__ripple"
    span (label >>> empty) # cl' "mdc-button__label") # cl' "mdc-button" # cl' "mdc-button--raised" # cl' "initAside-button" # bracket (gets _.sibling >>= newComponent material.ripple."MDCRipple") (const $ pure) (const $ pure) # clickable

filledTextField :: forall a b. { floatingLabel :: Widget String b -> Widget a b } -> (Widget String String -> Widget a a) -> Widget a a
filledTextField { floatingLabel } value =
  label (S.do
    span (empty :: Widget a a) # cl' "mdc-text-field__ripple"
    (S.do
      (span text # cl' "mdc-floating-label" # at' "id" id # dcl' "mdc-floating-label--float-above" (not <<< null) # floatingLabel) >>> empty
      textInput # value # cl' "mdc-text-field__input" # at' "aria-labelledby" id)
    span (empty :: Widget a a) # cl' "mdc-line-ripple") # cl' "mdc-text-field" # cl' "mdc-text-field--filled" # cl' "mdc-text-field--label-floating" # bracket (gets _.sibling >>= newComponent material.textField."MDCTextField") (const $ pure) (const $ pure)
    where
      id = unsafePerformEffect uniqueId

checkbox :: forall a b. { labelContent :: Widget (Maybe a) b } -> (Widget (Maybe a) (Maybe (Maybe a)) -> Widget (Maybe a) (Maybe a)) -> Widget (Maybe a) (Maybe a)
checkbox { labelContent } checked =
  div ( S.do
    div ( S.do
      Web.checkbox # checked # cl' "mdc-checkbox__native-control" # at' "id" id -- TODO define id' = at' "id" id
      div (S.do
        html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div empty # cl' "mdc-checkbox__mixedmark") # cl' "mdc-checkbox__background"
      div empty # cl' "mdc-checkbox__ripple") # cl' "mdc-checkbox" # bracket (gets _.sibling >>= newComponent material.checkbox."MDCCheckbox") (const $ pure) (const $ pure)
    label (labelContent >>> empty) # at' "for" id) # cl' "mdc-form-field" # bracket (gets _.sibling >>= newComponent material.formField."MDCFormField") (const $ pure) (const $ pure)
    where
      id = unsafePerformEffect uniqueId

-- TODO add html grouping
radioButton :: forall a b. { labelContent :: Widget a b } -> (Widget (Maybe a) (Maybe a) -> Widget a a) -> Widget a a
radioButton { labelContent } value =
  div (S.do
    div (S.do
        Web.radioButton # value # cl' "mdc-radio__native-control" # at' "id" uid
        div (S.do
          div empty # cl' "mdc-radio__outer-circle"
          div empty # cl' "mdc-radio__inner-circle") # cl' "mdc-radio__background"
        div empty # cl' "mdc-radio__ripple") # cl' "mdc-radio" # bracket (gets _.sibling >>= newComponent material.radio."MDCRadio") (const $ pure) (const $ pure)
    label (labelContent >>> empty) # at' "for" uid
  ) # cl' "mdc-form-field" # bracket (gets _.sibling >>= newComponent material.formField."MDCFormField") (const $ pure) (const $ pure)
    where
      uid = unsafePerformEffect uniqueId

headline1 :: forall a b. Widget a b -> Widget a b
headline1 w = h1 w # cl' "mdc-typography--headline1"

headline2 :: forall a b. Widget a b -> Widget a b
headline2 w = h2 w # cl' "mdc-typography--headline2"

headline3 :: forall a b. Widget a b -> Widget a b
headline3 w = h3 w # cl' "mdc-typography--headline3"

headline4 :: forall a b. Widget a b -> Widget a b
headline4 w = h4 w # cl' "mdc-typography--headline4"

headline5 :: forall a b. Widget a b -> Widget a b
headline5 w = h5 w # cl' "mdc-typography--headline5"

headline6 :: forall a b. Widget a b -> Widget a b
headline6 w = h6 w # cl' "mdc-typography--headline6"

subtitle1 :: forall a b. Widget a b -> Widget a b
subtitle1 w = p w # cl' "mdc-typography--subtitle1"

subtitle2 :: forall a b. Widget a b -> Widget a b
subtitle2 w = p w # cl' "mdc-typography--subtitle2"

button :: forall a b. Widget a b -> Widget a b
button w = span w # cl' "mdc-typography--button"

caption :: forall a b. Widget a b -> Widget a b
caption w = span w # cl' "mdc-typography--caption"

overline :: forall a b. Widget a b -> Widget a b
overline w = span w # cl' "mdc-typography--overline"

-- Widget transformers

body1 :: forall a b. Widget a b -> Widget a b
body1 w = p w # cl'"mdc-typography--body1"

body2 :: forall a b. Widget a b -> Widget a b
body2 w = p w # cl'"mdc-typography--body2"

elevation1 :: forall a b. Widget a b -> Widget a b
elevation1 w = div w # cl' "mdc-elevation--z1"

elevation10 :: forall a b. Widget a b -> Widget a b
elevation10 w = div w # cl' "mdc-elevation--z10" # at' "style" "padding: 25px" -- TODO padding added ad-hoc, to remove

elevation20 :: forall a b. Widget a b -> Widget a b
elevation20 w = div w # cl' "mdc-elevation--z20" # at' "style" "padding: 25px"-- TODO padding added ad-hoc, to remove

card :: forall a b. Widget a b -> Widget a b
card w = div w # cl' "mdc-card" # at' "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;"  -- TODO padding added ad-hoc, to remove

dialog :: forall a b. { title :: Widget a b } -> Widget a a -> Widget a a
dialog { title } content =
  aside (S.do
    div (S.do
      div (S.do
        h2 (title >>> empty) # cl' "mdc-dialog__title" # at' "id" "my-dialog-title"
        div content # cl' "mdc-dialog__content" # at' "id" "my-dialog-content"
      ) # cl' "mdc-dialog__surface" # at' "role" "alertdialog" # at' "aria-modal" "true" # at' "aria-labelledby" "my-dialog-title" # at' "aria-describedby" "my-dialog-content"
    ) # cl' "mdc-dialog__container"
    div empty # cl' "mdc-dialog__scrim"
  ) # cl' "mdc-dialog" # bracket initializeMdcDialog openMdcComponent closeMdcComponent
    where
      initializeMdcDialog = gets _.sibling >>= newComponent material.dialog."MDCDialog"
      openMdcComponent comp a = liftEffect do
        open comp
        pure a
      closeMdcComponent comp a = liftEffect do
        close comp
        pure a

snackbar :: forall a b. { label :: Widget a b } -> Widget a b
snackbar { label } =
  aside
    ( div
      ( div
        label # cl' "mdc-snackbar__label" # at' "aria-atomic" "false") # at' "role" "status" # at' "aria-relevant" "additions" # cl' "mdc-snackbar__surface") # cl' "mdc-snackbar" # bracket initializeMdcSnackbar openMdcComponent (const $ pure)
    where
      initializeMdcSnackbar = gets _.sibling >>= newComponent material.snackbar."MDCSnackbar"
      openMdcComponent comp a = liftEffect do
        open comp
        pure a

-- Private

newComponent :: forall m. MonadEffect m => ComponentClass -> Node -> m Component
newComponent classes node = liftEffect $ runEffectFn2 _new classes node

foreign import data Component :: Type
foreign import data ComponentClass :: Type
foreign import data WebUI :: Type

foreign import _new :: EffectFn2 ComponentClass Node Component
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
     }

foreign import open :: Component -> Effect Unit
foreign import close :: Component -> Effect Unit
