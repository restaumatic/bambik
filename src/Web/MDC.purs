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

import Control.Plus (empty)
import Data.Maybe (Maybe)
import Data.String (null)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Propagator (bracket)
import QualifiedDo.Semigroup as S
import Web (Widget, aside, clickable, div, h1, h2, h3, h4, h5, h6, html, label, p, span, input)
import Web (button, checkbox, radioButton) as Web
import Web.Internal.DOM (Node, attr)
import Web.Internal.DOMBuilder (getSibling)

-- Primitive widgets

containedButton :: forall a b. { label :: Widget a b } -> Widget a a
containedButton { label } =
  Web.button (attr "class" "mdc-button mdc-button--raised initAside-button") mempty (S.do
    div (attr "class" "mdc-button__ripple") mempty (empty :: Widget a a) -- TODO why we need to specify type?
    span (attr "class" "mdc-button__label") mempty (label >>> empty)) # bracket (getSibling >>= newComponent material.ripple."MDCRipple") (const $ pure) (const $ pure) # clickable

filledTextField :: forall a b. { floatingLabel :: Widget String b } -> (Widget String String -> Widget a a) -> Widget a a
filledTextField { floatingLabel } value =
  label (attr "class" "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") mempty (S.do
    span (attr "class" "mdc-text-field__ripple") mempty empty
    (S.do
      span (attr "class" "mdc-floating-label" <> attr "id" "my-label-id") (\currentInput -> if not (null currentInput) then attr "class" "mdc-floating-label--float-above" else mempty) (floatingLabel >>> empty)
      input (attr "class" "mdc-text-field__input" <> attr "type" "text" <> attr "aria-labelledby" "my-label-id") ) # value
    span (attr "class" "mdc-line-ripple") mempty empty ) # bracket (getSibling >>= newComponent material.textField."MDCTextField") (const $ pure) (const $ pure)

checkbox :: forall a b. { labelContent :: Widget (Maybe a) b } -> (Widget (Maybe a) (Maybe (Maybe a)) -> Widget (Maybe a) (Maybe a)) -> Widget (Maybe a) (Maybe a)
checkbox { labelContent } checked =
  div (attr "class" "mdc-form-field") mempty ( S.do
    div (attr "class" "mdc-checkbox") mempty ( S.do
      Web.checkbox (attr "class" "mdc-checkbox__native-control" <> attr "type" "checkbox" <> attr "id" id) # checked
      div (attr "class" "mdc-checkbox__background") mempty S.do
        html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div (attr "class" "mdc-checkbox__mixedmark") mempty empty
      div (attr "class" "mdc-checkbox__ripple") mempty empty ) # bracket (getSibling >>= newComponent material.checkbox."MDCCheckbox") (const $ pure) (const $ pure)
    label (attr "for" id) mempty (labelContent >>> empty) ) # bracket (getSibling >>= newComponent material.formField."MDCFormField") (const $ pure) (const $ pure)
    where
      id = unsafePerformEffect randomElementId

radioButton :: forall a b. { labelContent :: Widget a b } -> (Widget (Maybe a) (Maybe a) -> Widget a a) -> Widget a a
radioButton { labelContent } value =
  div (attr "class" "mdc-form-field") mempty (S.do
  div (attr "class" "mdc-radio") mempty (S.do
      Web.radioButton (attr "class" "mdc-radio__native-control" <> attr "id" id ) # value
      div (attr "class" "mdc-radio__background") mempty S.do
        div (attr "class" "mdc-radio__outer-circle") mempty empty
        div (attr "class" "mdc-radio__inner-circle") mempty empty
      div (attr "class" "mdc-radio__ripple") mempty empty) # bracket (getSibling >>= newComponent material.radio."MDCRadio") (const $ pure) (const $ pure)
  label (attr "for" id) mempty (labelContent >>> empty)
  )
  # bracket (getSibling >>= newComponent material.formField."MDCFormField") (const $ pure) (const $ pure)
    where
      id = unsafePerformEffect randomElementId

headline1 :: forall a b. Widget a b -> Widget a b
headline1 = h1 (attr "class" "mdc-typography--headline1") mempty

headline2 :: forall a b. Widget a b -> Widget a b
headline2 = h2 (attr "class" "mdc-typography--headline2") mempty

headline3 :: forall a b. Widget a b -> Widget a b
headline3 = h3 (attr "class" "mdc-typography--headline3") mempty

headline4 :: forall a b. Widget a b -> Widget a b
headline4 = h4 (attr "class" "mdc-typography--headline4") mempty

headline5 :: forall a b. Widget a b -> Widget a b
headline5 = h5 (attr "class" "mdc-typography--headline5") mempty

headline6 :: forall a b. Widget a b -> Widget a b
headline6 = h6 (attr "class" "mdc-typography--headline6") mempty

subtitle1 :: forall a b. Widget a b -> Widget a b
subtitle1 = p (attr "class" "mdc-typography--subtitle1") mempty

subtitle2 :: forall a b. Widget a b -> Widget a b
subtitle2 = p (attr "class" "mdc-typography--subtitle2") mempty

button :: forall a b. Widget a b -> Widget a b
button = span (attr "class" "mdc-typography--button") mempty

caption :: forall a b. Widget a b -> Widget a b
caption = span (attr "class" "mdc-typography--caption") mempty

overline :: forall a b. Widget a b -> Widget a b
overline = span (attr "class" "mdc-typography--overline") mempty

-- Widget transformers

body1 :: forall a b. Widget a b -> Widget a b
body1 = p (attr "class" "mdc-typography--body1") mempty

body2 :: forall a b. Widget a b -> Widget a b
body2 = p (attr "class" "mdc-typography--body2") mempty

elevation1 :: forall a b. Widget a b -> Widget a b
elevation1 = div (attr "class" "elevation-demo-surface mdc-elevation--z1") mempty

elevation10 :: forall a b. Widget a b -> Widget a b
elevation10 = div (attr "class" "elevation-demo-surface mdc-elevation--z10" <> attr "style" "padding: 25px") mempty -- TODO padding added ad-hoc, to remove

elevation20 :: forall a b. Widget a b -> Widget a b
elevation20 = div (attr "class" "elevation-demo-surface mdc-elevation--z20" <> attr "style" "padding: 25px") mempty -- TODO padding added ad-hoc, to remove

card :: forall a b. Widget a b -> Widget a b
card = div (attr "class" "mdc-card" <> attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;") mempty -- TODO padding added ad-hoc, to remove

dialog :: forall a b. { title :: Widget a b } -> Widget a a -> Widget a a
dialog { title } content =
  aside (attr "class" "mdc-dialog") mempty (S.do
    div (attr "class" "mdc-dialog__container") mempty S.do
      div (attr "class" "mdc-dialog__surface" <> attr "role" "alertdialog" <> attr "aria-modal" "true" <> attr "aria-labelledby" "my-dialog-title" <> attr "aria-describedby" "my-dialog-content") mempty S.do
        h2 (attr "class" "mdc-dialog__title" <> attr "id" "my-dialog-title") mempty (title >>> empty)
        div (attr "class" "mdc-dialog__content" <> attr "id" "my-dialog-content") mempty content
    div (attr "class" "mdc-dialog__scrim") mempty empty ) # bracket initializeMdcDialog openMdcComponent closeMdcComponent
    where
      initializeMdcDialog = getSibling >>= newComponent material.dialog."MDCDialog"
      openMdcComponent comp a = liftEffect do
        open comp
        pure a
      closeMdcComponent comp a = liftEffect do
        close comp
        pure a

snackbar :: forall a b. { label :: Widget a b } -> Widget a b
snackbar { label } =
  aside (attr "class" "mdc-snackbar") mempty
    ( div (attr "class" "mdc-snackbar__surface" <> attr "role" "status" <> attr "aria-relevant" "additions") mempty
      ( div (attr "class" "mdc-snackbar__label" <> attr "aria-atomic" "false") mempty
        label ) ) # bracket initializeMdcSnackbar openMdcComponent (const $ pure)
    where
      initializeMdcSnackbar = getSibling >>= newComponent material.snackbar."MDCSnackbar"
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
foreign import randomElementId :: Effect String
