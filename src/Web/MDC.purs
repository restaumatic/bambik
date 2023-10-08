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

import Data.Maybe (Maybe)
import Data.String (null)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Propagator (bracket, hush)
import Web (Widget, aside, clickable, div, h1, h2, h3, h4, h5, h6, html, label, p, span, textInput)
import Web (button, checkbox, radioButton) as Web
import Web.Internal.DOM (Node, attr, classes, getCurrentNode)
import QualifiedDo.Semigroup as S

-- Primitive widgets

containedButton :: forall a b. { label :: Widget a b } -> Widget a a
containedButton { label } =
  Web.button (classes "mdc-button mdc-button--raised initAside-button") mempty (S.do
    div (classes "mdc-button__ripple") mempty (mempty :: Widget a a)
    span (classes "mdc-button__label") mempty S.do
      label >>> hush) # bracket (getCurrentNode >>= newComponent material.ripple."MDCRipple") (const $ pure) (const $ pure) # clickable

filledTextField :: forall a b. { floatingLabel :: Widget String b } -> (Widget String String -> Widget a a) -> Widget a a
filledTextField { floatingLabel } value =
  label (classes "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") mempty (S.do
    span (classes "mdc-text-field__ripple") mempty mempty
    ( span (classes "mdc-floating-label" <> attr "id" "my-label-id") (\currentInput -> if not (null currentInput) then classes "mdc-floating-label--float-above" else mempty) S.do
      floatingLabel >>> hush
      textInput (classes "mdc-text-field__input" <> attr "type" "text" <> attr "aria-labelledby" "my-label-id") ) # value
    span (classes "mdc-line-ripple") mempty mempty ) # bracket (getCurrentNode >>= newComponent material.textField."MDCTextField") (const $ pure) (const $ pure)

checkbox :: forall a b. { labelContent :: Widget a b } -> (Widget Boolean Boolean -> Widget a a) -> Widget a a
checkbox { labelContent } checked =
  div (classes "mdc-form-field") mempty ( S.do
    div (classes "mdc-checkbox") mempty ( S.do
      Web.checkbox (classes "mdc-checkbox__native-control" <> attr "type" "checkbox" <> attr "id" id) # checked
      div (classes "mdc-checkbox__background") mempty S.do
        html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div (classes "mdc-checkbox__mixedmark") mempty mempty
      div (classes "mdc-checkbox__ripple") mempty mempty ) # bracket (getCurrentNode >>= newComponent material.checkbox."MDCCheckbox") (const $ pure) (const $ pure)
    label (attr "for" id) mempty (labelContent >>> hush) ) # bracket (getCurrentNode >>= newComponent material.formField."MDCFormField") (const $ pure) (const $ pure)
    where
      id = unsafePerformEffect randomElementId

radioButton :: forall a b. { labelContent :: Widget a b } -> (Widget (Maybe a) (Maybe a) -> Widget a a) -> Widget a a
radioButton { labelContent } value =
  div (classes "mdc-form-field") mempty (S.do
  div (classes "mdc-radio") mempty (S.do
      Web.radioButton (classes "mdc-radio__native-control" <> attr "id" id ) # value
      div (classes "mdc-radio__background") mempty S.do
        div (classes "mdc-radio__outer-circle") mempty mempty
        div (classes "mdc-radio__inner-circle") mempty mempty
      div (classes "mdc-radio__ripple") mempty mempty) # bracket (getCurrentNode >>= newComponent material.radio."MDCRadio") (const $ pure) (const $ pure)
  label (attr "for" id) mempty (labelContent >>> hush)
  )
  # bracket (getCurrentNode >>= newComponent material.formField."MDCFormField") (const $ pure) (const $ pure)
    where
      id = unsafePerformEffect randomElementId

headline1 :: forall a b. Widget a b -> Widget a b
headline1 = h1 (classes "mdc-typography--headline1") mempty

headline2 :: forall a b. Widget a b -> Widget a b
headline2 = h2 (classes "mdc-typography--headline2") mempty

headline3 :: forall a b. Widget a b -> Widget a b
headline3 = h3 (classes "mdc-typography--headline3") mempty

headline4 :: forall a b. Widget a b -> Widget a b
headline4 = h4 (classes "mdc-typography--headline4") mempty

headline5 :: forall a b. Widget a b -> Widget a b
headline5 = h5 (classes "mdc-typography--headline5") mempty

headline6 :: forall a b. Widget a b -> Widget a b
headline6 = h6 (classes "mdc-typography--headline6") mempty

subtitle1 :: forall a b. Widget a b -> Widget a b
subtitle1 = p (classes "mdc-typography--subtitle1") mempty

subtitle2 :: forall a b. Widget a b -> Widget a b
subtitle2 = p (classes "mdc-typography--subtitle2") mempty

button :: forall a b. Widget a b -> Widget a b
button = span (classes "mdc-typography--button") mempty

caption :: forall a b. Widget a b -> Widget a b
caption = span (classes "mdc-typography--caption") mempty

overline :: forall a b. Widget a b -> Widget a b
overline = span (classes "mdc-typography--overline") mempty

-- Widget transformers

body1 :: forall a b. Widget a b -> Widget a b
body1 = p (classes "mdc-typography--body1") mempty

body2 :: forall a b. Widget a b -> Widget a b
body2 = p (classes "mdc-typography--body2") mempty

elevation1 :: forall a b. Widget a b -> Widget a b
elevation1 = div (classes "elevation-demo-surface mdc-elevation--z1") mempty

elevation10 :: forall a b. Widget a b -> Widget a b
elevation10 = div (classes "elevation-demo-surface mdc-elevation--z10" <> attr "style" "padding: 25px") mempty -- TODO padding added ad-hoc, to remove

elevation20 :: forall a b. Widget a b -> Widget a b
elevation20 = div (classes "elevation-demo-surface mdc-elevation--z20" <> attr "style" "padding: 25px") mempty -- TODO padding added ad-hoc, to remove

card :: forall a b. Widget a b -> Widget a b
card = div (classes "mdc-card" <> attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;") mempty -- TODO padding added ad-hoc, to remove

dialog :: forall a b. { title :: Widget a b } -> Widget a a -> Widget a a
dialog { title } content =
  aside (classes "mdc-dialog") mempty (S.do
    div (classes "mdc-dialog__container") mempty S.do
      div (classes "mdc-dialog__surface" <> attr "role" "alertdialog" <> attr "aria-modal" "true" <> attr "aria-labelledby" "my-dialog-title" <> attr "aria-describedby" "my-dialog-content") mempty S.do
        h2 (classes "mdc-dialog__title" <> attr "id" "my-dialog-title") mempty (title >>> hush)
        div (classes "mdc-dialog__content" <> attr "id" "my-dialog-content") mempty content
    div (classes "mdc-dialog__scrim") mempty mempty ) # bracket initializeMdcDialog openMdcComponent closeMdcComponent
    where
      initializeMdcDialog = getCurrentNode >>= newComponent material.dialog."MDCDialog"
      openMdcComponent comp a = liftEffect do
        open comp
        pure a
      closeMdcComponent comp a = liftEffect do
        close comp
        pure a

snackbar :: forall a b. { label :: Widget a b } -> Widget a b
snackbar { label } =
  aside (classes "mdc-snackbar") mempty
    ( div (classes "mdc-snackbar__surface" <> attr "role" "status" <> attr "aria-relevant" "additions") mempty
      ( div (classes "mdc-snackbar__label" <> attr "aria-atomic" "false") mempty
        label ) ) # bracket initializeMdcSnackbar openMdcComponent (const $ pure)
    where
      initializeMdcSnackbar = getCurrentNode >>= newComponent material.snackbar."MDCSnackbar"
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
