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
import Web (Widget, WidgetOptics, aside, bracket, div, h1, h2, h3, h4, h5, h6, html, label, p, pzero, span, text, textInput, (<^), (^), (^>))
import Web (button, checkbox, radioButton) as Web
import Web.Internal.DOM (Node, attr, classes, getCurrentNode)

-- Primitive widgets

containedButton :: forall a. (WidgetOptics String String a a) -> Widget a a
containedButton label =
  Web.button (classes "mdc-button mdc-button--raised initAside-button") mempty
    ( div (classes "mdc-button__ripple") mempty pzero
    <^ span (classes "mdc-button__label") mempty
      ( text # label ) ) # bracket (getCurrentNode >>= newComponent material.ripple."MDCRipple") mempty mempty

filledTextField :: forall a b. { caption :: WidgetOptics String b a a, value :: WidgetOptics String String a a } -> Widget a a
filledTextField { caption, value } =
  label (classes "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") mempty
    ( span (classes "mdc-text-field__ripple") mempty pzero
    -- ^ span (classes "mdc-floating-label" <> attr "id" "my-label-id") (\value -> if not (null value) then classes "mdc-floating-label--float-above" else mempty)
    --   ( text # caption )
    ^ textInput (classes "mdc-text-field__input" <> attr "type" "text" <> attr "aria-labelledby" "my-label-id") # value
    ^ span (classes "mdc-line-ripple") mempty pzero ) # bracket (getCurrentNode >>= newComponent material.textField."MDCTextField") mempty mempty

checkbox :: forall a b. { caption :: WidgetOptics String b a a, checked :: WidgetOptics Boolean Boolean a a } -> Widget a a
checkbox { caption, checked } =
  div (classes "mdc-form-field") mempty
    ( div (classes "mdc-checkbox") mempty
      ( Web.checkbox (classes "mdc-checkbox__native-control" <> attr "type" "checkbox" <> attr "id" id) # checked
      ^ div (classes "mdc-checkbox__background") mempty
        ( html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        ^ div (classes "mdc-checkbox__mixedmark") mempty pzero)
      ^ div (classes "mdc-checkbox__ripple") mempty pzero ) # bracket (getCurrentNode >>= newComponent material.checkbox."MDCCheckbox") mempty mempty
    ^ label (attr "for" id) mempty
      ( text # caption ) ) # bracket (getCurrentNode >>= newComponent material.formField."MDCFormField") mempty mempty
    where
      id = unsafePerformEffect randomElementId

radioButton :: forall a b. { caption :: WidgetOptics String b a a, value :: WidgetOptics (Maybe a) (Maybe a) a a } -> Widget a a
radioButton { caption, value } =
  div (classes "mdc-form-field") mempty
  ( div (classes "mdc-radio") mempty
      ( Web.radioButton (classes "mdc-radio__native-control" <> attr "id" id ) # value
      <^ div (classes "mdc-radio__background") mempty
        ( div (classes "mdc-radio__outer-circle") mempty pzero
        ^ div (classes "mdc-radio__inner-circle") mempty pzero)
        ^ div (classes "mdc-radio__ripple") mempty pzero ) # bracket (getCurrentNode >>= newComponent material.radio."MDCRadio") mempty mempty
  ^ label (attr "for" id) mempty
      ( text # caption ) ) # bracket (getCurrentNode >>= newComponent material.formField."MDCFormField") mempty mempty
    where
      id = unsafePerformEffect randomElementId

headline1 :: forall a b. WidgetOptics String String a b -> Widget a b
headline1 content = h1 (classes "mdc-typography--headline1") mempty content

headline2 :: forall a b. WidgetOptics String String a b -> Widget a b
headline2 content = h2 (classes "mdc-typography--headline2") mempty content

headline3 :: forall a b. WidgetOptics String String a b -> Widget a b
headline3 content = h3 (classes "mdc-typography--headline3") mempty content

headline4 :: forall a b. WidgetOptics String String a b -> Widget a b
headline4 content = h4 (classes "mdc-typography--headline4") mempty content

headline5 :: forall a b. WidgetOptics String String a b -> Widget a b
headline5 content = h5 (classes "mdc-typography--headline5") mempty content

headline6 :: forall a b. WidgetOptics String String a b -> Widget a b
headline6 content = h6 (classes "mdc-typography--headline6") mempty content

subtitle1 :: forall a b. WidgetOptics String String a b -> Widget a b
subtitle1 label = p (classes "mdc-typography--subtitle1") mempty
  ( text # label )

subtitle2 :: forall a b. WidgetOptics String String a b -> Widget a b
subtitle2 label = p (classes "mdc-typography--subtitle2") mempty
  ( text # label )

button :: forall a b. WidgetOptics String String a b -> Widget a b
button label = span (classes "mdc-typography--button") mempty
  ( text # label )

caption :: forall a b. WidgetOptics String String a b -> Widget a b
caption label = span (classes "mdc-typography--caption") mempty
  ( text # label )

overline :: forall a b. WidgetOptics String String a b -> Widget a b
overline label = span (classes "mdc-typography--overline") mempty
  ( text # label )

-- Widget transformers

body1 :: forall a b. WidgetOptics String String a b -> Widget a b
body1 content = p (classes "mdc-typography--body1") mempty
  ( text # content )

body2 :: forall a b. WidgetOptics a b a b
body2 = p (classes "mdc-typography--body2") mempty

elevation1 :: forall a b. WidgetOptics a b a b
elevation1 = div (classes "elevation-demo-surface mdc-elevation--z1") mempty

elevation10 :: forall a b. WidgetOptics a b a b
elevation10 = div (classes "elevation-demo-surface mdc-elevation--z10" <> attr "style" "padding: 25px") mempty -- TODO padding added ad-hoc, to remove

elevation20 :: forall a b. WidgetOptics a b a b
elevation20 = div (classes "elevation-demo-surface mdc-elevation--z20" <> attr "style" "padding: 25px") mempty -- TODO padding added ad-hoc, to remove

card :: forall a b. WidgetOptics a b a b
card = div (classes "mdc-card" <> attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;") mempty -- TODO padding added ad-hoc, to remove

dialog :: forall a b c d. { title :: WidgetOptics String String a b, content :: WidgetOptics c d a b } -> Widget c d -> Widget a b
dialog { title, content } w =
  aside (classes "mdc-dialog") mempty
    ( div (classes "mdc-dialog__container") mempty
      ( div (classes "mdc-dialog__surface" <> attr "role" "alertdialog" <> attr "aria-modal" "true" <> attr "aria-labelledby" "my-dialog-title" <> attr "aria-describedby" "my-dialog-content") mempty
        ( h2 (classes "mdc-dialog__title" <> attr "id" "my-dialog-title") mempty title
        ^> div (classes "mdc-dialog__content" <> attr "id" "my-dialog-content") mempty w # content) )
      <^ div (classes "mdc-dialog__scrim") mempty pzero ) # bracket initializeMdcDialog openMdcComponent closeMdcComponent
    where
      initializeMdcDialog = getCurrentNode >>= newComponent material.dialog."MDCDialog"
      openMdcComponent comp _ = open comp
      closeMdcComponent comp _ = close comp

snackbar :: forall a b c. WidgetOptics String String a c -> Widget a b
snackbar label =
  aside (classes "mdc-snackbar") mempty
    ( div (classes "mdc-snackbar__surface" <> attr "role" "status" <> attr "aria-relevant" "additions") mempty
      ( div (classes "mdc-snackbar__label" <> attr "aria-atomic" "false") mempty
        ( text # label
        ^> pzero ) ) ) # bracket initializeMdcSnackbar openMdcComponent mempty
    where
      initializeMdcSnackbar = getCurrentNode >>= newComponent material.snackbar."MDCSnackbar"
      openMdcComponent comp _ = open comp

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
