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

import Data.Maybe (Maybe, maybe)
import Data.String (null)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Specular.Dom.Builder (Node, addEventListener, attr, classes, removeNode)
import Web (Widget, aside, div, h1, h2, h3, h4, h5, h6, html, label, p, pzero, span, text, textInput, (<^), (^), (^>))
import Web (button, checkbox, radioButton) as Web

-- Primitive widgets

containedButton :: forall a. (Widget String String -> Widget a a) -> Widget a a
containedButton label =
  Web.button (classes "mdc-button mdc-button--raised initAside-button") mempty ((\node _ _ -> do
    void $ newComponent material.ripple."MDCRipple" node
    pure mempty) <> (\node ema action-> do
    addEventListener "click" node $ const $ ema >>= maybe mempty action
    pure mempty))
    ( div (classes "mdc-button__ripple") mempty mempty pzero <^
      span (classes "mdc-button__label") mempty mempty
        (text # label))

filledTextField :: (Widget String String -> Widget String String) -> Widget String String
filledTextField floatingLabel =
  label (classes "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") mempty (\node _ -> void $ newComponent material.textField."MDCTextField" node) $
    (span (classes "mdc-text-field__ripple") mempty mempty pzero)
    ^
    (span (classes "mdc-floating-label" <> attr "id" "my-label-id") (\value -> if not (null value) then classes "mdc-floating-label--float-above" else mempty)) mempty
      (text # floatingLabel)
    ^>
    (textInput (classes "mdc-text-field__input" <> attr "type" "text" <> attr "aria-labelledby" "my-label-id"))
    ^
    (span (classes "mdc-line-ripple") mempty mempty pzero)

checkbox :: (Widget String String -> Widget Boolean Boolean) -> Widget Boolean Boolean
checkbox labelCaption =
  div (classes "mdc-form-field") mempty (\node _ -> void $ newComponent material.formField."MDCFormField" node)
    ( div (classes "mdc-checkbox") mempty (\node _ -> void $ newComponent material.checkbox."MDCCheckbox" node)
      ( Web.checkbox (classes "mdc-checkbox__native-control" <> attr "type" "checkbox" <> attr "id" id)
      ^ div (classes "mdc-checkbox__background") mempty mempty
        ( html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        ^ div (classes "mdc-checkbox__mixedmark") mempty mempty pzero) ^
      div (classes "mdc-checkbox__ripple") mempty mempty pzero) <^
      label (attr "for" id) mempty mempty
        ( text # labelCaption ))
    where
      id = unsafePerformEffect randomElementId

radioButton :: forall a. (Widget String String -> Widget (Maybe a) (Maybe a)) -> Widget (Maybe a) (Maybe a)
radioButton labelCaption =
  div (classes "mdc-form-field") mempty mempty
  ((div (classes "mdc-radio") mempty (\node _ -> void $ newComponent material.radio."MDCRadio" node) $
      Web.radioButton (classes "mdc-radio__native-control" <> attr "id" id ) <^
      div (classes "mdc-radio__background") mempty mempty
        ( div (classes "mdc-radio__outer-circle") mempty mempty pzero ^
          div (classes "mdc-radio__inner-circle") mempty mempty pzero) ^
      (div (classes "mdc-radio__ripple") mempty mempty pzero)
    ) <^
    label (attr "for" id) mempty mempty
      ( text # labelCaption ))
    where
      id = unsafePerformEffect randomElementId

headline1 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline1 content = h1 (classes "mdc-typography--headline1") mempty mempty content

headline2 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline2 content = h2 (classes "mdc-typography--headline2") mempty mempty content

headline3 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline3 content = h3 (classes "mdc-typography--headline3") mempty mempty content

headline4 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline4 content = h4 (classes "mdc-typography--headline4") mempty mempty content

headline5 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline5 content = h5 (classes "mdc-typography--headline5") mempty mempty content

headline6 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline6 content = h6 (classes "mdc-typography--headline6") mempty mempty content

subtitle1 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
subtitle1 label = p (classes "mdc-typography--subtitle1") mempty mempty
  ( text # label )

subtitle2 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
subtitle2 label = p (classes "mdc-typography--subtitle2") mempty mempty
  ( text # label )

button :: forall a b. (Widget String String -> Widget a b) -> Widget a b
button label = span (classes "mdc-typography--button") mempty mempty
  ( text # label )

caption :: forall a b. (Widget String String -> Widget a b) -> Widget a b
caption label = span (classes "mdc-typography--caption") mempty mempty
  ( text # label )

overline :: forall a b. (Widget String String -> Widget a b) -> Widget a b
overline label = span (classes "mdc-typography--overline") mempty mempty
  ( text # label )

-- Widget transformers

body1 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
body1 content = p (classes "mdc-typography--body1") mempty mempty
  ( text # content )

body2 :: forall a b. Widget a b -> Widget a b
body2 = p (classes "mdc-typography--body2") mempty mempty

elevation1 :: forall a b. Widget a b -> Widget a b
elevation1 = div (classes "elevation-demo-surface mdc-elevation--z1") mempty mempty

elevation10 :: forall a b. Widget a b -> Widget a b
elevation10 = div (classes "elevation-demo-surface mdc-elevation--z10" <> attr "style" "padding: 25px") mempty mempty -- TODO padding added ad-hoc, to remove

elevation20 :: forall a b. Widget a b -> Widget a b
elevation20 = div (classes "elevation-demo-surface mdc-elevation--z20" <> attr "style" "padding: 25px") mempty mempty -- TODO padding added ad-hoc, to remove

card :: forall a b. Widget a b -> Widget a b
card = div (classes "mdc-card" <> attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;") mempty mempty -- TODO padding added ad-hoc, to remove

dialog :: forall a b. (Widget String String -> Widget a b) -> Widget a b -> Widget a b
dialog title w =
  aside (classes "mdc-dialog") mempty initAside
    ( div (classes "mdc-dialog__container") mempty mempty
      ( div (classes "mdc-dialog__surface" <> attr "role" "alertdialog" <> attr "aria-modal" "true" <> attr "aria-labelledby" "my-dialog-title" <> attr "aria-describedby" "my-dialog-content") mempty mempty
        ( h2 (classes "mdc-dialog__title" <> attr "id" "my-dialog-title") mempty mempty title
        ^> div (classes "mdc-dialog__content" <> attr "id" "my-dialog-content") mempty mempty w) ) <^
      div (classes "mdc-dialog__scrim") mempty mempty pzero )
    where
      initAside node _ _ = do
        comp <- newComponent material.dialog."MDCDialog" node
        open comp
        pure $ do
          close comp
          removeNode node

snackbar :: forall a. Number -> (Widget String String -> Widget a a) -> Widget a a
snackbar ms label =
  aside (classes "mdc-snackbar") mempty initAside
    ( div (classes "mdc-snackbar__surface" <> attr "role" "status" <> attr "aria-relevant" "additions") mempty mempty
      ( div (classes "mdc-snackbar__label" <> attr "aria-atomic" "false") mempty mempty
        (text # label )))
    where
      initAside node _ _ = do
        comp <- newComponent material.snackbar."MDCSnackbar" node
        open comp
        launchAff_ do
          delay $ Milliseconds ms
          liftEffect $ do
            close comp
            removeNode node
        pure mempty

-- Private

newComponent :: ComponentClass -> Node -> Effect Component
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
