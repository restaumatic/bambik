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
import Data.Profunctor.Plus (pzero, (<^), (^), (^>))
import Data.String (null)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Builder (Node, addEventListener, attr, classes)
import Web (Widget, aside, div, element, h2, label, label', span, text, textInput)
import Web as Web

-- Primitives

containedButton :: forall a. (Widget String String -> Widget a a) -> Widget a a
containedButton label =
  Web.button (classes "mdc-button mdc-button--raised initAside-button") mempty init
    ( div (classes "mdc-button__ripple") mempty mempty pzero <^
      span (classes "mdc-button__label") mempty mempty
        (text # label))
    where
      init node ea action _ = do
        void $ newComponent material.ripple."MDCRipple" node
        addEventListener "click" node $ const $ ea >>= action
        pure mempty

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
checkbox label =
  div (classes "mdc-form-field") mempty (\node _ -> void $ newComponent material.formField."MDCFormField" node)
    ( div (classes "mdc-checkbox") mempty (\node _ -> void $ newComponent material.checkbox."MDCCheckbox" node)
      ( Web.checkbox (classes "mdc-checkbox__native-control") ^
        div (classes "mdc-checkbox__background") mempty mempty
        ( element "svg" (classes "mdc-checkbox__checkmark" <> attr "viewBox" "0 0 24 24") mempty mempty
          ( element "path" (classes "mdc-checkbox__checkmark-path" <> attr "fill" "none" <> attr "d" "M1.73,12.91 8.1,19.28 22.79,4.59") mempty mempty pzero) ^
        div (classes "mdc-checkbox__mixedmark") mempty mempty pzero) ^
      div (classes "mdc-checkbox__ripple") mempty mempty pzero) <^
      label'
        ( text # label ))

radioButton :: forall a. (Widget String String -> Widget (Maybe a) (Maybe a)) -> Widget (Maybe a) (Maybe a)
radioButton label =
  div (classes "mdc-form-field") mempty mempty
  ((div (classes "mdc-radio") mempty (\node _ -> void $ newComponent material.radio."MDCRadio" node) $
      Web.radioButton (classes "mdc-radio__native-control" <> attr "id" "radio-1" ) <^
      div (classes "mdc-radio__background") mempty mempty
        ( div (classes "mdc-radio__outer-circle") mempty mempty pzero ^
          div (classes "mdc-radio__inner-circle") mempty mempty pzero) ^
      (div (classes "mdc-radio__ripple") mempty mempty pzero)
    ) <^
    label'
      ( text # label ))

headline1 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline1 label = element "h1" (classes "mdc-typography--headline1") mempty mempty
 ( text # label )

headline2 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline2 label = element "h2" (classes "mdc-typography--headline2") mempty mempty
 ( text # label )

headline3 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline3 label = element "h3" (classes "mdc-typography--headline3") mempty mempty
 ( text # label )

headline4 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline4 label = element "h5" (classes "mdc-typography--headline4") mempty mempty
 ( text # label )

headline5 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline5 label = element "h5" (classes "mdc-typography--headline5") mempty mempty
 ( text # label )

headline6 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
headline6 label = element "h6" (classes "mdc-typography--headline6") mempty mempty
 ( text # label )

subtitle1 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
subtitle1 label = element "p" (classes "mdc-typography--subtitle1") mempty mempty
  ( text # label )

subtitle2 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
subtitle2 label = element "p" (classes "mdc-typography--subtitle2") mempty mempty
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

-- Optics

body1 :: forall a b. (Widget String String -> Widget a b) -> Widget a b
body1 content = element "p" (classes "mdc-typography--body1") mempty mempty
  ( text # content )

body2 :: forall a b. Widget a b -> Widget a b
body2 = element "p" (classes "mdc-typography--body2") mempty mempty

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
        ( h2 (classes "mdc-dialog__title" <> attr "id" "my-dialog-title") mempty mempty
          ( text # title ) ^>
          div (classes "mdc-dialog__content" <> attr "id" "my-dialog-content") mempty mempty w) ) <^
      div (classes "mdc-dialog__scrim") mempty mempty pzero )
    where
      initAside node _ _ ctx = do
        comp <- newComponent material.dialog."MDCDialog" node
        open comp
        pure $ do
          close comp
          ctx.destroy

snackbar :: forall a. Number -> (Widget String String -> Widget a a) -> Widget a a
snackbar ms label =
  aside (classes "mdc-snackbar") mempty initAside
    ( div (classes "mdc-snackbar__surface" <> attr "role" "status" <> attr "aria-relevant" "additions") mempty mempty
      ( div (classes "mdc-snackbar__label" <> attr "aria-atomic" "false") mempty mempty
        (text # label )))
    where
      initAside node _ _ context = do
        comp <- newComponent material.snackbar."MDCSnackbar" node
        open comp
        launchAff_ do
          delay $ Milliseconds ms
          liftEffect $ do
            close comp
            context.destroy
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
