module Web.MDC
  ( body1
  , body2
  , button
  , caption
  , checkbox
  , containedButton
  , elevation1
  , elevation9
  , filledTextField
  , headline1
  , headline2
  , headline3
  , headline4
  , headline5
  , headline6
  , overline
  , radioButton
  , subtitle1
  , subtitle2
  )
  where

import Prelude hiding (div)

import Data.Maybe (Maybe)
import Data.Profunctor.Plus (pzero, (<^), (^), (^>))
import Data.String (null)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Builder (Node, addEventListener, attr, classes)
import Web (Widget, div, element, label, label', span, text, textInput)
import Web as Web

-- Primitives

containedButton :: forall a b c. (Widget String Void -> Widget a b) -> (a -> Effect Unit) -> Widget a c
containedButton label action =
  Web.button (classes "mdc-button mdc-button--raised foo-button") mempty ((\node _ -> mdcWith material.ripple."MDCRipple" node) <> (\node ea -> addEventListener "click" node $ const $ ea >>= action))
    ( div (classes "mdc-button__ripple") mempty mempty pzero <^
      span (classes "mdc-button__label") mempty mempty
        (text # label))

filledTextField :: forall a. (Widget String Void -> Widget String a) -> Widget String String
filledTextField floatingLabel =
  label (classes "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") mempty (\node _ -> mdcWith material.textField."MDCTextField" node) $
    (span (classes "mdc-text-field__ripple") mempty mempty pzero)
    ^
    (span (classes "mdc-floating-label" <> attr "id" "my-label-id") (\value -> if not (null value) then classes "mdc-floating-label--float-above" else mempty)) mempty
      (text # floatingLabel)
    ^>
    (textInput (classes "mdc-text-field__input" <> attr "type" "text" <> attr "aria-labelledby" "my-label-id"))
    ^
    (span (classes "mdc-line-ripple") mempty mempty pzero)

checkbox :: forall a. (Widget String Void -> Widget Boolean a) -> Widget Boolean Boolean
checkbox label =
  div (classes "mdc-form-field") mempty mempty -- (\node _ -> mdcWith material.formField."MDCFormField" node mempty)
    ( div (classes "mdc-checkbox") mempty (\node _ -> mdcWith material.checkbox."MDCCheckbox" node)
      ( Web.checkbox (classes "mdc-checkbox__native-control") ^
        div (classes "mdc-checkbox__background") mempty mempty
        ( element "svg" (classes "mdc-checkbox__checkmark" <> attr "viewBox" "0 0 24 24") mempty mempty
          ( element "path" (classes "mdc-checkbox__checkmark-path" <> attr "fill" "none" <> attr "d" "M1.73,12.91 8.1,19.28 22.79,4.59") mempty mempty pzero) ^
        div (classes "mdc-checkbox__mixedmark") mempty mempty pzero) ^
      div (classes "mdc-checkbox__ripple") mempty mempty pzero) <^
      label'
        ( text # label ))

radioButton :: forall a b. (Widget String Void -> Widget (Maybe a) b) -> Widget (Maybe a) (Maybe a)
radioButton label =
  div (classes "mdc-form-field") mempty mempty
  ((div (classes "mdc-radio") mempty (\node _ -> mdcWith material.radio."MDCRadio" node) $
      Web.radioButton (classes "mdc-radio__native-control" <> attr "id" "radio-1" ) <^
      div (classes "mdc-radio__background") mempty mempty
        ( div (classes "mdc-radio__outer-circle") mempty mempty pzero ^
          div (classes "mdc-radio__inner-circle") mempty mempty pzero) ^
      (div (classes "mdc-radio__ripple") mempty mempty pzero)
    ) <^
    label'
      ( text # label ))

headline1 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
headline1 label = element "h1" (classes "mdc-typography--headline1") mempty mempty
 ( text # label )

headline2 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
headline2 label = element "h2" (classes "mdc-typography--headline2") mempty mempty
 ( text # label )

headline3 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
headline3 label = element "h3" (classes "mdc-typography--headline3") mempty mempty
 ( text # label )

headline4 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
headline4 label = element "h4" (classes "mdc-typography--headline4") mempty mempty
 ( text # label )

headline5 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
headline5 label = element "h5" (classes "mdc-typography--headline5") mempty mempty
 ( text # label )

headline6 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
headline6 label = element "h6" (classes "mdc-typography--headline6") mempty mempty
 ( text # label )

subtitle1 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
subtitle1 label = element "h6" (classes "mdc-typography--subtitle1") mempty mempty
  ( text # label )

subtitle2 :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
subtitle2 label = element "h6" (classes "mdc-typography--subtitle2") mempty mempty
  ( text # label )

button :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
button label = span (classes "mdc-typography--button") mempty mempty
  ( text # label )

caption :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
caption label = span (classes "mdc-typography--caption") mempty mempty
  ( text # label )

overline :: forall a b. (Widget String Void -> Widget a b) -> Widget a b
overline label = span (classes "mdc-typography--overline") mempty mempty
  ( text # label )

-- Optics

body1 :: forall a b. Widget a b -> Widget a b
body1 = element "p" (classes "mdc-typography--body1") mempty mempty

body2 :: forall a b. Widget a b -> Widget a b
body2 = element "p" (classes "mdc-typography--body2") mempty mempty

elevation1 :: forall a b. Widget a b -> Widget a b
elevation1 = div (classes "elevation-demo-surface mdc-elevation--z1") mempty mempty

elevation9 :: forall a b. Widget a b -> Widget a b
elevation9 = div (classes "elevation-demo-surface mdc-elevation--z9" <> attr "style" "padding: 25px") mempty mempty -- TODO padding added ad-hoc, to remove

-- Private

mdcWith :: ComponentClass -> Node -> Effect Unit
mdcWith classes node = void $ liftEffect $ runEffectFn2 _new classes node

foreign import data ComponentClass :: Type
foreign import data WebUI :: Type
foreign import _new :: EffectFn2 ComponentClass Node WebUI
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


-- TODO

-- list :: forall a. Component a -> Component (Array a)
-- list c = wrapWebComponent \callbackas -> do -- -> Builder Unit (UserInput a -> Effect Unit)
--   slot <- newSlot
--   asRef <- liftEffect $ new []
--   pure $ \as -> replaceSlot slot do
--     liftEffect $ write as asRef
--     void $ elAttr "ul" ("class" := "mdc-list mdc-list--two-line") $
--       forWithIndex_ as \i a -> elAttr "li" ("class" := "mdc-list-item") do
--         void $ elAttr "span'" ("class" := "mdc-list-item__ripple") (pure unit)
--         void $ elAttr "span'" ("class" := "mdc-list-item__text") do
--           void $ elAttr "span'" ("class" := "mdc-list-item__secondary-text") $ text $ "Item " <> show i
--           void $ elAttr "span'" ("class" := "mdc-list-item__primary-text") do
--             update <- unwrap c \value -> do
--               newas <- modify (\currentAs -> fromMaybe currentAs (updateAt i value currentAs)) asRef
--               callbackas newas
--             liftEffect $ update a


