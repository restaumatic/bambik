module Web.MDC
  ( containedButton
  , checkbox
  , filledTextField
  , radioButton
  )
  where

import Prelude hiding (div)

import Data.Invariant.Transformers.Changed (Changed(..))
import Data.Maybe (Maybe)
import Data.Profunctor.Plus (pzero, (<^), (^))
import Data.String (null)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Builder (Node, attr, classes)
import Web (Component, button, div, element, label, onClick, radio, span, text, textInput)
import Web as Web

containedButton :: forall a. (a -> Effect Unit) -> Component a -> Component a
containedButton action wrapped =
  button (classes "mdc-button mdc-button--raised foo-button") mempty ((\node _ -> mdcWith material.ripple."MDCRipple" node mempty) <> onClick action) $
    (div (classes "mdc-button__ripple") mempty mempty pzero)
    ^
    (span (classes "mdc-button__label") mempty mempty wrapped)

filledTextField :: String -> Component String
filledTextField hintText =
  label (classes "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") mempty (\node _ -> mdcWith material.textField."MDCTextField" node mempty) $
    (span (classes "mdc-text-field__ripple") mempty mempty pzero)
    ^
    (span (classes "mdc-floating-label" <> attr "id" "my-label-id") (\(Changed _ value) -> if not (null value) then classes "mdc-floating-label--float-above" else mempty)) mempty (text hintText)
    ^
    (textInput (classes "mdc-text-field__input" <> attr "type" "text" <> attr "aria-labelledby" "my-label-id"))
    ^
    (span (classes "mdc-line-ripple") mempty mempty pzero)

checkbox :: Component Boolean
checkbox =
  div (classes "mdc-form-field") mempty mempty -- (\node _ -> mdcWith material.formField."MDCFormField" node mempty)
    (
    div (classes "mdc-checkbox") mempty (\node _ -> mdcWith material.checkbox."MDCCheckbox" node mempty)
      (
      Web.checkbox (classes "mdc-checkbox__native-control")
      ^
      div (classes "mdc-checkbox__background") mempty mempty
        (
        element "svg" (classes "mdc-checkbox__checkmark" <> attr "viewBox" "0 0 24 24") mempty mempty
          (
          element "path" (classes "mdc-checkbox__checkmark-path" <> attr "fill" "none" <> attr "d" "M1.73,12.91 8.1,19.28 22.79,4.59") mempty mempty pzero
          )
        ^
        div (classes "mdc-checkbox__mixedmark") mempty mempty pzero
        )
      ^
      div (classes "mdc-checkbox__ripple") mempty mempty pzero
      )
    )

radioButton :: forall a. Component (Maybe a)
radioButton = div (classes "mdc-form-field") mempty mempty
  (
    (div (classes "mdc-radio") mempty (\node _ -> mdcWith material.radio."MDCRadio" node mempty) $
      (radio (classes "mdc-radio__native-control" <> attr "id" "radio-1" ))
      <^
      (div (classes "mdc-radio__background") mempty mempty $
        div (classes "mdc-radio__outer-circle") mempty mempty pzero
        ^
        div (classes "mdc-radio__inner-circle") mempty mempty pzero
      )
      ^
      (div (classes "mdc-radio__ripple") mempty mempty pzero)
    )
    -- <>
    -- (Web.element' "label'" ("for" := "radio-1") (\_ node -> (liftEffect $ mdcWith material.formField."MDCFormField" node mempty) *> pure never) $ text # static "Radio 1")
  )

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


mdcWith :: ComponentClass -> Node -> (WebUI -> Node -> Effect Unit) -> Effect Unit
mdcWith classes node init = do
  component <- new classes node
  pure unit
  -- Tuple _ cleanup <- (map fst <<< runCleanupT) $ init component node
  -- pushDelayed cleanups cleanup
  where
    new :: ComponentClass -> Node -> Effect WebUI
    new cls node = liftEffect $ runEffectFn2 _new cls node

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

