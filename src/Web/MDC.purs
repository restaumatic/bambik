module Web.MDC
  ( button
  , checkbox
  , filledTextField
  -- , list
  , radioButton
  )
  where

import Prelude hiding (zero)

import Data.Plus (prozero, (^^))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Browser (Node, (:=))
import Web (WebComponent, WebComponentWrapper, div', inside', label', radio, span', text)
import Web as Web

button :: forall a. WebComponent a a -> WebComponent a a
button wrapped =
  inside' "button" (const $ "class" := "mdc-button mdc-button--raised foo-button") ((\node _ -> mdcWith material.ripple."MDCRipple" node mempty) <> Web.onClick) $
    (div' (const $ "class" := "mdc-button__ripple") mempty prozero)
    ^^
    (span' (const $ "class" := "mdc-button__label") mempty wrapped)

filledTextField :: String -> WebComponentWrapper String String
filledTextField hintText =
  label' (const $ "class" := "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") (\node _ -> mdcWith material.textField."MDCTextField" node mempty) $
    (span' (const $ "class" := "mdc-text-field__ripple") mempty prozero)
    ^^
    (span' (const $ "class" := "mdc-floating-label mdc-floating-label--float-above" <> "id" := "my-label-id") mempty (text hintText))
    ^^
    (Web.textInput ("class" := "mdc-text-field__input" <> "type" := "text" <> "aria-labelledby" := "my-label-id"))
    ^^
    (span' (const $ "class" := "mdc-line-ripple") mempty prozero)

checkbox :: WebComponentWrapper Boolean Boolean
checkbox =
  div' (const $ "class" := "mdc-form-field") mempty -- (\node _ -> mdcWith material.formField."MDCFormField" node mempty)
    (
    div' (const $ "class" := "mdc-checkbox") (\node _ -> mdcWith material.checkbox."MDCCheckbox" node mempty)
      (
      Web.checkbox ("class" := "mdc-checkbox__native-control" <> "type" := "checkbox")
      ^^
      div' (const $ "class":= "mdc-checkbox__background") mempty
        (
        inside' "svg" (const $ "class" := "mdc-checkbox__checkmark" <> "viewBox" := "0 0 24 24") mempty
          (
          inside' "path" (const $ "class" := "mdc-checkbox__checkmark-path" <> "fill" := "none" <> "d" := "M1.73,12.91 8.1,19.28 22.79,4.59") mempty prozero
          )
        ^^
        div' (const $ "class" := "mdc-checkbox__mixedmark") mempty prozero
        )
      ^^
      div' (const $ "class" := "mdc-checkbox__ripple") mempty prozero
      )
    )

radioButton :: WebComponent Boolean Boolean -- TODO
radioButton = div' (const $ "class" := "mdc-form-field") mempty
  (
    (div' (const $ "class" := "mdc-radio") (\node _ -> mdcWith material.radio."MDCRadio" node mempty) $
      (radio (const $ "class" := "mdc-radio__native-control" <> "id" := "radio-1"))
      ^^
      (div' (const $ "class" := "mdc-radio__background") mempty $
        div' (const $ "class" := "mdc-radio__outer-circle") mempty prozero
        ^^
        div' (const $ "class" := "mdc-radio__inner-circle") mempty prozero
      )
      ^^
      (div' (const $ "class" := "mdc-radio__ripple") mempty prozero)
    )
    -- <>
    -- (Web.inside "label" (const $ "for" := "radio-1") (\_ node -> (liftEffect $ mdcWith material.formField."MDCFormField" node mempty) *> pure never) $ text # static "Radio 1")
  )

-- list :: forall a. WebComponentWrapper a -> WebComponentWrapper (Array a)
-- list c = wrapWebComponent \callbackas -> do -- -> Builder Unit (UserInput a -> Effect Unit)
--   slot <- newSlot
--   asRef <- liftEffect $ new []
--   pure $ \as -> replaceSlot slot do
--     liftEffect $ write as asRef
--     void $ elAttr "ul" ("class" := "mdc-list mdc-list--two-line") $
--       forWithIndex_ as \i a -> elAttr "li" ("class" := "mdc-list-item") do
--         void $ elAttr "span" ("class" := "mdc-list-item__ripple") (pure unit)
--         void $ elAttr "span" ("class" := "mdc-list-item__text") do
--           void $ elAttr "span" ("class" := "mdc-list-item__secondary-text") $ text $ "Item " <> show i
--           void $ elAttr "span" ("class" := "mdc-list-item__primary-text") do
--             update <- unwrap c \value -> do
--               newas <- modify (\currentAs -> fromMaybe currentAs (updateAt i value currentAs)) asRef
--               callbackas newas
--             liftEffect $ update a

foreign import data ComponentClass :: Type
foreign import data WebUI :: Type

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

foreign import _new :: EffectFn2 ComponentClass Node WebUI

mdcWith :: ComponentClass -> Node -> (WebUI -> Node -> Effect Unit) -> Effect Unit
mdcWith class_ node init = do
  component <- new class_ node
  pure unit
  -- Tuple _ cleanup <- (map fst <<< runCleanupT) $ init component node
  -- pushDelayed cleanups cleanup
  where
    new :: ComponentClass -> Node -> Effect WebUI
    new cls node = liftEffect $ runEffectFn2 _new cls node
