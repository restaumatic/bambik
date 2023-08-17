module Web.MDC
  ( button
  , checkbox
  , filledText
  -- , list
  , radioButton
  )
  where

import Prelude hiding (zero)

import Data.Invariant.Transformers.Scoped (Scoped(..))
import Data.Profunctor.Plus (prozero, (<^), (^))
import Data.String (null)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Browser (AttrValue(..), Node, (:=))
import Web (Widget, Component, div', inside', label', radio, span', text)
import Web as Web

button :: forall a. Widget a a -> Widget a a
button wrapped =
  inside' "button" ("class" := ClassNames "mdc-button mdc-button--raised foo-button") mempty ((\node _ -> mdcWith material.ripple."MDCRipple" node mempty) <> Web.onClick) $
    (div' ("class" := ClassNames "mdc-button__ripple") mempty mempty prozero)
    ^
    (span' ("class" := ClassNames "mdc-button__label") mempty mempty wrapped)

filledText :: String -> Component String String
filledText hintText =
  label' ("class" := ClassNames "mdc-text-field mdc-text-field--filled mdc-text-field--label-floating") mempty (\node _ -> mdcWith material.textField."MDCTextField" node mempty) $
    (span' ("class" := ClassNames "mdc-text-field__ripple") mempty mempty prozero)
    ^
    (span' ("class" := ClassNames "mdc-floating-label" <> "id" := AttrValue "my-label-id") (\(Scoped _ value) -> if not (null value) then "class" := ClassNames "mdc-floating-label--float-above" else mempty)) mempty (text hintText)
    ^
    (Web.textInput ("class" := ClassNames "mdc-text-field__input" <> "type" := AttrValue "text" <> "aria-labelledby" := AttrValue "my-label-id"))
    ^
    (span' ("class" := ClassNames "mdc-line-ripple") mempty mempty prozero)

checkbox :: Component Boolean Boolean
checkbox =
  div' ("class" := ClassNames "mdc-form-field") mempty mempty -- (\node _ -> mdcWith material.formField."MDCFormField" node mempty)
    (
    div' ("class" := ClassNames "mdc-checkbox") mempty (\node _ -> mdcWith material.checkbox."MDCCheckbox" node mempty)
      (
      Web.checkbox ("class" := ClassNames "mdc-checkbox__native-control")
      ^
      div' ("class":= ClassNames "mdc-checkbox__background") mempty mempty
        (
        inside' "svg" ("class" := ClassNames "mdc-checkbox__checkmark" <> "viewBox" := AttrValue "0 0 24 24") mempty mempty
          (
          inside' "path" ("class" := ClassNames "mdc-checkbox__checkmark-path" <> "fill" := AttrValue "none" <> "d" := AttrValue "M1.73,12.91 8.1,19.28 22.79,4.59") mempty mempty prozero
          )
        ^
        div' ("class" := ClassNames "mdc-checkbox__mixedmark") mempty mempty prozero
        )
      ^
      div' ("class" := ClassNames "mdc-checkbox__ripple") mempty mempty prozero
      )
    )

radioButton :: Component Boolean Unit -- TODO
radioButton = div' ("class" := ClassNames "mdc-form-field") mempty mempty
  (
    (div' ("class" := ClassNames "mdc-radio") mempty (\node _ -> mdcWith material.radio."MDCRadio" node mempty) $
      (radio ("class" := ClassNames "mdc-radio__native-control" <> "id" := AttrValue "radio-1" ))
      <^
      (div' ("class" := ClassNames "mdc-radio__background") mempty mempty $
        div' ("class" := ClassNames "mdc-radio__outer-circle") mempty mempty prozero
        ^
        div' ("class" := ClassNames "mdc-radio__inner-circle") mempty mempty prozero
      )
      ^
      (div' ("class" := ClassNames "mdc-radio__ripple") mempty mempty prozero)
    )
    -- <>
    -- (Web.inside "label" ("for" := "radio-1") (\_ node -> (liftEffect $ mdcWith material.formField."MDCFormField" node mempty) *> pure never) $ text # static "Radio 1")
  )

-- list :: forall a. Component a -> Component (Array a)
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


mdcWith :: ComponentClass -> Node -> (WebUI -> Node -> Effect Unit) -> Effect Unit
mdcWith class_ node init = do
  component <- new class_ node
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

