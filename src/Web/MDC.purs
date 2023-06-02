module Web.MDC
  ( button
  , filledText
  , checkbox
  , radioButton
  ) where

import Prelude hiding (zero)

import Web (ComponentWrapper, inside, onClick, staticText, textInput)
import Web as Web
import Data.Plus (plus, zero)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Browser (Node, (:=))

button :: forall f a. Applicative f => ComponentWrapper f a -> ComponentWrapper f a
button wrapped = 
  inside "button" (const $ "class" := "mdc-button mdc-button--raised foo-button") ((\_ node _ -> mdcWith material.ripple."MDCRipple" node mempty) <> onClick) $
    (inside "div" (const $ "class" := "mdc-button__ripple") mempty zero)
    `plus`
    (inside "span" (const $ "class" := "mdc-button__label") mempty wrapped)

filledText :: forall f. Applicative f => String -> ComponentWrapper f String
filledText hintText =
  inside "label" (const $ "class" := "mdc-text-field mdc-text-field--filled") (\_ node _ -> mdcWith material.textField."MDCTextField" node mempty) $
    (inside "span" (const $ "class" := "mdc-text-field__ripple") mempty zero)
    `plus`
    (inside "span" (const $ "class" := "mdc-floating-label" <> "id" := "my-label-id") mempty (staticText hintText))
    `plus`
    (textInput (const $ "class" := "mdc-text-field__input" <> "type" := "text" <> "aria-labelledby" := "my-label-id"))
    `plus`
    (inside "span" (const $ "class" := "mdc-line-ripple") mempty zero)


checkbox :: forall f. Applicative f => ComponentWrapper f Boolean
checkbox =
  inside "div" (const $ "class" := "mdc-touch-target-wrapper") mempty $
    inside "div" (const $ "class" := "mdc-checkbox mdc-checkbox--touch") (\_ node _ -> mdcWith material.checkbox."MDCCheckbox" node mempty) $
      (Web.checkbox (const $ "class" := "mdc-checkbox__native-control"))
      `plus`
      (inside "div" (const $ "class":= "mdc-checkbox__background") mempty $
        inside "svg" (const $ "class" := "mdc-checkbox__checkmark" <> "viewBox" := "0 0 24 24") mempty $
          (inside "path" (const $ "class" := "mdc-checkbox__checkmark-path" <> "fill" := "none" <> "d" := "M1.73,12.91 8.1,19.28 22.79,4.59") mempty zero)
          `plus`
          (inside "div" (const $ "class" := "mdc-checkbox__mixedmark") mempty zero)
      )
      `plus`
      (inside "div" (const $ "class" := "mdc-checkbox__ripple") mempty zero)

radioButton :: forall f. Applicative f => ComponentWrapper f Boolean
radioButton = inside "div" (const $ "class" := "mdc-form-field") mempty
  (
    (inside "div" (const $ "class" := "mdc-radio") (\_ node _ -> mdcWith material.radio."MDCRadio" node mempty) $
      (Web.radio (const $ "class" := "mdc-radio__native-control" <> "id" := "radio-1"))
      `plus`
      (inside "div" (const $ "class" := "mdc-radio__background") mempty $
        inside "div" (const $ "class" := "mdc-radio__outer-circle") mempty zero
        `plus`
        inside "div" (const $ "class" := "mdc-radio__inner-circle") mempty zero
      )
      `plus`
      (inside "div" (const $ "class" := "mdc-radio__ripple") mempty zero)
    )
    -- <>
    -- (inside "label" (const $ "for" := "radio-1") (\_ node -> (liftEffect $ mdcWith material.formField."MDCFormField" node mempty) *> pure never) $ text # static "Radio 1")
  )

foreign import data ComponentClass :: Type
foreign import data Component :: Type

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

foreign import _new :: EffectFn2 ComponentClass Node Component

mdcWith :: ComponentClass -> Node -> (Component -> Node -> Effect Unit) -> Effect Unit
mdcWith class_ node init = do
  component <- new class_ node
  pure unit
  -- Tuple _ cleanup <- (map fst <<< runCleanupT) $ init component node
  -- pushDelayed cleanups cleanup
  where
    new :: ComponentClass -> Node -> Effect Component
    new cls node = liftEffect $ runEffectFn2 _new cls node
