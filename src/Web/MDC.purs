module Web.MDC
  ( button
  , checkbox
  , filledText
  , list
  , radioButton
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (newSlot, replaceSlot)
import Data.Array (updateAt)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Plus (plus, pzero)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (modify, new, write)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Specular.Dom.Browser (Node, (:=))
import Specular.Dom.Builder.Class (elAttr, text)
import Web as Web
import Web.HTML as HTML

button :: forall a. Web.WebComponent a -> Web.WebComponent a
button wrapped =
  Web.inside' "button" (const $ "class" := "mdc-button mdc-button--raised foo-button") ((\node _ -> mdcWith material.ripple."MDCRipple" node mempty) <> HTML.onClick) $
    (Web.inside' "div" (const $ "class" := "mdc-button__ripple") mempty pzero)
    `plus`
    (Web.inside' "span" (const $ "class" := "mdc-button__label") mempty wrapped)

filledText :: String -> Web.WebComponent String
filledText hintText =
  Web.inside' "label" (const $ "class" := "mdc-text-field mdc-text-field--filled") (\node _ -> mdcWith material.textField."MDCTextField" node mempty) $
    (Web.inside' "span" (const $ "class" := "mdc-text-field__ripple") mempty pzero)
    `plus`
    (Web.inside' "span" (const $ "class" := "mdc-floating-label" <> "id" := "my-label-id") mempty (HTML.staticText hintText))
    `plus`
    (HTML.textInput ("class" := "mdc-text-field__input" <> "type" := "text" <> "aria-labelledby" := "my-label-id"))
    `plus`
    (Web.inside' "span" (const $ "class" := "mdc-line-ripple") mempty pzero)


checkbox :: Web.WebComponent Boolean
checkbox =
  Web.inside' "div" (const $ "class" := "mdc-touch-target-wrapper") mempty $
    Web.inside' "div" (const $ "class" := "mdc-checkbox mdc-checkbox--touch") (\node _ -> mdcWith material.checkbox."MDCCheckbox" node mempty) $
      (HTML.checkbox ("class" := "mdc-checkbox__native-control"))
      `plus`
      (Web.inside' "div" (const $ "class":= "mdc-checkbox__background") mempty $
        Web.inside' "svg" (const $ "class" := "mdc-checkbox__checkmark" <> "viewBox" := "0 0 24 24") mempty $
          (Web.inside' "path" (const $ "class" := "mdc-checkbox__checkmark-path" <> "fill" := "none" <> "d" := "M1.73,12.91 8.1,19.28 22.79,4.59") mempty pzero)
          `plus`
          (Web.inside' "div" (const $ "class" := "mdc-checkbox__mixedmark") mempty pzero)
      )
      `plus`
      (Web.inside' "div" (const $ "class" := "mdc-checkbox__ripple") mempty pzero)

radioButton :: Web.WebComponent Boolean
radioButton = Web.inside' "div" (const $ "class" := "mdc-form-field") mempty
  (
    (Web.inside' "div" (const $ "class" := "mdc-radio") (\node _ -> mdcWith material.radio."MDCRadio" node mempty) $
      (HTML.radio (const $ "class" := "mdc-radio__native-control" <> "id" := "radio-1"))
      `plus`
      (Web.inside' "div" (const $ "class" := "mdc-radio__background") mempty $
        Web.inside' "div" (const $ "class" := "mdc-radio__outer-circle") mempty pzero
        `plus`
        Web.inside' "div" (const $ "class" := "mdc-radio__inner-circle") mempty pzero
      )
      `plus`
      (Web.inside' "div" (const $ "class" := "mdc-radio__ripple") mempty pzero)
    )
    -- <>
    -- (HTML.inside "label" (const $ "for" := "radio-1") (\_ node -> (liftEffect $ mdcWith material.formField."MDCFormField" node mempty) *> pure never) $ text # static "Radio 1")
  )

list :: forall a. Web.WebComponent a -> Web.WebComponent (Array a)
list c = wrap \callbackas -> do -- -> Builder Unit (UserInput a -> Effect Unit)
  slot <- newSlot
  asRef <- liftEffect $ new []
  pure $ \as -> replaceSlot slot do
    liftEffect $ write as asRef
    void $ elAttr "ul" ("class" := "mdc-list mdc-list--two-line") $
      forWithIndex_ as \i a -> elAttr "li" ("class" := "mdc-list-item") do
        void $ elAttr "span" ("class" := "mdc-list-item__ripple") (pure unit)
        void $ elAttr "span" ("class" := "mdc-list-item__text") do
          void $ elAttr "span" ("class" := "mdc-list-item__secondary-text") $ text $ "Item " <> show i
          void $ elAttr "span" ("class" := "mdc-list-item__primary-text") do
            update <- unwrap c \value -> do
              newas <- modify (\currentAs -> fromMaybe currentAs (updateAt i value currentAs)) asRef
              callbackas newas
            liftEffect $ update a

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
