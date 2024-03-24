-- MDC implemented with Widgets, dogfooding intentional.
module MDC
  ( body1
  , body2
  , button
  , caption
  , card
  , checkbox
  , confirmationDialog
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
  , indeterminateLinearProgress
  , overline
  , radioButton
  , snackbar
  , subtitle1
  , subtitle2
  )
  where

import Control.Monad.State (gets)
import Data.Maybe (Maybe, isNothing, maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (class Monad, Unit, Void, bind, const, discard, mempty, pure, unit, (#), ($), (<<<), (>>=), (>>>))
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as T
import Web (Node, Web, aside, attr, checkboxInput, cl, clickable, div, dynClass, h1, h2, h3, h4, h5, h6, html, label, p, span, text, input, uniqueId)
import Web (button, radioButton) as Web
import Widget (Changed(..), Widget, WidgetOptics, WidgetOptics', Foo, action', devoid, effAdapter, effBracket)

-- Primitive widgets

containedButton :: forall a b. { label :: WidgetOptics String Void a b } -> Widget Web a a
containedButton { label } =
  Web.button >>> cl "mdc-button" >>> cl "mdc-button--raised" >>> cl "initAside-button" >>> br >>> clickable $ S.do
    div >>> cl "mdc-button__ripple" $ (devoid :: Widget Web a a) -- TODO
    span >>> cl "mdc-button__label" $
      (label text) >>> devoid
  where
    br = bracket (gets _.sibling >>= (liftEffect <<< newComponent material.ripple."MDCRipple")) (const $ pure unit) (const $ pure unit)

-- TODO support input types: email, text, password, number, search, tel, url
filledTextField :: forall a b. { floatingLabel :: WidgetOptics String Void a b } -> WidgetOptics' String a -> Widget Web a a
filledTextField { floatingLabel } value =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--label-floating" >>> dynClass "mdc-text-field--disabled" (maybe true $ case _ of
    Altered _ -> false
    Removed -> true) >>> bracket (gets _.sibling >>= (liftEffect <<< newComponent material.textField."MDCTextField")) (const $ pure unit) (const $ pure unit) $ S.do
    span >>> cl "mdc-text-field__ripple" $ devoid
    S.do
      (span >>> cl "mdc-floating-label" >>> attr "id" id >>> dynClass "mdc-floating-label--float-above" (maybe false (case _ of
        Removed -> false
        Altered _ -> true)) $
          floatingLabel text) >>> devoid
      input "text" # value # cl "mdc-text-field__input" # attr "aria-labelledby" id
    span >>> cl "mdc-line-ripple" $ devoid
    where
      id = unsafePerformEffect uniqueId


checkbox :: forall a b s. { labelContent :: WidgetOptics String Void s b, default :: a } -> WidgetOptics' (Maybe a) s -> Widget Web s s
checkbox { labelContent, default } checked =
  div >>> cl "mdc-form-field" >>> bracket (gets _.sibling >>= (liftEffect <<< newComponent material.formField."MDCFormField")) (const $ pure unit) (const $ pure unit) $ S.do
    div >>> cl "mdc-checkbox" >>> bracket (gets _.sibling >>= (liftEffect <<< newComponent material.checkbox."MDCCheckbox")) (const $ pure unit) (const $ pure unit) $ S.do
      checkboxInput default # checked # cl "mdc-checkbox__native-control" # attr "id" id
      div >>> cl "mdc-checkbox__background" $ S.do
        html """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div >>> cl "mdc-checkbox__mixedmark" $ devoid
      div >>> cl "mdc-checkbox__ripple" $ devoid
    label (labelContent text >>> devoid) # attr "for" id
    where
      id = unsafePerformEffect uniqueId

-- TODO add html grouping?
radioButton :: forall a b s. { labelContent :: WidgetOptics String Void s b, default :: a } -> WidgetOptics' a s -> Widget Web s s
radioButton { labelContent, default } value =
  div >>> cl "mdc-form-field" >>> bracket (gets _.sibling >>= (liftEffect <<< newComponent material.formField."MDCFormField")) (const $ pure unit) (const $ pure unit) $ S.do
    div >>> cl "mdc-radio" >>> dynClass "mdc-radio--disabled" isNothing >>> bracket (gets _.sibling >>= (liftEffect <<< newComponent material.radio."MDCRadio")) (const $ pure unit) (const $ pure unit) $ S.do
      Web.radioButton default # cl "mdc-radio__native-control" # attr "id" uid # value
      div >>> cl "mdc-radio__background" $ S.do
        div >>> cl "mdc-radio__outer-circle" $ devoid
        div >>> cl "mdc-radio__inner-circle" $ devoid
      div >>> cl "mdc-radio__ripple" $ devoid
    label (labelContent text >>> devoid) # attr "for" uid
  where
    uid = unsafePerformEffect uniqueId

headline1 :: Foo Web
headline1 w = h1 w # cl "mdc-typography--headline1"

headline2 :: Foo Web
headline2 w = h2 w # cl "mdc-typography--headline2"

headline3 :: Foo Web
headline3 w = h3 w # cl "mdc-typography--headline3"

headline4 :: Foo Web
headline4 w = h4 w # cl "mdc-typography--headline4"

headline5 :: Foo Web
headline5 w = h5 w # cl "mdc-typography--headline5"

headline6 :: Foo Web
headline6 w = h6 w # cl "mdc-typography--headline6"

subtitle1 :: Foo Web
subtitle1 w = p w # cl "mdc-typography--subtitle1"

subtitle2 :: Foo Web
subtitle2 w = p w # cl "mdc-typography--subtitle2"

button :: Foo Web
button w = span w # cl "mdc-typography--button"

caption :: Foo Web
caption w = span w # cl "mdc-typography--caption"

overline :: Foo Web
overline w = span w # cl "mdc-typography--overline"

body1 :: Foo Web
body1 w = p w # cl"mdc-typography--body1"

body2 :: Foo Web
body2 w = p w # cl"mdc-typography--body2"

elevation1 :: Foo Web
elevation1 w = div w # cl "mdc-elevation--z1"

elevation10 :: Foo Web
elevation10 w = div w # cl "mdc-elevation--z10" # attr "style" "padding: 25px"

elevation20 :: Foo Web
elevation20 w = div w # cl "mdc-elevation--z20" # attr "style" "padding: 25px"

card :: forall a b . Foo Web
card w = div w # cl "mdc-card" # attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;"

-- TODO card with primary and other actions
-- card :: forall a b. { title :: Widget Web a Void } -> Foo Web
-- card { title } w = div >>> cl "mdc-card" >>> attr "style" "padding: 10px; margin: 15px 0 15px 0; text-align: justify;" $ S.do
--   div >>> cl "mdc-card__primary-action" $ S.do
--     div >>> cl "mdc-card__media" >>> cl "mdc-card__media--square" $ div >>> cl "mdc-card__media-content" $ (title >>> devoid)
--     w
--     div >>> cl "mdc-card__ripple" $ devoid
--   -- TODO  card actions

dialog :: forall a b. { title :: WidgetOptics String Void a b } -> Widget Web a a -> Widget Web a a
dialog { title } content =
  aside >>> cl "mdc-dialog" >>> bracket initializeMdcDialog openMdcComponent closeMdcComponent $ S.do
    div >>> cl "mdc-dialog__container" $ S.do
      div >>> cl "mdc-dialog__surface" >>> attr "role" "alertdialog" >>> attr "aria-modal" "true" >>> attr "aria-labelledby" "my-dialog-title" >>> attr "aria-describedby" "my-dialog-content" $ S.do
        h2 >>> cl "mdc-dialog__title" >>> attr "id" "my-dialog-title" $
          title text >>> devoid
        div >>> cl "mdc-dialog__content" >>> attr "id" "my-dialog-content" $
          content
    div >>> cl "mdc-dialog__scrim" $ devoid
    where
      initializeMdcDialog = gets _.sibling >>= (liftEffect <<< newComponent material.dialog."MDCDialog")
      openMdcComponent comp = liftEffect $ open comp
      closeMdcComponent comp = liftEffect $ close comp

confirmationDialog :: forall a b c d. { title :: WidgetOptics String Void a b, dismiss :: WidgetOptics String Void a c, confirm :: WidgetOptics String Void a d } -> Widget Web a a -> Widget Web a a
confirmationDialog { title, dismiss, confirm } content =
  div >>> cl "mdc-dialog" >>> bracket initializeMdcDialog openMdcComponent closeMdcComponent $ S.do
    div >>> cl "mdc-dialog__container" $
      div >>> cl "mdc-dialog__surface" >>> attr "role" "altertdialog" >>> attr "aria-modal" "true" >>> attr "aria-labelledby" "my-dialog-title" >>> attr "aria-describedby" "my-dialog-content" $ S.do
        T.do
          S.do
            h2 >>> cl "mdc-dialog__title" >>> attr "id" id $ T.do
              title text -- TODO text # title doesn't compile, why?
              devoid
            div >>> cl "mdc-dialog__content" >>> attr "id" id' $
              content
          div >>> cl "mdc-dialog__actions" $ S.do
            Web.button >>> attr "type" "button" >>> cl "mdc-button" >>> cl "mdc-dialog__button" >>> attr "data-mdc-dialog-action" "close" $ S.do
              div >>> cl "mdc-button__ripple" $ devoid
              span >>> cl "mdc-button__label" $ T.do
                dismiss text
                devoid
            Web.button >>> attr "type" "button" >>> cl "mdc-button" >>> cl "mdc-dialog__button" >>> clickable $ S.do
              div >>> cl "mdc-button__ripple" $ (devoid :: Widget Web a a) -- TODO
              span >>>  cl "mdc-button__label" $ T.do
                confirm text
                devoid
    div >>> cl "mdc-dialog__scrim" $ devoid
    where
      id = unsafePerformEffect uniqueId
      id' = unsafePerformEffect uniqueId
      initializeMdcDialog = gets _.sibling >>= (liftEffect <<< newComponent material.dialog."MDCDialog")
      openMdcComponent comp = liftEffect $ open comp
      closeMdcComponent comp = liftEffect $ close comp

snackbar :: forall a b. { label :: WidgetOptics String Void a b } -> Widget Web a a
snackbar { label } =
  aside >>> cl "mdc-snackbar" >>> effBracket (do
    comp <- gets _.sibling >>= (liftEffect <<< newComponent material.snackbar."MDCSnackbar")
    pure { beforeInput: case _ of
      Removed -> close comp
      Altered _ -> mempty
    , afterInput: case _ of
      Removed -> mempty
      Altered _ -> open comp
    , beforeOutput: mempty
    , afterOutput: mempty
    }) >>> (action' \a a2eff o2eff -> liftEffect do
    a2eff a
    o2eff a) $
    div >>> attr "role" "status" >>> attr "aria-relevant" "additions" >>> cl "mdc-snackbar__surface" $
      div $
        label text # cl "mdc-snackbar__label" # attr "aria-atomic" "false"

indeterminateLinearProgress :: forall a. Widget Web Boolean a
indeterminateLinearProgress =
  div >>> attr "role" "indeterminateLinearProgress" >>> cl "mdc-linear-progress" >>> attr "aria-label" "TODO: Example Progress Bar" >>> attr "aria-valuemin" "0" >>> attr "aria-valuemax" "1" >>> attr "aria-valuenow" "0" >>> effAdapter adapter $ T.do
    div >>> cl "mdc-linear-progress__buffer" $ T.do
      div >>> cl "mdc-linear-progress__buffer-bar" $ devoid
      div >>> cl "mdc-linear-progress__buffer-dots" $ devoid
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ devoid
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ devoid
    where
      adapter = do
        comp <- gets _.sibling >>= (liftEffect <<< newComponent material.linearProgress."MDCLinearProgress")
        liftEffect $ close comp
        liftEffect $ setDeterminate comp false
        pure
          { pre: case _ of
            true -> open comp
            false -> close comp
          , post: pure }

-- Private

bracket :: forall a b c m. Monad m => m c -> (c -> Effect Unit) -> (c -> Effect Unit) -> Widget m a b -> Widget m a b
bracket afterInit afterInward beforeOutward = effBracket do
  ctx <- afterInit
  pure
    { beforeInput: mempty
    , afterInput: case _ of
      Removed -> pure unit -- TODO really?
      Altered _ -> afterInward ctx
    , beforeOutput: const $ beforeOutward ctx
    , afterOutput: mempty
    }

foreign import data Component :: Type
foreign import data ComponentClass :: Type
foreign import open :: Component -> Effect Unit
foreign import close :: Component -> Effect Unit
foreign import newComponent :: ComponentClass -> Node -> Effect Component
foreign import setDeterminate :: Component -> Boolean -> Effect Unit
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
     , linearProgress :: { "MDCLinearProgress" :: ComponentClass }
     }

