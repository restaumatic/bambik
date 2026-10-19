module Checkout.StepperMDC2 (stepper, step) where

import Prelude (Unit, bind, discard, identity, negate, pure, show, unit, unless, ($), (&&), (*>), (+), (-), (<), (<#>), (<>), (>=), (>>=), (>>>))

import Control.Monad.State (gets)
import Data.Array (findIndex, index, length)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Class (liftEffect)
import Prim.Row (class Cons)
import PUI (static, PUI, fromHandles, handles, optionWiring)
import PUI.Web (Node, Web, addClass, appendChild, clazz, createTextNode, grouping, member, unpickAll, element, ensureStyle, chrome, el, cl, staticText, fieldLeaf, getValue, hasClass, invokeMethod, listenNode, removeAttribute, removeClass, setAttribute, setTextNodeValue, staticHTML, (:=))
import PUI.Web.HTML (div, span)
import Type.Proxy (Proxy(..))

-- | A wizard's step selector: Back · one dot per step · Next, the current
-- | step named beneath the dots. Its options are `step` leaves, merged
-- | `VariantToVariant.do` in step order; Back and Next pick the dot beside
-- | the current one, so each step emits its own case.
stepper :: forall @l w b r. IsSymbol l => Cons l (Variant w) b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
stepper steps = fieldLeaf @l $ "aria-label" := reflectSymbol (Proxy @l) $ stepperGroup steps

-- | One step of a `stepper`: a dot, current while the field holds case `c`.
step :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
step = fromHandles do
  chrome (static (el "data" >>> cl "mdc2-stepper__dot" >>> "aria-hidden" := "true" >>> "value" := reflectSymbol (Proxy @c)))
  dot <- lastBuilt
  steps <- member
    { node: dot
    , picked: hasClass "mdc2-stepper__dot--current" dot
    , unpick: removeClass dot "mdc2-stepper__dot--current"
    }
  let
    current = do
      unpickAll steps
      addClass dot "mdc2-stepper__dot--current"
  liftEffect $ optionWiring @c
    { show: current
    , reports: \k -> listenNode dot "click" do
        already <- hasClass "mdc2-stepper__dot--current" dot
        unless already (current *> k)
    , picked: pure true
    }

stepperGroup :: forall a. PUI Web a a -> PUI Web a a
stepperGroup steps =
  div >>> "class" := "mdc2-stepper" >>> "role" := "group" $ fromHandles do
    liftEffect $ ensureStyle "mdc2-stepper" stepperCss
    back <- navButton "chevron_left" "Back"
    { dots, inner, caption } <- element "div" do
      { result: inner, members: dots } <- element "div" (grouping (handles steps))
      clazz "mdc2-stepper__dots"
      caption <- liftEffect $ createTextNode ""
      _ <- handles (staticHTML "<span class=\"mdc2-stepper__caption\" aria-live=\"polite\"></span>")
      lastBuilt >>= \span -> liftEffect $ appendChild caption span
      pure { dots, inner, caption }
    clazz "mdc2-stepper__progress"
    next <- navButton "chevron_right" "Next"
    let
      -- the dots in order, with the current one's index
      scan = do
        ds <- dots
        picks <- traverse (\{ picked } -> picked) ds
        current <- case findIndex identity picks of
          Just i -> traverse (\d -> getValue d.node <#> \label -> { index: i, label }) (index ds i)
          Nothing -> pure Nothing
        pure { count: length ds, current }
      refresh = do
        { count, current } <- scan
        setTextNodeValue caption case current of
          Just { index, label } -> "Step " <> show (index + 1) <> " of " <> show count <> " · " <> label
          Nothing -> ""
        disabledIf back case current of
          Just { index } -> index < 1
          Nothing -> true
        disabledIf next case current of
          Just { index } -> index >= count - 1
          Nothing -> true
      moveBy delta = do
        { count, current } <- scan
        case current of
          Just { index: i } | i + delta >= 0 && i + delta < count -> do
            ds <- dots
            for_ (index ds (i + delta)) \d -> invokeMethod "click" d.node
            refresh
          _ -> pure unit
    liftEffect do
      listenNode back "click" (moveBy (-1))
      listenNode next "click" (moveBy 1)
    pure
      { toUser: \a -> inner.toUser a *> refresh
      , fromUser: inner.fromUser
      }

navButton :: String -> String -> Web Node
navButton icon caption = do
  chrome $ el "button" >>> cl "mdc-button" $ static (span >>> cl "mdc-button__ripple") >>> (el "i" >>> cl "material-icons" >>> cl "mdc-button__icon" >>> "aria-hidden" := "true" $ staticText icon) >>> (span >>> cl "mdc-button__label" $ staticText caption)
  lastBuilt

lastBuilt :: Web Node
lastBuilt = gets \{ sibling } -> sibling

disabledIf :: Node -> Boolean -> Effect Unit
disabledIf node on = if on then setAttribute node "disabled" "" else removeAttribute node "disabled"

stepperCss :: String
stepperCss = """
.mdc2-stepper { display: flex; align-items: center; justify-content: space-between; gap: 8px; padding: 8px 0; }
.mdc2-stepper__progress { display: flex; flex-direction: column; align-items: center; gap: 6px; }
.mdc2-stepper__dots { display: flex; gap: 8px; }
.mdc2-stepper__dot { width: 8px; height: 8px; border-radius: 50%; background: rgba(0, 0, 0, 0.26); }
.mdc2-stepper__dot--current { background: var(--mdc-theme-primary, #6200ee); }
.mdc2-stepper__caption { font: 400 14px/20px Roboto, sans-serif; color: rgba(0, 0, 0, 0.6); }
"""
