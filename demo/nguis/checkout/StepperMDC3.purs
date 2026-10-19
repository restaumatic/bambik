module Checkout.StepperMDC3 (stepper, step) where

import Prelude (Unit, bind, discard, negate, pure, show, unit, unless, when, ($), (&&), (*>), (+), (-), (<), (<$>), (<*>), (<>), (==), (>=), (>>=), (>>>))

import Control.Monad.State (gets)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import PUI (PUI, fromHandles, handles, optionWiring)
import PUI.Web (Node, Web, addClass, appendChild, clazz, createTextNode, eachMatching, element, ensureStyle, escaped, getValue, hasClass, invokeMethod, listenNode, removeAttribute, removeClass, setAttribute, setTextNodeValue, staticHTML, (:=))
import PUI.Web.HTML (div)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Type.Proxy (Proxy(..))

-- | A wizard's step selector: Back · one dot per step · Next, the current
-- | step named beneath the dots. Its options are `step` leaves, merged
-- | `VariantToVariant.do` in step order; Back and Next pick the dot beside
-- | the current one, so each step emits its own case.
stepper :: forall @l w b r. IsSymbol l => Cons l (Variant w) b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
stepper steps = focusField @l $ "name" := reflectSymbol (Proxy @l) >>> "aria-label" := reflectSymbol (Proxy @l) $ stepperGroup steps

-- | One step of a `stepper`: a dot, current while the field holds case `c`.
step :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
step = fromHandles do
  dots <- gets _.parent
  _ <- handles (staticHTML ("<data class=\"md3-stepper__dot\" aria-hidden=\"true\" value=\"" <> escaped (reflectSymbol (Proxy @c)) <> "\"></data>"))
  dot <- lastBuilt
  let
    current = do
      eachMatching ".md3-stepper__dot" dots \d -> removeClass d "md3-stepper__dot--current"
      addClass dot "md3-stepper__dot--current"
  liftEffect $ optionWiring @c
    { show: current
    , reports: \k -> listenNode dot "click" do
        already <- hasClass "md3-stepper__dot--current" dot
        unless already (current *> k)
    , picked: pure true
    }

stepperGroup :: forall a. PUI Web a a -> PUI Web a a
stepperGroup steps =
  div >>> "class" := "md3-stepper" >>> "role" := "group" $ fromHandles do
    liftEffect $ ensureStyle "md3-stepper" stepperCss
    back <- navButton "chevron_left" "Back"
    { dots, inner, caption } <- element "div" do
      { dots, inner } <- element "div" do
        dots <- gets _.parent
        inner <- handles steps
        pure { dots, inner }
      clazz "md3-stepper__dots"
      caption <- liftEffect $ createTextNode ""
      _ <- handles (staticHTML "<span class=\"md3-stepper__caption\" aria-live=\"polite\"></span>")
      lastBuilt >>= \span -> liftEffect $ appendChild caption span
      pure { dots, inner, caption }
    clazz "md3-stepper__progress"
    next <- navButton "chevron_right" "Next"
    let
      -- the dots in order, with the current one's index
      scan = do
        countRef <- Ref.new 0
        currentRef <- Ref.new Nothing
        eachMatching ".md3-stepper__dot" dots \d -> do
          i <- Ref.read countRef
          isCurrent <- hasClass "md3-stepper__dot--current" d
          when isCurrent do
            label <- getValue d
            Ref.write (Just { index: i, label }) currentRef
          Ref.write (i + 1) countRef
        { count: _, current: _ } <$> Ref.read countRef <*> Ref.read currentRef
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
          Just { index } | index + delta >= 0 && index + delta < count -> do
            targetRef <- Ref.new 0
            eachMatching ".md3-stepper__dot" dots \d -> do
              i <- Ref.read targetRef
              when (i == index + delta) $ invokeMethod "click" d
              Ref.write (i + 1) targetRef
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
  _ <- handles (staticHTML ("<md-text-button><md-icon slot=\"icon\">" <> escaped icon <> "</md-icon>" <> escaped caption <> "</md-text-button>"))
  lastBuilt

lastBuilt :: Web Node
lastBuilt = gets \{ sibling } -> sibling

disabledIf :: Node -> Boolean -> Effect Unit
disabledIf node on = if on then setAttribute node "disabled" "" else removeAttribute node "disabled"

stepperCss :: String
stepperCss = """
.md3-stepper { display: flex; align-items: center; justify-content: space-between; gap: 8px; padding: 8px 0; }
.md3-stepper__progress { display: flex; flex-direction: column; align-items: center; gap: 6px; }
.md3-stepper__dots { display: flex; gap: 8px; }
.md3-stepper__dot { width: 8px; height: 8px; border-radius: 50%; background: var(--md-sys-color-outline-variant, #cac4d0); }
.md3-stepper__dot--current { background: var(--md-sys-color-primary, #6750a4); }
.md3-stepper__caption { font: 500 14px/20px Roboto, sans-serif; color: var(--md-sys-color-on-surface-variant, #49454f); }
"""
