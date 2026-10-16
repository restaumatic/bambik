module Checkout.StepperMDC2 (stepper) where

import Prelude (Unit, bind, discard, pure, show, when, ($), (&&), (*>), (+), (-), (<), (<>), (==), (>), (>=), (>>>), (>>=))

import Control.Monad.State (gets)
import Data.Array (length, (!!))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (for)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import PUI (PUI, fromHandles, handles)
import PUI.Web (Choices, choiceTags, tagged, Node, Web, addClass, appendChild, clazz, createTextNode, element, ensureStyle, escaped, listenNode, removeAttribute, removeClass, selectedAt, selectorWiring, setAttribute, setTextNodeValue, staticHTML, (:=))
import PUI.Web.HTML (div)
import Type.Proxy (Proxy(..))

stepper :: forall @l w b r. IsSymbol l => Cons l (Variant w) b r => Choices w -> PUI Web { | r } { | r }
stepper steps = selectedAt @l $ "name" := reflectSymbol (Proxy @l) >>> "aria-label" := reflectSymbol (Proxy @l) $ tagged $ stepperLeaf (choiceTags steps)

stepperLeaf :: Array String -> PUI Web (Maybe String) (Maybe String)
stepperLeaf steps =
  div >>> "class" := "mdc2-stepper" >>> "role" := "group" $ fromHandles do
    liftEffect $ ensureStyle "mdc2-stepper" stepperCss
    back <- navButton "chevron_left" "Back"
    { dots, caption } <- element "div" do
      dots <- element "div" (for steps \_ -> handles (staticHTML "<span class=\"mdc2-stepper__dot\" aria-hidden=\"true\"></span>") *> lastBuilt)
      clazz "mdc2-stepper__dots"
      caption <- liftEffect $ createTextNode ""
      _ <- handles (staticHTML "<span class=\"mdc2-stepper__caption\" aria-live=\"polite\"></span>")
      lastBuilt >>= \span -> liftEffect $ appendChild caption span
      pure { dots, caption }
    clazz "mdc2-stepper__progress"
    next <- navButton "chevron_right" "Next"
    idxRef <- liftEffect $ Ref.new Nothing
    let
      count = length steps
      render mi = do
        Ref.write mi idxRef
        forWithIndex_ dots \i dot -> classIf dot "mdc2-stepper__dot--current" (mi == Just i)
        setTextNodeValue caption $ maybe "" (\i -> "Step " <> show (i + 1) <> " of " <> show count <> " · " <> fromMaybe "" (steps !! i)) mi
        disabledIf back (maybe true (_ < 1) mi)
        disabledIf next (maybe true (\i -> i > count - 2) mi)
      moved step reported = do
        mi <- Ref.read idxRef
        for_ mi \i -> do
          let j = step i
          when (j >= 0 && j < count) do
            render (Just j)
            reported
    liftEffect $ selectorWiring steps
      { show: render
      , reports: \reported -> do
          listenNode back "click" (moved (_ - 1) reported)
          listenNode next "click" (moved (_ + 1) reported)
      , read: Ref.read idxRef
      }

navButton :: String -> String -> Web Node
navButton icon caption = do
  _ <- handles (staticHTML ("<button class=\"mdc-button\"><span class=\"mdc-button__ripple\"></span><i class=\"material-icons mdc-button__icon\" aria-hidden=\"true\">" <> escaped icon <> "</i><span class=\"mdc-button__label\">" <> escaped caption <> "</span></button>"))
  lastBuilt

lastBuilt :: Web Node
lastBuilt = gets \{ sibling } -> sibling

classIf :: Node -> String -> Boolean -> Effect Unit
classIf node name on = if on then addClass node name else removeClass node name

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
