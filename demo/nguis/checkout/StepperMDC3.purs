module Checkout.StepperMDC3 (stepper) where

import Prelude (class Eq, Unit, bind, discard, pure, show, when, ($), (&&), (*>), (+), (-), (<), (<>), (==), (>), (>=), (>>>), (>>=))

import Control.Monad.State (gets)
import Data.Array (length, (!!))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Row.Structural (withStructuralEq)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons)
import PUI (PUI, fromHandles, handles)
import PUI.Web (Node, Web, addClass, appendChild, clazz, createTextNode, element, ensureStyle, escaped, listenNode, removeAttribute, removeClass, selectedAt, selectorWiring, setAttribute, setTextNodeValue, staticHTML, (:=))
import PUI.Web.HTML (div)
import Type.Proxy (Proxy(..))

stepper :: forall @l a b r. IsSymbol l => Cons l a b r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
stepper steps = withStructuralEq @a (selectedAt @l $ "name" := reflectSymbol (Proxy @l) >>> "aria-label" := reflectSymbol (Proxy @l) $ stepperLeaf steps)

stepperLeaf :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web (Maybe a) (Maybe a)
stepperLeaf steps =
  div >>> "class" := "md3-stepper" >>> "role" := "group" $ fromHandles do
    liftEffect $ ensureStyle "md3-stepper" stepperCss
    back <- navButton "chevron_left" "Back"
    { dots, caption } <- element "div" do
      dots <- element "div" (for steps \_ -> handles (staticHTML "<span class=\"md3-stepper__dot\" aria-hidden=\"true\"></span>") *> lastBuilt)
      clazz "md3-stepper__dots"
      caption <- liftEffect $ createTextNode ""
      _ <- handles (staticHTML "<span class=\"md3-stepper__caption\" aria-live=\"polite\"></span>")
      lastBuilt >>= \span -> liftEffect $ appendChild caption span
      pure { dots, caption }
    clazz "md3-stepper__progress"
    next <- navButton "chevron_right" "Next"
    idxRef <- liftEffect $ Ref.new Nothing
    let
      count = length steps
      render mi = do
        Ref.write mi idxRef
        forWithIndex_ dots \i dot -> classIf dot "md3-stepper__dot--current" (mi == Just i)
        setTextNodeValue caption $ maybe "" (\i -> "Step " <> show (i + 1) <> " of " <> show count <> " · " <> maybe "" stepLabel (steps !! i)) mi
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
  _ <- handles (staticHTML ("<md-text-button><md-icon slot=\"icon\">" <> escaped icon <> "</md-icon>" <> escaped caption <> "</md-text-button>"))
  lastBuilt

lastBuilt :: Web Node
lastBuilt = gets \{ sibling } -> sibling

stepLabel :: forall a. { value :: a, label :: String } -> String
stepLabel { label } = label

classIf :: Node -> String -> Boolean -> Effect Unit
classIf node name on = if on then addClass node name else removeClass node name

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
