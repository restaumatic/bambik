-- Opis by MB:
-- 
-- Mam formularz do konfiguracji integracji. Chciałbym mieć w nim takie zachowanie:
-- Jak wybiorę restaurację i ustawię rodzaj integracji na UberDirect, to pole w tym formularzu powinno sobie ustawić wartość na podstawie wyniku RPC (no i jak zmienię resto, to przeliczyć)
-- Jak RPC zwróci Nothing to powinien się pojawić guzik przy tym polu, który pozwala odpalić inne RPC, które potencjalnie ustawi tę wartość.
-- Jak coś takiego osiągnąć w R.Formie? Jedyne co mi przychodzi do głowy to użyć withEffect. Ale w docsach jest do tego dużo ostrzeżeń, a nie znalazłem dobrego przykładu w kodzie. W sumie to bardziej chodzi o tę pierwszą część, bo ta druga to action + merge. A w tej pierwszej jakoś nie bardzo pasuje zrobić readDynamic i withInitialValue.
-- Kontekst jest taki, że UberDirect ma coś takiego jak organizacja i podorgranizacje. Zamiast prosić użytkowników, żeby wyklikali sobie konto w UberDirect i podali nam swój zestaw credentiali, to możemy stworzyć dla każdej restauracji podorganizacje w ramach naszej organizacji. Pierwsze RPC sprawdza czy z jakąś restauracją jest już powiązana jakaś organizacja i jeśli tak, to uzupełniamy jej id w tym polu. Drugie RPC pozwala na utworzenie nowej organizacji i powiązanie z nią restauracji.
module Main (main) where

import Prelude hiding (div)

import Data.Default (class Default)
import Data.Lens (Iso, Lens, lens)
import Data.Lens.Extra.Commons (constructor, field, input, just, nothing, output)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Endo as Form
import Data.Profunctor.Sum as View
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Flow
import UI (Action, UI, action, debounced)
import Web (Web, body, variant, label, staticText, text, transient)

main :: Effect Unit
main = body $ MDC.elevation10 $ Form.do
  MDC.subtitle1 $ staticText "Integration form"
  glovo $ MDC.radioButton $ label $ staticText "Glovo"
  uberDirect $ MDC.radioButton $ label $ staticText "UberDirect"
  glovo $ variant $ glovoForm 
  uberDirect $ variant $ uberDirectForm 

glovoForm :: UI Web Unit Unit
glovoForm = MDC.card $ staticText "Some Glovo-specific stuff"

uberDirectForm :: UI Web UberDirectForm UberDirectForm
uberDirectForm = MDC.card $ Form.do
  restaurantIdInput $ Flow.do
    MDC.filledTextField { floatingLabel: "Restaurant ID" }
    debounced $ organizationIdLookup MDC.indeterminateLinearProgress
    transient $ Form.do
      field @"mOrganizationId" $ just $ variant $ Form.do
        MDC.caption $ Form.do
          staticText "Found organization ID: "
          text
        MDC.containedButton { label: Just "Use found organization ID", icon: Nothing }
      Flow.do
        field @"mOrganizationId" $ nothing $ variant $ Form.do
          MDC.caption $ staticText "No organization ID found"
          MDC.containedButton { label: Just "Generate organization ID", icon: Nothing }
        generate MDC.indeterminateLinearProgress
      field @"mOrganizationId" $ nothing $ variant $ Form.do
        MDC.containedButton { label: Just "I'll provide already generated organization ID below", icon: Nothing }
  organizationIdInput $ MDC.filledTextField { floatingLabel: "Organization ID" }
  MDC.subtitle2 $ staticText "Preview "
  restaurantIdOutput $ MDC.caption View.do
    staticText "Restaurant ID: "
    text
  organizationIdOutput $ MDC.caption View.do
    staticText "Organization ID: "
    text

restaurantIdInput :: Lens UberDirectForm UberDirectForm RestaurantId LookupResult
restaurantIdInput = lens (_.restaurantId) (\form { restaurantId, mOrganizationId} -> form { restaurantId = restaurantId, organizationId = fromMaybe form.organizationId mOrganizationId })

restaurantIdOutput :: forall a. Lens UberDirectForm a RestaurantId Void
restaurantIdOutput = output @"restaurantId"

organizationIdInput :: Lens UberDirectForm UberDirectForm OrganizationId OrganizationId
organizationIdInput = input @"organizationId"

organizationIdOutput :: forall a. Lens UberDirectForm a OrganizationId Void
organizationIdOutput = output @"organizationId"

type RestaurantId = String

type OrganizationId = String

type UberDirectForm = { restaurantId :: RestaurantId, organizationId :: OrganizationId }

data Integration = UberDirect UberDirectForm | Glovo

instance Default Integration where
  default = Glovo

uberDirect :: Iso Integration Integration (Maybe { restaurantId :: RestaurantId, organizationId :: OrganizationId }) { restaurantId :: RestaurantId, organizationId :: OrganizationId }
uberDirect = constructor UberDirect case _ of
  UberDirect c -> Just c
  _ -> Nothing

glovo :: Iso Integration Integration (Maybe Unit) Unit
glovo = constructor (const Glovo) case _ of
  Glovo -> Just unit
  _ -> Nothing

type LookupResult =
  { restaurantId :: RestaurantId
  , mOrganizationId :: Maybe OrganizationId
  }

organizationIdLookup :: Action RestaurantId LookupResult Boolean Void
organizationIdLookup = action \restaurantId -> do
  delay $ Milliseconds 300.0
  mOrganizationId <- liftEffect $ Ref.read mapRef <#> Map.lookup restaurantId
  pure { restaurantId, mOrganizationId }

generate :: Action LookupResult LookupResult Boolean Void
generate = action \{ restaurantId, mOrganizationId } -> do
  delay (Milliseconds 300.0)
  let generatedIntegrationId = "GEN-" <> restaurantId
  liftEffect $ Ref.modify_ (Map.insert restaurantId generatedIntegrationId) mapRef
  pure { restaurantId, mOrganizationId: Just generatedIntegrationId }

mapRef :: Ref (Map RestaurantId OrganizationId)
mapRef = unsafePerformEffect $ Ref.new $ Map.fromFoldable
  [ Tuple "a" "GXHJK" 
  , Tuple "b" "OJAKL" 
  , Tuple "c" "HUQOO" 
  ]
