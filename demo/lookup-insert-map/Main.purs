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
import Data.Lens (Iso)
import Data.Lens.Extra.Commons (constructor, field, nothing, withDefault)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Endo as Endo
import Data.Profunctor.Sum as Sum
import Data.Profunctor.Zero (pzero)
import Data.String.Regex.Flags as ENdo
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (Action, UI, action, constant)
import Web (Web, body, label, p, slot, staticText, text)

main :: Effect Unit
main = body $ MDC.elevation10 $ Endo.do
  MDC.subtitle1 $ staticText "Integration form"
  MDC.card $ Endo.do
    glovo $ MDC.radioButton $ label $ staticText "Glovo"
    uberDirect $ MDC.radioButton $ label $ staticText "UberDirect"
  MDC.card $ Endo.do
    glovo $ slot $ glovoForm 
    uberDirect $ slot $ uberDirectForm 

glovoForm :: UI Web Unit Unit
glovoForm = staticText "Some Glovo-specific stuff"

uberDirectForm :: UI Web UberDirectForm UberDirectForm
uberDirectForm = Endo.do
  -- field @"restaurantId" $ MDC.filledTextField { floatingLabel: "Restaurant ID" }
  -- field @"organizationId" $ MDC.filledTextField { floatingLabel: "Organization ID" }
  -- Semigroupoid.do 
  --   MDC.containedButton { label: Just "Lookup organization ID", icon: Nothing }
  --   lookup' MDC.indeterminateLinearProgress
  field @"restaurantId" $ Semigroupoid.do
    p $ MDC.caption $ staticText "Provide restaurant ID. For ids 'a', 'b' and 'c' organization IDs are already generated. For other IDs an organization IDs will be generated. Ultimately, the organization ID will be displayed in the card below."
    MDC.filledTextField { floatingLabel: "Restaurant ID" }
    MDC.containedButton { label: Just "Lookup organization ID", icon: Nothing }
    lookup MDC.indeterminateLinearProgress
    Endo.do
      Semigroupoid.do
        field @"mOrganizationId" $ nothing "" $ slot $ MDC.card Endo.do
          MDC.caption $ staticText "No organization ID found"
          MDC.containedButton { label: Just "Generate organization ID", icon: Nothing }
        generate MDC.indeterminateLinearProgress
      field @"mOrganizationId" $ withDefault "" $ MDC.filledTextField { floatingLabel: "Organization ID" }
        -- MDC.card $ Sum.do
        --   staticText "Organization ID: "
        --   text
      Endo.do
        field @"restaurantId" $ MDC.caption Sum.do
          constant "Restaurant ID: " $ text
          text
        field @"mOrganizationId" $ withDefault "None" $ MDC.caption Sum.do
          constant "Organization ID: " $ text
          text
    pzero

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
  UberDirect c -> Nothing
  _ -> Just unit

type LookupResult =
  { restaurantId :: RestaurantId
  , mOrganizationId :: Maybe OrganizationId
  }

lookup :: Action RestaurantId LookupResult Boolean Void
lookup = action \restaurantId -> do
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
