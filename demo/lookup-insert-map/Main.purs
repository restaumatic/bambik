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
import Data.Lens.Extra.Commons (constructor, field, nothing)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Endo as Endo
import Data.Profunctor.Sum as Sum
import Data.Profunctor.Zero (pzero)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import MDC (filledTextField)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (Action, action)
import Web (body, label, p, slot, staticText, text)

main :: Effect Unit
main = body $ MDC.elevation10 $ 
  Endo.do
    MDC.subtitle1 $ staticText "UberDirect Organization API demo"
    MDC.card $ Endo.do
      glovo $ MDC.radioButton $ label $ staticText "Glovo"
      uberDirect $ MDC.radioButton $ label $ staticText "UberDirect"
    glovo $ slot $ MDC.card $ MDC.caption $ staticText "Some Glovo-specific stuff"
    uberDirect $ slot $ Endo.do
      p $ MDC.caption $ staticText "Provide restaurant id. For ids 'a', 'b' and 'c' integration ids are already present in the map. For other ids provide a value that will be registered. Ultimately, the integration id will be displayed in the card below. "
      field @"restaurantId" Semigroupoid.do
        MDC.filledTextField { floatingLabel: "Restaurant ID" }
        MDC.containedButton { label: Just "Lookup", icon: Nothing }
        lookup MDC.indeterminateLinearProgress
        Endo.do
          Semigroupoid.do
            field @"mIntegrationId" $ nothing "" $ slot $ Semigroupoid.do
              filledTextField { floatingLabel: "Integration ID" }
              MDC.containedButton { label: Just "Insert", icon: Nothing }
            insert MDC.indeterminateLinearProgress
          field @"mIntegrationId" $ slot $ Sum.do
            MDC.card $ Sum.do
              staticText "Integration ID: "
              text
            MDC.card $ MDC.caption $ staticText "Some further stuff about UberDirect integration"
        pzero

type RestaurantId = String

type IntegrationId = String

data Integration = UberDirect { restaurantId :: RestaurantId, integrationId :: IntegrationId } | Glovo

instance Default Integration where
  default = Glovo

uberDirect :: Iso Integration Integration (Maybe { restaurantId :: RestaurantId, integrationId :: IntegrationId }) { restaurantId :: RestaurantId, integrationId :: IntegrationId }
uberDirect = constructor UberDirect case _ of
  UberDirect c -> Just c
  _ -> Nothing

glovo :: Iso Integration Integration (Maybe Unit) Unit
glovo = constructor (const Glovo) case _ of
  UberDirect c -> Nothing
  _ -> Just unit

type LookupResult =
  { restaurantId :: RestaurantId
  , mIntegrationId :: Maybe IntegrationId
  }

lookup :: Action RestaurantId LookupResult Boolean Void
lookup = action \restaurantId -> do
  delay $ Milliseconds 300.0
  mIntegrationId <- liftEffect $ Ref.read mapRef <#> Map.lookup restaurantId
  pure { restaurantId, mIntegrationId }

insert :: Action LookupResult LookupResult Boolean Void
insert = action \{ restaurantId, mIntegrationId } -> do
  delay (Milliseconds 300.0)
  for_ mIntegrationId \value -> liftEffect $ Ref.modify_ (Map.insert restaurantId value) mapRef
  pure { restaurantId, mIntegrationId }

mapRef :: Ref (Map RestaurantId IntegrationId)
mapRef = unsafePerformEffect $ Ref.new $ Map.fromFoldable
  [ Tuple "a" "GXHJK" 
  , Tuple "b" "OJAKL" 
  , Tuple "c" "HUQOO" 
  ]
