-- Opis by MB:
-- 
-- Mam formularz do konfiguracji integracji. Chciałbym mieć w nim takie zachowanie:
-- Jak wybiorę restaurację i ustawię rodzaj integracji na UberDirect, to pole w tym formularzu powinno sobie ustawić wartość na podstawie wyniku RPC (no i jak zmienię resto, to przeliczyć)
-- Jak RPC zwróci Nothing to powinien się pojawić guzik przy tym polu, który pozwala odpalić inne RPC, które potencjalnie ustawi tę wartość.
-- Jak coś takiego osiągnąć w R.Formie? Jedyne co mi przychodzi do głowy to użyć withEffect. Ale w docsach jest do tego dużo ostrzeżeń, a nie znalazłem dobrego przykładu w kodzie. W sumie to bardziej chodzi o tę pierwszą część, bo ta druga to action + merge. A w tej pierwszej jakoś nie bardzo pasuje zrobić readDynamic i withInitialValue.
-- Kontekst jest taki, że UberDirect ma coś takiego jak organizacja i podorgranizacje. Zamiast prosić użytkowników, żeby wyklikali sobie konto w UberDirect i podali nam swój zestaw credentiali, to możemy stworzyć dla każdej restauracji podorganizacje w ramach naszej organizacji. Pierwsze RPC sprawdza czy z jakąś restauracją jest już powiązana jakaś organizacja i jeśli tak, to uzupełniamy jej id w tym polu. Drugie RPC pozwala na utworzenie nowej organizacji i powiązanie z nią restauracji.
module Main (main) where

import Prelude hiding (div)

import Data.Lens.Extra.Commons (field, nothing)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Endo as Endo
import Data.Profunctor.Sum as Sum
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
import Web (body, p, slot, staticText, text)

main :: Effect Unit
main = body $ MDC.elevation10 $ Semigroupoid.do
  MDC.subtitle1 $ staticText "Lookup/insert map demo"
  p $ MDC.caption $ staticText "Provide a map key. For keys 'a', 'b' and 'c' the values are already present in the map. For other keys provide a value that will be inserted to the map. Ultimately, the value will be displayed in the card below. "
  MDC.filledTextField { floatingLabel: "Key" }
  MDC.containedButton { label: Just "Lookup", icon: Nothing }
  lookup MDC.indeterminateLinearProgress
  Endo.do
    Semigroupoid.do
      field @"mvalue" $ nothing "" $ slot $ Semigroupoid.do
        filledTextField { floatingLabel: "Value" }
        MDC.containedButton { label: Just "Insert", icon: Nothing }
      insert MDC.indeterminateLinearProgress
    field @"mvalue" $ slot $ MDC.card $ Sum.do
      MDC.subtitle2 $ staticText "Value "
      MDC.caption $ text

type Key = String
type Value = String

type LookupResult =
  { key :: Key
  , mvalue :: Maybe Value
  }

lookup :: Action Key LookupResult Boolean Void
lookup = action \key -> do
  delay $ Milliseconds 300.0
  mvalue <- liftEffect $ Ref.read mapRef <#> Map.lookup key
  pure { key, mvalue }

insert :: Action LookupResult LookupResult Boolean Void
insert = action \{ key, mvalue } -> do
  delay (Milliseconds 300.0)
  for_ mvalue \value -> liftEffect $ Ref.modify_ (Map.insert key value) mapRef
  pure { key, mvalue }

mapRef :: Ref (Map Key Value)
mapRef = unsafePerformEffect $ Ref.new $ Map.fromFoldable
  [ Tuple "a" "GXHJK" 
  , Tuple "b" "OJAKL" 
  , Tuple "c" "HUQOO" 
  ]
