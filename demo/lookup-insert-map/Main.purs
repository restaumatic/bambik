-- Mam formularz do konfiguracji integracji. Chciałbym mieć w nim takie zachowanie:
-- Jak wybiorę restaurację i ustawię rodzaj integracji na UberDirect, to pole w tym formularzu powinno sobie ustawić wartość na podstawie wyniku RPC (no i jak zmienię resto, to przeliczyć)
-- Jak RPC zwróci Nothing to powinien się pojawić guzik przy tym polu, który pozwala odpalić inne RPC, które potencjalnie ustawi tę wartość.
-- Jak coś takiego osiągnąć w R.Formie? Jedyne co mi przychodzi do głowy to użyć withEffect. Ale w docsach jest do tego dużo ostrzeżeń, a nie znalazłem dobrego przykładu w kodzie. W sumie to bardziej chodzi o tę pierwszą część, bo ta druga to action + merge. A w tej pierwszej jakoś nie bardzo pasuje zrobić readDynamic i withInitialValue.
-- Kontekst jest taki, że UberDirect ma coś takiego jak organizacja i podorgranizacje. Zamiast prosić użytkowników, żeby wyklikali sobie konto w UberDirect i podali nam swój zestaw credentiali, to możemy stworzyć dla każdej restauracji podorganizacje w ramach naszej organizacji. Pierwsze RPC sprawdza czy z jakąś restauracją jest już powiązana jakaś organizacja i jeśli tak, to uzupełniamy jej id w tym polu. Drugie RPC pozwala na utworzenie nowej organizacji i powiązanie z nią restauracji.

-- key text field -> lookup button -> lookup value progress bar --------------------------------------------------------------> value
--                                                              -> create value button -> create value progress bar (effect) -> value

-- Key
-- Value
module Main (main) where

import Prelude hiding (div)

import Data.Lens.Extra.Commons (field, nothing)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Endo as Endo
import Data.Profunctor.Sum as Sum
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import MDC (filledTextField)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (Action, action)
import Web (body, p, slot, staticText, text)

main :: Effect Unit
main = body $ MDC.elevation10 $ Semigroupoid.do
  MDC.subtitle1 $ staticText "Lookup/insert map demo"
  p $ MDC.caption $ staticText "Provide a map key. For keys 'a', 'b' and 'c' the values are present in the map. For other keys provide a value that will be inserted to the map. Ultimately, the value will be displayed in the card below. "
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
  pure
    { key
    , mvalue: case key of
      "a" -> Just "GXHJK"
      "b" -> Just "OJAKL"
      "c" -> Just "HUQOO"
      _ -> Nothing
    }

insert :: Action LookupResult LookupResult Boolean Void
insert = action \{ key, mvalue } -> do
  delay (Milliseconds 1000.0)
  pure { key, mvalue }