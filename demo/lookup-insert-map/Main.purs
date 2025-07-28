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

import Data.Lens.Extra.Commons (just, missing, missing', nothing)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Sum as Sum
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import MDC (filledTextField)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (action)
import Web (body, div, p, slot, staticText, text)

main :: Effect Unit
main = body $ div $ Semigroupoid.do
    p $ staticText "Lookup/insert map demo"
    MDC.filledTextField { floatingLabel: "Key" }
    MDC.containedButton { label: Just "Lookup", icon: Nothing }
    action (\key -> do
      delay (Milliseconds 1000.0)
      pure $ case key of
        "A" -> Just "GXHJK"
        "B" -> Just "OJAKL"
        "C" -> Just "HUQOO"
        _ -> Nothing) $ MDC.indeterminateLinearProgress
    -- Sum.do
    nothing "" $ slot Semigroupoid.do
      filledTextField { floatingLabel: "Create Value" }
      MDC.containedButton { label: Just "Insert", icon: Nothing }
      action (\value -> pure value) $ MDC.indeterminateLinearProgress
    slot $ text
    -- missing "" $ filledTextField { floatingLabel: "Value" }
    -- p $ text
    pzero

