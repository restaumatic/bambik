-- | The smallest runnable app that goes through both a **Reel** and a **Shutter**.
-- |
-- | Neither optic has a `(->)` instance — they need a profunctor that can hold
-- | state / loop, so the only inhabitant is `UI`. That is why this is a rendered
-- | app and not a pure-value trace.
-- |
-- | Flow: type a name (Reel) → click Greet (Shutter) → the greeting is shown.
module HelloShutterReel where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToVariant (shutter)
import Data.Profunctor.Row.VariantToRecord (reelE)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Flow
import UI (UI)
import Web (Web, body, staticText, text)

-- | The **Reel** (+ → ×), built via the existential constructor `reelE`. Run in
-- | its *stateless* mode: every keystroke is a fresh focus (`decon = Left`), and
-- | the residual `c` is unused. Because `decon` is `Left`, the field echoes its
-- | value back through `recon` — including the initial `""` at startup, so `greet`
-- | emits `"Hello, "` immediately. That initial emission **seeds the downstream
-- | button**, which is what closes the premature-click hazard below.
-- | (For reels that actually carry state in `c`, see `RestaurantReel`/`BusinessOptics`.)
greet :: UI Web String String
greet =
  reelE
    (Left :: String -> Either String Unit)
    (\(Tuple typed _) -> "Hello, " <> typed)
    (MDC.filledTextField { floatingLabel: "Your name" })

-- | The **Shutter** (× → +): open on the greeting, then snap shut on one value.
-- | The button click is a `Done` (`cont=false`), so the **build** leg fires and
-- | appends `"!"`. The **escape** leg is the `Loop` branch — it would fire on a
-- | `cont=true` emission (e.g. a text field), which a button never produces.
-- |
-- | The button's value is **seeded at startup** by `greet`'s initial emission, so
-- | clicking *Greet* before typing yields `"Hello, !"` (a real greeting), not the
-- | uninitialized value it would otherwise hold.
confirm :: UI Web String String
confirm =
  shutter identity (_ <> "!") identity
    (MDC.containedButton { label: Just "Greet", icon: Nothing })

-- | type name (Reel) → click Greet (Shutter) → `text` shows the greeting.
main :: Effect Unit
main = body $ Flow.do
  greet
  confirm
  text
  staticText ""
