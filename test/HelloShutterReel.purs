-- | The smallest runnable app that goes through both a **Reel** and a **Shutter**.
-- |
-- | Neither optic has a `(->)` instance — they need a profunctor that can hold
-- | state / loop, so the only inhabitant is `UI`. That is why this is a rendered
-- | app and not a pure-value trace.
-- |
-- | Flow: the app seeds the greeting prefix (the Reel's retained state) →
-- | type a name → click Greet (Shutter) → the greeting is shown. Everything
-- | is gated: no stage emits before the state it needs exists.
module HelloShutterReel where

import Prelude

import Data.Either (Either(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToVariant (shutter)
import Data.Profunctor.Row.VariantToRecord (reel)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (UI, silence, with)
import Web (Web, body, button, staticText, text)

-- | The **Reel** (+ → ×), a genuine two-beat: the *retained state* is the
-- | greeting **prefix**, installed from the model side (`Right` — the app's
-- | initial render seeds `"Hello, "`, exactly the "install a finisher"
-- | protocol of `retain`); the text field runs freely, and each typed name is
-- | finished against that retained prefix. Under the gated `Retaining (UI m)`
-- | nothing is emitted before the prefix has arrived — there is no greeting
-- | to fabricate. (For a reel whose state is *updated by events*, see
-- | `RestaurantReel`/`BusinessOptics`.)
greet :: UI Web String String
greet =
  reel
    (\prefix -> Right \typed -> prefix <> typed.name)
    (MDC.filledTextField @"name" { floatingLabel: "Your name" })

-- | The **Shutter** (× → +): open on the greeting, then snap shut on one value.
-- | The button click is a `Done` (`cont=false`), so the **build** leg fires and
-- | appends `"!"`. The **escape** leg is the `Loop` branch — it would fire on a
-- | `cont=true` emission (e.g. a text field), which a button never produces.
-- |
-- | Clicking *Greet* before typing does nothing: the button has received no
-- | value yet, so its click is withheld — the gates make a premature click
-- | silent instead of letting it fabricate a greeting.
confirm :: UI Web String String
confirm =
  shutter identity (_ <> "!") identity
    (button $ staticText "Greet")

-- | seed prefix (Reel state) → type name → click Greet (Shutter) → `text`
-- | shows the greeting.
main :: Effect Unit
main = body $ with "Hello, " $ Semigroupoid.do
  greet
  confirm
  text
  silence
