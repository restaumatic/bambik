-- | The **pipeline block**: the lines of a screen, top to bottom, in the
-- | order they are shown.
-- |
-- | ```purescript
-- | import PUI.Pipeline as Pipeline
-- |
-- | body $ Pipeline.do
-- |   headline4 (text countLine)
-- |   button @"Count" {}
-- |   snackbar @"Count" countedLine # fold increment
-- | # looped @( counted :: Int ) # with freshCount
-- | ```
-- |
-- | A line is either a **stage** — it outputs a row or a case, which the
-- | next line is fed (`>>>`) — or a line that **shows data**: it reads a
-- | record and outputs `{}`, the empty record (a display, a static, a pane,
-- | a list of displays). A line that shows data changes no data, so it is
-- | not composed into the flow but applied to it as a decorator **before**
-- | what follows (`prefixed`), fed whatever that part is fed — the heading
-- | before its section, the count before the button. Code order is DOM
-- | order.
-- |
-- | A block's last line has nothing after it, so it is composed like a
-- | stage: a block whose lines all show data shows data itself (it outputs
-- | `{}`) and sits as one line in the block around it, or as the content of
-- | a pane. Displays that end a block **after** a stage whose row the block
-- | must still release — the totals after the sliders, the lap list after
-- | the clock — say so on that stage: `slider @"Split between" {} #
-- | suffixed (…)`. The block cannot decide it for them: with the view model
-- | not written yet (guardrails L18) a fold's row is unknown, and an
-- | unknown row could be the empty one.
-- |
-- | Which rule applies is read off the line's own type, which its leaf
-- | fixes even when the logic is a hole. Polymorphic packaged code whose
-- | stages are typed at a rigid row composes them with `>>>`.
module PUI.Pipeline
  ( discard
  , class Line
  , line
  , class LineRecord
  , lineRecord
  ) where

import Prelude (Unit, unit, (>>>))

import Effect.Class (class MonadEffect)
import PUI (PUI, prefixed)
import Prim.RowList (class RowToList, Nil)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

discard
  :: forall m a b b' c
   . MonadEffect m
  => Line a b b'
  => PUI m a b
  -> (Unit -> PUI m b' c)
  -> PUI m a c
discard s k = line s (k unit)

-- | How a line `PUI m a b` joins the rest of its block, which it feeds `b'`:
-- | a record output is looked at further (`LineRecord`); anything else — a
-- | case, an event — is a stage, composed with `>>>`.
class Line :: Type -> Type -> Type -> Constraint
class Line a b b' | a b -> b' where
  line :: forall m c. MonadEffect m => PUI m a b -> PUI m b' c -> PUI m a c

instance (RowToList o rl, LineRecord rl a o b') => Line a (Record o) b' where
  line = lineRecord (Proxy :: Proxy rl)
else instance Line a b b where
  line s k = s >>> k

-- | A record-reading line that outputs `{}` shows data: it decorates the rest
-- | of the block, before it, and the rest is fed what the line is fed. Any
-- | other record output is a stage, composed with `>>>`.
class LineRecord :: forall k. k -> Type -> Row Type -> Type -> Constraint
class LineRecord rl a o b' | rl a o -> b' where
  lineRecord :: forall m c. MonadEffect m => Proxy rl -> PUI m a (Record o) -> PUI m b' c -> PUI m a c

instance LineRecord Nil (Record i) o (Record i) where
  lineRecord _ s k = prefixed (unsafeCoerce s) k
else instance LineRecord rl a o (Record o) where
  lineRecord _ s k = s >>> k
