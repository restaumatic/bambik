-- | Bounded exhaustive checking of the merge laws — every script up to a
-- | stated length, with a fresh token per event — on the `PUI Effect`
-- | carrier, and conformance of the effectful record gate to its pure step
-- | (`PUI.Gate`). Why a bound is a proof here and not a sample:
-- | doc/observational-semantics.md, "The gate as a Mealy machine".
-- |
-- | A **script** is a sequence of events at the boundary of a merge: `Feed`
-- | (a fresh token fed), `FeedAgain` (the last token fed again), `FireK`
-- | (operand K emits a fresh token). A **rig** is a merge term wired to
-- | collect its boundary output stream as text; a **law** names two rigs, the
-- | event alphabet and length to enumerate, and the relation the two output
-- | streams must stand in at every prefix.
module Test.Exhaustive (run) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (recordToRecord)
import Data.Profunctor.Row.RecordToVariant (recordToVariant)
import Data.Profunctor.Row.VariantToRecord (variantToRecord)
import Data.Profunctor.Row.VariantToVariant (variantToVariant)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, match)
import Effect (Effect, foreachE)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import PUI (PUI(..), silence)
import PUI.Gate (GateInput(..), GateOutput(..), GateState, gateStep, initialGate)
import Unsafe.Coerce (unsafeCoerce)

data Event = Feed | FeedAgain | Fire1 | Fire2 | Fire3

derive instance Eq Event

instance Show Event where
  show = case _ of
    Feed -> "Feed"
    FeedAgain -> "FeedAgain"
    Fire1 -> "Fire1"
    Fire2 -> "Fire2"
    Fire3 -> "Fire3"

-- | A merge term at its boundary: feed it a token, fire operand K with a
-- | token, read what it emitted so far.
type Rig =
  { feed :: Int -> Effect Unit
  , fires :: Array (Int -> Effect Unit)
  , outs :: Ref.Ref (Array String)
  }

-- | How the left stream must relate to the right one, at every prefix.
data Rel = Equal | Refines

type Law =
  { name :: String
  , alphabet :: Array Event
  , len :: Int
  , left :: Effect Rig
  , right :: Effect Rig
  , rel :: Rel
  }

-- An operand: a component plus a hand trigger emitting `f` of a token.
type Op i o = { p :: PUI Effect i o, fire :: Int -> Effect Unit }

emitter :: forall o. Effect { emit :: o -> Effect Unit, register :: (o -> Effect Unit) -> Effect Unit }
emitter = do
  propRef <- Ref.new Nothing
  pure
    { emit: \v -> Ref.read propRef >>= \mp -> for_ mp \prop -> prop v
    , register: \prop -> Ref.write (Just prop) propRef
    }

-- ×→× citizen: echoes `f` of every feed, and can be fired.
echoOp :: forall o. (Int -> { | o }) -> Effect (Op { s :: Int } { | o })
echoOp f = do
  e <- emitter
  pure { p: PUI (pure { toUser: \r -> e.emit (f r.s), fromUser: e.register }), fire: e.emit <<< f }

-- ×→+ citizen: a feed arms and never emits; a fire emits `f` of the token.
armedOp :: forall o. (Int -> Variant o) -> Effect (Op { s :: Int } (Variant o))
armedOp f = do
  e <- emitter
  pure { p: PUI (pure { toUser: \_ -> pure unit, fromUser: e.register }), fire: e.emit <<< f }

-- +→+ citizen: forwards each occurrence through `g`, and can be fired.
forwardOp :: forall i o. (Variant i -> Variant o) -> (Int -> Variant o) -> Effect (Op (Variant i) (Variant o))
forwardOp g f = do
  e <- emitter
  pure { p: PUI (pure { toUser: e.emit <<< g, fromUser: e.register }), fire: e.emit <<< f }

-- +→× citizen: folds each occurrence through `g` into a whole row, and can
-- be fired.
foldOp :: forall i o. (Variant i -> { | o }) -> (Int -> { | o }) -> Effect (Op (Variant i) { | o })
foldOp g f = do
  e <- emitter
  pure { p: PUI (pure { toUser: e.emit <<< g, fromUser: e.register }), fire: e.emit <<< f }

-- The refinement witness: the operand minus its first emission.
quieter :: forall i o. Op i o -> Effect (Op i o)
quieter op = do
  spoken <- Ref.new false
  let
    p = PUI do
      inner <- unwrap op.p
      pure
        { toUser: inner.toUser
        , fromUser: \prop -> inner.fromUser \o -> do
            already <- Ref.read spoken
            if already then prop o else Ref.write true spoken
        }
  pure { p, fire: op.fire }

-- The exactness adversary: every emission carries a stale runtime copy of
-- the sibling's field, typed at the operand's own narrow row — what an echo
-- wire over the widening coercion hands the gate.
fatA :: Int -> { a :: Int }
fatA n = unsafeCoerce { a: n, b: 999 }

rig :: forall i o. Show o => (Int -> i) -> PUI Effect i o -> Array (Int -> Effect Unit) -> Effect Rig
rig mkFeed m fires = do
  m' <- unwrap m
  outs <- Ref.new []
  m'.fromUser \o -> Ref.modify_ (_ <> [ show o ]) outs
  pure { feed: m'.toUser <<< mkFeed, fires, outs }

-- A rig whose feeds are no-ops: equal streams mean feeds never emit.
deaf :: Effect Rig -> Effect Rig
deaf mk = mk <#> _ { feed = \_ -> pure unit }

runScript :: Effect Rig -> Array Event -> Effect (Array (Array String))
runScript mk script = do
  r <- mk
  counter <- Ref.new 0
  lastFed <- Ref.new 0
  snaps <- Ref.new []
  let
    fresh = Ref.modify (_ + 1) counter
    fireAt k n = for_ (Array.index r.fires k) \f -> f n
  for_ script \ev -> do
    case ev of
      Feed -> do
        n <- fresh
        Ref.write n lastFed
        r.feed n
      FeedAgain -> Ref.read lastFed >>= r.feed
      Fire1 -> fresh >>= fireAt 0
      Fire2 -> fresh >>= fireAt 1
      Fire3 -> fresh >>= fireAt 2
    Ref.read r.outs >>= \o -> Ref.modify_ (_ <> [ o ]) snaps
  Ref.read snaps

scriptsOf :: Array Event -> Int -> Array (Array Event)
scriptsOf _ 0 = [ [] ]
scriptsOf alphabet n = do
  s <- scriptsOf alphabet (n - 1)
  k <- alphabet
  pure (Array.snoc s k)

isSubsequence :: forall a. Eq a => Array a -> Array a -> Boolean
isSubsequence xs ys = case Array.uncons xs, Array.uncons ys of
  Nothing, _ -> true
  Just _, Nothing -> false
  Just x, Just y -> isSubsequence (if x.head == y.head then x.tail else xs) y.tail

dedupConsecutive :: forall a. Eq a => Array a -> Array a
dedupConsecutive = foldl (\acc x -> if Array.last acc == Just x then acc else acc <> [ x ]) []

holds :: Rel -> Array String -> Array String -> Boolean
holds Equal l r = l == r
holds Refines l r = isSubsequence l r

failing :: forall a. Show a => String -> Array Event -> a -> a -> Effect Unit
failing name script l r =
  throw (name <> " fails on " <> show script <> ": left " <> show l <> ", right " <> show r)

checkLaw :: Law -> Effect Int
checkLaw law = do
  log ("Exhaustive: " <> law.name)
  let scripts = scriptsOf law.alphabet law.len
  -- `foreachE`, not `for_`: a `for_` over sixteen thousand scripts in
  -- `Effect` nests one frame per script and overflows the stack
  foreachE scripts \script -> do
    ls <- runScript law.left script
    rs <- runScript law.right script
    for_ (Array.zip ls rs) \(Tuple l r) ->
      unless (holds law.rel l r) (failing law.name script l r)
  pure (Array.length scripts)

-- Feed-idempotence of a term: a `FeedAgain` directly after a feed may be
-- deleted without changing the stream up to stutter.
checkRepetition :: String -> Array Event -> Int -> Effect Rig -> Effect Int
checkRepetition name alphabet len mk = do
  let scripts = scriptsOf alphabet len
  foreachE scripts \script ->
    for_ (Array.range 1 (Array.length script - 1)) \i ->
      when (Array.index script i == Just FeedAgain && (Array.index script (i - 1) == Just Feed || Array.index script (i - 1) == Just FeedAgain)) do
        full <- runScript mk script
        for_ (Array.deleteAt i script) \shorter -> do
          less <- runScript mk shorter
          let
            l = dedupConsecutive (fromMaybeEmpty (Array.last full))
            r = dedupConsecutive (fromMaybeEmpty (Array.last less))
          unless (l == r) (failing name script l r)
  pure (Array.length scripts)
  where
  fromMaybeEmpty = case _ of
    Just xs -> xs
    Nothing -> []

--------------------------------------------------------------------------------
-- The rigs
--------------------------------------------------------------------------------

recordFeed :: Int -> { s :: Int }
recordFeed n = { s: n }

-- dispatch feeds by token parity (two operands) or residue (three)
case2 :: Int -> Variant (x :: Int, y :: Int)
case2 n = if n `mod` 2 == 0 then .x n else .y n

case3 :: Int -> Variant (x :: Int, y :: Int, z :: Int)
case3 n = case n `mod` 3 of
  0 -> .x n
  1 -> .y n
  _ -> .z n

-- ×→×
rrA :: Effect (Op { s :: Int } { a :: Int })
rrA = echoOp \n -> { a: n }

rrB :: Effect (Op { s :: Int } { b :: Int })
rrB = echoOp \n -> { b: n }

rrC :: Effect (Op { s :: Int } { c :: Int })
rrC = echoOp \n -> { c: n }

rrTwo :: (PUI Effect { s :: Int } { a :: Int } -> PUI Effect { s :: Int } { b :: Int } -> PUI Effect { s :: Int } { a :: Int, b :: Int }) -> Effect Rig
rrTwo merge = do
  a <- rrA
  b <- rrB
  rig recordFeed (merge a.p b.p) [ a.fire, b.fire ]

rrThree :: (PUI Effect { s :: Int } { a :: Int } -> PUI Effect { s :: Int } { b :: Int } -> PUI Effect { s :: Int } { c :: Int } -> PUI Effect { s :: Int } { a :: Int, b :: Int, c :: Int }) -> Effect Rig
rrThree merge = do
  a <- rrA
  b <- rrB
  c <- rrC
  rig recordFeed (merge a.p b.p c.p) [ a.fire, b.fire, c.fire ]

-- the pure gate driven directly by the same script: the conformance oracle
pureGateRR :: Effect Rig
pureGateRR = do
  st <- Ref.new (initialGate Nothing Nothing :: GateState (a :: Int) (b :: Int))
  outs <- Ref.new []
  let
    step inp = do
      s <- Ref.read st
      let Tuple s' out = gateStep { owns1: true, owns2: true } s inp
      Ref.write s' st
      case out of
        Released (o :: { a :: Int, b :: Int }) -> Ref.modify_ (_ <> [ show o ]) outs
        _ -> pure unit
  pure
    { feed: \n -> step StepBegun *> step (Contributed1 { a: n }) *> step (Contributed2 { b: n }) *> step StepEnded
    , fires: [ \n -> step (Contributed1 { a: n }), \n -> step (Contributed2 { b: n }) ]
    , outs
    }

-- +→×
vrA :: Effect (Op (Variant (x :: Int)) { a :: Int })
vrA = foldOp (match { x: \n -> { a: n } }) \n -> { a: n }

vrB :: Effect (Op (Variant (y :: Int)) { b :: Int })
vrB = foldOp (match { y: \n -> { b: n } }) \n -> { b: n }

vrC :: Effect (Op (Variant (z :: Int)) { c :: Int })
vrC = foldOp (match { z: \n -> { c: n } }) \n -> { c: n }

vrTwo :: (PUI Effect (Variant (x :: Int)) { a :: Int } -> PUI Effect (Variant (y :: Int)) { b :: Int } -> PUI Effect (Variant (x :: Int, y :: Int)) { a :: Int, b :: Int }) -> Effect Rig
vrTwo merge = do
  a <- vrA
  b <- vrB
  rig case2 (merge a.p b.p) [ a.fire, b.fire ]

vrThree :: (PUI Effect (Variant (x :: Int)) { a :: Int } -> PUI Effect (Variant (y :: Int)) { b :: Int } -> PUI Effect (Variant (z :: Int)) { c :: Int } -> PUI Effect (Variant (x :: Int, y :: Int, z :: Int)) { a :: Int, b :: Int, c :: Int }) -> Effect Rig
vrThree merge = do
  a <- vrA
  b <- vrB
  c <- vrC
  rig case3 (merge a.p b.p c.p) [ a.fire, b.fire, c.fire ]

pureGateVR :: Effect Rig
pureGateVR = do
  st <- Ref.new (initialGate Nothing Nothing :: GateState (a :: Int) (b :: Int))
  outs <- Ref.new []
  let
    step inp = do
      s <- Ref.read st
      let Tuple s' out = gateStep { owns1: true, owns2: true } s inp
      Ref.write s' st
      case out of
        Released (o :: { a :: Int, b :: Int }) -> Ref.modify_ (_ <> [ show o ]) outs
        _ -> pure unit
  pure
    { feed: \n -> step StepBegun *> (if n `mod` 2 == 0 then step (Contributed1 { a: n }) else step (Contributed2 { b: n })) *> step StepEnded
    , fires: [ \n -> step (Contributed1 { a: n }), \n -> step (Contributed2 { b: n }) ]
    , outs
    }

-- ×→+
rvA :: Effect (Op { s :: Int } (Variant (x :: Int)))
rvA = armedOp \n -> .x n

rvB :: Effect (Op { s :: Int } (Variant (y :: Int)))
rvB = armedOp \n -> .y n

rvC :: Effect (Op { s :: Int } (Variant (z :: Int)))
rvC = armedOp \n -> .z n

rvTwo :: (PUI Effect { s :: Int } (Variant (x :: Int)) -> PUI Effect { s :: Int } (Variant (y :: Int)) -> PUI Effect { s :: Int } (Variant (x :: Int, y :: Int))) -> Effect Rig
rvTwo merge = do
  a <- rvA
  b <- rvB
  rig recordFeed (merge a.p b.p) [ a.fire, b.fire ]

rvThree :: (PUI Effect { s :: Int } (Variant (x :: Int)) -> PUI Effect { s :: Int } (Variant (y :: Int)) -> PUI Effect { s :: Int } (Variant (z :: Int)) -> PUI Effect { s :: Int } (Variant (x :: Int, y :: Int, z :: Int))) -> Effect Rig
rvThree merge = do
  a <- rvA
  b <- rvB
  c <- rvC
  rig recordFeed (merge a.p b.p c.p) [ a.fire, b.fire, c.fire ]

-- +→+
vvA :: Effect (Op (Variant (x :: Int)) (Variant (ok :: Int)))
vvA = forwardOp (match { x: \n -> .ok n }) \n -> .ok n

vvB :: Effect (Op (Variant (y :: Int)) (Variant (err :: Int)))
vvB = forwardOp (match { y: \n -> .err n }) \n -> .err n

vvC :: Effect (Op (Variant (z :: Int)) (Variant (ok :: Int)))
vvC = forwardOp (match { z: \n -> .ok n }) \n -> .ok n

vvTwo :: (PUI Effect (Variant (x :: Int)) (Variant (ok :: Int)) -> PUI Effect (Variant (y :: Int)) (Variant (err :: Int)) -> PUI Effect (Variant (x :: Int, y :: Int)) (Variant (ok :: Int, err :: Int))) -> Effect Rig
vvTwo merge = do
  a <- vvA
  b <- vvB
  rig case2 (merge a.p b.p) [ a.fire, b.fire ]

vvThree :: (PUI Effect (Variant (x :: Int)) (Variant (ok :: Int)) -> PUI Effect (Variant (y :: Int)) (Variant (err :: Int)) -> PUI Effect (Variant (z :: Int)) (Variant (ok :: Int)) -> PUI Effect (Variant (x :: Int, y :: Int, z :: Int)) (Variant (ok :: Int, err :: Int))) -> Effect Rig
vvThree merge = do
  a <- vvA
  b <- vvB
  c <- vvC
  rig case3 (merge a.p b.p c.p) [ a.fire, b.fire, c.fire ]

--------------------------------------------------------------------------------
-- The laws
--------------------------------------------------------------------------------

-- The bounds (doc/observational-semantics.md, "The gate as a Mealy
-- machine"): a shortest distinguishing script is no longer than the number
-- of reachable joint control states of the two rigs compared, which is one
-- per subset of operands that has spoken — four for two operands, eight for
-- three.
two :: Int
two = 6

three :: Int
three = 8

rrAlphabet :: Array Event
rrAlphabet = [ Feed, FeedAgain, Fire1, Fire2 ]

rrAlphabet3 :: Array Event
rrAlphabet3 = [ Feed, Fire1, Fire2, Fire3 ]

evAlphabet :: Array Event
evAlphabet = [ Feed, Fire1, Fire2 ]

evAlphabet3 :: Array Event
evAlphabet3 = [ Feed, Fire1, Fire2, Fire3 ]

unitRR :: PUI Effect {} {}
unitRR = identity

unitVV :: PUI Effect (Variant ()) (Variant ())
unitVV = identity

unitVR :: PUI Effect (Variant ()) {}
unitVR = lcmap case_ identity

unitRV :: PUI Effect {} (Variant ())
unitRV = silence

laws :: Array Law
laws =
  -- ×→×
  [ { name: "×→× symmetry", alphabet: rrAlphabet, len: two, rel: Equal
    , left: rrTwo recordToRecord, right: rrTwo \a b -> recordToRecord b a }
  , { name: "×→× associativity", alphabet: rrAlphabet3, len: three, rel: Equal
    , left: rrThree \a b c -> recordToRecord (recordToRecord a b) c
    , right: rrThree \a b c -> recordToRecord a (recordToRecord b c) }
  , { name: "×→× left unit", alphabet: rrAlphabet, len: two, rel: Equal
    , left: rrTwo \a b -> recordToRecord (recordToRecord unitRR a) b, right: rrTwo recordToRecord }
  , { name: "×→× right unit", alphabet: rrAlphabet, len: two, rel: Equal
    , left: rrTwo \a b -> recordToRecord (recordToRecord a unitRR) b, right: rrTwo recordToRecord }
  , { name: "×→× exactness", alphabet: rrAlphabet, len: two, rel: Equal
    , left: do
        a <- echoOp fatA
        b <- rrB
        rig recordFeed (recordToRecord a.p b.p) [ a.fire, b.fire ]
    , right: rrTwo recordToRecord }
  , { name: "×→× monotonicity", alphabet: rrAlphabet, len: two, rel: Refines
    , left: do
        a <- rrA >>= quieter
        b <- rrB
        rig recordFeed (recordToRecord a.p b.p) [ a.fire, b.fire ]
    , right: rrTwo recordToRecord }
  , { name: "×→× conformance to PUI.Gate", alphabet: rrAlphabet, len: two, rel: Equal
    , left: rrTwo recordToRecord, right: pureGateRR }
  -- +→×
  , { name: "+→× symmetry", alphabet: evAlphabet, len: two, rel: Equal
    , left: vrTwo variantToRecord, right: vrTwo \a b -> variantToRecord b a }
  , { name: "+→× associativity", alphabet: evAlphabet3, len: three, rel: Equal
    , left: vrThree \a b c -> variantToRecord (variantToRecord a b) c
    , right: vrThree \a b c -> variantToRecord a (variantToRecord b c) }
  , { name: "+→× left unit", alphabet: evAlphabet, len: two, rel: Equal
    , left: vrTwo \a b -> variantToRecord (variantToRecord unitVR a) b, right: vrTwo variantToRecord }
  , { name: "+→× right unit", alphabet: evAlphabet, len: two, rel: Equal
    , left: vrTwo \a b -> variantToRecord (variantToRecord a unitVR) b, right: vrTwo variantToRecord }
  , { name: "+→× exactness", alphabet: evAlphabet, len: two, rel: Equal
    , left: do
        a <- foldOp (match { x: fatA }) fatA
        b <- vrB
        rig case2 (variantToRecord a.p b.p) [ a.fire, b.fire ]
    , right: vrTwo variantToRecord }
  , { name: "+→× monotonicity", alphabet: evAlphabet, len: two, rel: Refines
    , left: do
        a <- vrA >>= quieter
        b <- vrB
        rig case2 (variantToRecord a.p b.p) [ a.fire, b.fire ]
    , right: vrTwo variantToRecord }
  , { name: "+→× conformance to PUI.Gate", alphabet: evAlphabet, len: two, rel: Equal
    , left: vrTwo variantToRecord, right: pureGateVR }
  -- ×→+
  , { name: "×→+ symmetry", alphabet: evAlphabet, len: two, rel: Equal
    , left: rvTwo recordToVariant, right: rvTwo \a b -> recordToVariant b a }
  , { name: "×→+ associativity", alphabet: evAlphabet3, len: three, rel: Equal
    , left: rvThree \a b c -> recordToVariant (recordToVariant a b) c
    , right: rvThree \a b c -> recordToVariant a (recordToVariant b c) }
  , { name: "×→+ left unit", alphabet: evAlphabet, len: two, rel: Equal
    , left: rvTwo \a b -> recordToVariant (recordToVariant unitRV a) b, right: rvTwo recordToVariant }
  , { name: "×→+ right unit", alphabet: evAlphabet, len: two, rel: Equal
    , left: rvTwo \a b -> recordToVariant (recordToVariant a unitRV) b, right: rvTwo recordToVariant }
  , { name: "×→+ monotonicity", alphabet: evAlphabet, len: two, rel: Refines
    , left: do
        a <- rvA >>= quieter
        b <- rvB
        rig recordFeed (recordToVariant a.p b.p) [ a.fire, b.fire ]
    , right: rvTwo recordToVariant }
  , { name: "×→+ arming (feeds never emit)", alphabet: evAlphabet, len: two, rel: Equal
    , left: rvTwo recordToVariant, right: deaf (rvTwo recordToVariant) }
  -- +→+
  , { name: "+→+ symmetry", alphabet: evAlphabet, len: two, rel: Equal
    , left: vvTwo variantToVariant, right: vvTwo \a b -> variantToVariant b a }
  , { name: "+→+ associativity", alphabet: evAlphabet3, len: three, rel: Equal
    , left: vvThree \a b c -> variantToVariant (variantToVariant a b) c
    , right: vvThree \a b c -> variantToVariant a (variantToVariant b c) }
  , { name: "+→+ left unit", alphabet: evAlphabet, len: two, rel: Equal
    , left: vvTwo \a b -> variantToVariant (variantToVariant unitVV a) b, right: vvTwo variantToVariant }
  , { name: "+→+ right unit", alphabet: evAlphabet, len: two, rel: Equal
    , left: vvTwo \a b -> variantToVariant (variantToVariant a unitVV) b, right: vvTwo variantToVariant }
  , { name: "+→+ monotonicity", alphabet: evAlphabet, len: two, rel: Refines
    , left: do
        a <- vvA >>= quieter
        b <- vvB
        rig case2 (variantToVariant a.p b.p) [ a.fire, b.fire ]
    , right: vvTwo variantToVariant }
  ]

run :: Effect Unit
run = do
  total <- Ref.new 0
  for_ laws \law -> do
    n <- checkLaw law
    Ref.modify_ (_ + n) total
  n <- checkRepetition "×→× repetition (feed-idempotence of the merge)" rrAlphabet two (rrTwo recordToRecord)
  Ref.modify_ (_ + n) total
  count <- Ref.read total
  log ("Exhaustive: " <> show (Array.length laws + 1) <> " laws over " <> show count <> " scripts, no distinguishing script found")
