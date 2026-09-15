-- | Bounded exhaustive checking of the merge laws — every script up to a
-- | stated length, with a fresh token per event — on the `PUI Effect`
-- | carrier, and conformance of every effectful gate to the one pure step
-- | (`PUI.Gate`): the two record-output merges over their labels, the
-- | container action's gather over its keys. Why a bound is a proof here
-- | and not a sample: doc/observational-semantics.md, "The gate as a Mealy
-- | machine".
-- |
-- | A **script** is a sequence of events at the boundary of a term: `Feed`
-- | (a fresh token fed), `FeedAgain` (the last feed repeated, token and
-- | shape), `FireK` (operand or element K emits a fresh token), and for the
-- | collection the two other feed shapes — `FeedOnly1` (an array holding
-- | element 1 alone) and `FeedNone` (the empty array), which rekey the
-- | gate. A **rig** is a term wired to collect its boundary output stream as
-- | text; a **law** names two rigs, the event alphabet and length to
-- | enumerate, and the relation the two output streams must stand in at
-- | every prefix. Beside the paired laws, three single-rig checks:
-- | repetition (deleting a repeated feed changes nothing up to stutter),
-- | answer (one release per feed, untorn — `[]` for the empty array) and
-- | projection's input half at every shape (each operand is fed exactly its
-- | projection of the boundary feeds) — the laws of Data.Profunctor.Row
-- | ("The laws") the merge must preserve or provide.
module Test.Exhaustive (run) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Acting (actedBy)
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
import PUI (PUI(..), acted, silence)
import PUI.Gate (GateInput(..), GateOutput(..), GateState, gateStep, initialGate)
import Unsafe.Coerce (unsafeCoerce)

data Event = Feed | FeedAgain | FeedOnly1 | FeedNone | Fire1 | Fire2 | Fire3

derive instance Eq Event

instance Show Event where
  show = case _ of
    Feed -> "Feed"
    FeedAgain -> "FeedAgain"
    FeedOnly1 -> "FeedOnly1"
    FeedNone -> "FeedNone"
    Fire1 -> "Fire1"
    Fire2 -> "Fire2"
    Fire3 -> "Fire3"

isFeed :: Event -> Boolean
isFeed = case _ of
  Feed -> true
  FeedAgain -> true
  FeedOnly1 -> true
  FeedNone -> true
  _ -> false

-- | The shape of a feed: the whole value, or — for a collection, whose fed
-- | array is its participant set — element 1 alone, or nothing. A merge
-- | rig ignores the shape.
data Shape = Whole | Only1 | None

-- | A term at its boundary: feed it a token in a shape, fire operand K with
-- | a token, read what it emitted so far.
type Rig =
  { feed :: Shape -> Int -> Effect Unit
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
quieter op = pure { p: quieterP op.p, fire: op.fire }

-- The same on a bare component, per instance — so a collection element
-- lifted with it drops the first emission of each of its instances.
quieterP :: forall i o. PUI Effect i o -> PUI Effect i o
quieterP w = PUI do
  inner <- unwrap w
  spoken <- Ref.new false
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> inner.fromUser \o -> do
        already <- Ref.read spoken
        if already then prop o else Ref.write true spoken
    }

-- An operand that records the tokens it is fed: projection's witness.
type Traced i o = { p :: PUI Effect i o, fire :: Int -> Effect Unit, ins :: Ref.Ref (Array Int) }

traced :: forall i o. (i -> Int) -> Op i o -> Effect (Traced i o)
traced token op = do
  ins <- Ref.new []
  let
    p = PUI do
      inner <- unwrap op.p
      pure { toUser: \i -> Ref.modify_ (_ <> [ token i ]) ins *> inner.toUser i, fromUser: inner.fromUser }
  pure { p, fire: op.fire, ins }

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
  pure { feed: \_ n -> m'.toUser (mkFeed n), fires, outs }

-- A rig whose feeds are no-ops: equal streams mean feeds never emit.
deaf :: Effect Rig -> Effect Rig
deaf mk = mk <#> _ { feed = \_ _ -> pure unit }

runScript :: Effect Rig -> Array Event -> Effect (Array (Array String))
runScript mk script = do
  r <- mk
  counter <- Ref.new 0
  lastFed <- Ref.new (Tuple Whole 0)
  snaps <- Ref.new []
  let
    fresh = Ref.modify (_ + 1) counter
    fireAt k n = for_ (Array.index r.fires k) \f -> f n
    feedIn shape = do
      n <- fresh
      Ref.write (Tuple shape n) lastFed
      r.feed shape n
  for_ script \ev -> do
    case ev of
      Feed -> feedIn Whole
      FeedOnly1 -> feedIn Only1
      FeedNone -> feedIn None
      FeedAgain -> Ref.read lastFed >>= \(Tuple shape n) -> r.feed shape n
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
      when (Array.index script i == Just FeedAgain && maybe false isFeed (Array.index script (i - 1))) do
        full <- runScript mk script
        for_ (Array.deleteAt i script) \shorter -> do
          less <- runScript mk shorter
          let
            l = dedupConsecutive (fromMaybeEmpty [] (Array.last full))
            r = dedupConsecutive (fromMaybeEmpty [] (Array.last less))
          unless (l == r) (failing name script l r)
  pure (Array.length scripts)

fromMaybeEmpty :: Array String -> Maybe (Array String) -> Array String
fromMaybeEmpty dflt = case _ of
  Just xs -> xs
  Nothing -> dflt

-- Preservation of the answer law at `×→×` and at the collection: with
-- operands (elements) answering every feed, every feed of the term is
-- answered by exactly one release — which is at least one, and none torn.
checkAnswer :: String -> Array Event -> Int -> Effect Rig -> Effect Int
checkAnswer name alphabet len mk = do
  log ("Exhaustive: " <> name)
  let scripts = scriptsOf alphabet len
  foreachE scripts \script -> do
    snaps <- runScript mk script
    for_ (Array.range 0 (Array.length script - 1)) \i ->
      when (maybe false isFeed (Array.index script i)) do
        let
          before = if i == 0 then [] else fromMaybeEmpty [] (Array.index snaps (i - 1))
          answer = Array.drop (Array.length before) (fromMaybeEmpty [] (Array.index snaps i))
        unless (Array.length answer == 1) (failing name script answer before)
  pure (Array.length scripts)

-- Projection's input half: under every script, each operand's inner feed
-- stream is exactly its projection of the boundary feed stream — the whole
-- stream at a record input, the tokens of its own cases at a variant one.
type Projection =
  { name :: String
  , alphabet :: Array Event
  , len :: Int
  , build :: Effect { rig :: Rig, ins :: Array (Ref.Ref (Array Int)), owns :: Array (Int -> Boolean) }
  }

checkProjection :: Projection -> Effect Int
checkProjection law = do
  log ("Exhaustive: " <> law.name)
  let scripts = scriptsOf law.alphabet law.len
  foreachE scripts \script -> do
    fed <- Ref.new []
    built <- law.build
    let
      r0 = built.rig
      mk = pure (r0 { feed = \shape n -> Ref.modify_ (_ <> [ n ]) fed *> r0.feed shape n })
    _ <- runScript mk script
    feeds <- Ref.read fed
    for_ (Array.zip built.ins built.owns) \(Tuple insRef owns) -> do
      ins <- Ref.read insRef
      let expected = Array.filter owns feeds
      unless (ins == expected) (failing law.name script ins expected)
  pure (Array.length scripts)

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
-- The pure step, enrolled with the two labels and run by hand; a release
-- is assembled into the row the merge would emit.
pureRecordGate :: Effect { step :: GateInput String Int -> Effect Unit, outs :: Ref.Ref (Array String) }
pureRecordGate = do
  st <- Ref.new (initialGate [ "a", "b" ] :: GateState String Int)
  outs <- Ref.new []
  let
    step inp = do
      s <- Ref.read st
      let Tuple s' out = gateStep s inp
      Ref.write s' st
      case out of
        Released [ Tuple _ a, Tuple _ b ] -> Ref.modify_ (_ <> [ show { a, b } ]) outs
        _ -> pure unit
  pure { step, outs }

pureGateRR :: Effect Rig
pureGateRR = do
  g <- pureRecordGate
  pure
    { feed: \_ n -> g.step StepBegun *> g.step (Contributed [ Tuple "a" n ]) *> g.step (Contributed [ Tuple "b" n ]) *> g.step StepEnded
    , fires: [ \n -> g.step (Contributed [ Tuple "a" n ]), \n -> g.step (Contributed [ Tuple "b" n ]) ]
    , outs: g.outs
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
  g <- pureRecordGate
  pure
    { feed: \_ n -> g.step StepBegun *> (if n `mod` 2 == 0 then g.step (Contributed [ Tuple "a" n ]) else g.step (Contributed [ Tuple "b" n ])) *> g.step StepEnded
    , fires: [ \n -> g.step (Contributed [ Tuple "a" n ]), \n -> g.step (Contributed [ Tuple "b" n ]) ]
    , outs: g.outs
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
-- The container action: the record gate at runtime labels. The fed array is
-- the participant set — `Whole` enrols elements 1 and 2, `Only1` element 1,
-- `None` nobody — and each element echoes `{ v: s }` of its feed.
type Item = { k :: Int, s :: Int }

arrayOf :: Shape -> Int -> Array Item
arrayOf shape n = case shape of
  Whole -> [ { k: 1, s: n }, { k: 2, s: n } ]
  Only1 -> [ { k: 1, s: n } ]
  None -> []

-- An echo element, firable per key: each instance enrols its emit leg under
-- the key it is fed, so `fire k` reaches the instance currently holding `k`
-- (a leaver's stale instance still fires — and is no participant).
type Elem = { p :: PUI Effect Item { v :: Int }, fire :: Int -> Int -> Effect Unit }

echoElem :: Effect Elem
echoElem = do
  registry <- Ref.new Map.empty
  let
    p = PUI do
      e <- emitter
      pure
        { toUser: \r -> Ref.modify_ (Map.insert r.k e.emit) registry *> e.emit { v: r.s }
        , fromUser: e.register
        }
  pure { p, fire: \k n -> Ref.read registry >>= \m -> for_ (Map.lookup k m) \emit -> emit { v: n } }

-- `acted @"k"` over the element (wrapped by `wrap`: identity, or the
-- refinement witness), fed arrays by shape, firing elements 1 and 2.
actedRig :: (PUI Effect Item { v :: Int } -> PUI Effect Item { v :: Int }) -> Effect Rig
actedRig wrap = do
  el <- echoElem
  m' <- unwrap (acted @"k" (wrap el.p) :: PUI Effect (Array Item) (Array { k :: Int, v :: Int }))
  outs <- Ref.new []
  m'.fromUser \o -> Ref.modify_ (_ <> [ show o ]) outs
  pure { feed: \shape n -> m'.toUser (arrayOf shape n), fires: [ el.fire 1, el.fire 2 ], outs }

-- The pure step enrolled with nothing and rekeyed by each feed, run by hand
-- the way `actedWith` runs it; a release is the keyed vector.
pureGather :: Effect Rig
pureGather = do
  st <- Ref.new (initialGate [] :: GateState Int Int)
  outs <- Ref.new []
  let
    step inp = do
      s <- Ref.read st
      let Tuple s' out = gateStep s inp
      Ref.write s' st
      case out of
        Released kvs -> Ref.modify_ (_ <> [ show (kvs <#> \(Tuple k v) -> { k, v }) ]) outs
        _ -> pure unit
  pure
    { feed: \shape n -> do
        let items = arrayOf shape n
        step StepBegun
        step (Rekeyed (map _.k items))
        for_ items \r -> step (Contributed [ Tuple r.k r.s ])
        step StepEnded
    , fires: [ \n -> step (Contributed [ Tuple 1 n ]), \n -> step (Contributed [ Tuple 2 n ]) ]
    , outs
    }

-- A term over the fed array itself, no elements to fire: the wire law's rig.
arrayRig :: forall o. Show o => PUI Effect (Array Item) o -> Effect Rig
arrayRig m = do
  m' <- unwrap m
  outs <- Ref.new []
  m'.fromUser \o -> Ref.modify_ (_ <> [ show o ]) outs
  pure { feed: \shape n -> m'.toUser (arrayOf shape n), fires: [], outs }

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

-- the collection's alphabets: every feed shape, so the gate is rekeyed to
-- each reachable participant set
gatherAlphabet :: Array Event
gatherAlphabet = [ Feed, FeedOnly1, FeedNone, Fire1, Fire2 ]

gatherFeeds :: Array Event
gatherFeeds = [ Feed, FeedAgain, FeedOnly1, FeedNone ]

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
  -- the container action: the same gate, its labels the fed keys
  , { name: "acted conformance to PUI.Gate (rekeyed per feed)", alphabet: gatherAlphabet, len: two, rel: Equal
    , left: actedRig identity, right: pureGather }
  , { name: "acted monotonicity", alphabet: gatherAlphabet, len: two, rel: Refines
    , left: actedRig quieterP, right: actedRig identity }
  , { name: "acted wire (actedBy k identity ≈ identity)", alphabet: gatherFeeds, len: two, rel: Equal
    , left: arrayRig (actedBy _.k (identity :: PUI Effect Item Item)), right: arrayRig (identity :: PUI Effect (Array Item) (Array Item)) }
  ]

even :: Int -> Boolean
even n = n `mod` 2 == 0

projections :: Array Projection
projections =
  [ { name: "×→× projection (each operand fed every feed, whole)", alphabet: rrAlphabet, len: two
    , build: do
        a <- rrA >>= traced _.s
        b <- rrB >>= traced _.s
        r <- rig recordFeed (recordToRecord a.p b.p) [ a.fire, b.fire ]
        pure { rig: r, ins: [ a.ins, b.ins ], owns: [ const true, const true ] } }
  , { name: "×→+ projection (each operand fed every feed, whole)", alphabet: evAlphabet, len: two
    , build: do
        a <- rvA >>= traced _.s
        b <- rvB >>= traced _.s
        r <- rig recordFeed (recordToVariant a.p b.p) [ a.fire, b.fire ]
        pure { rig: r, ins: [ a.ins, b.ins ], owns: [ const true, const true ] } }
  , { name: "+→+ projection (each operand fed its own cases only)", alphabet: evAlphabet, len: two
    , build: do
        a <- vvA >>= traced (match { x: identity })
        b <- vvB >>= traced (match { y: identity })
        r <- rig case2 (variantToVariant a.p b.p) [ a.fire, b.fire ]
        pure { rig: r, ins: [ a.ins, b.ins ], owns: [ even, not <<< even ] } }
  , { name: "+→× projection (each operand fed its own cases only)", alphabet: evAlphabet, len: two
    , build: do
        a <- vrA >>= traced (match { x: identity })
        b <- vrB >>= traced (match { y: identity })
        r <- rig case2 (variantToRecord a.p b.p) [ a.fire, b.fire ]
        pure { rig: r, ins: [ a.ins, b.ins ], owns: [ even, not <<< even ] } }
  ]

run :: Effect Unit
run = do
  total <- Ref.new 0
  let count n = Ref.modify_ (_ + n) total
  for_ laws \law -> checkLaw law >>= count
  checkRepetition "×→× repetition (feed-idempotence of the merge)" rrAlphabet two (rrTwo recordToRecord) >>= count
  checkAnswer "×→× answer (one untorn release per feed)" rrAlphabet two (rrTwo recordToRecord) >>= count
  checkRepetition "acted repetition (feed-idempotence of the collection)" [ Feed, FeedAgain, FeedOnly1, Fire1, Fire2 ] two (actedRig identity) >>= count
  checkAnswer "acted answer (one untorn release per feed, [] included)" gatherAlphabet two (actedRig identity) >>= count
  for_ projections \law -> checkProjection law >>= count
  n <- Ref.read total
  log ("Exhaustive: " <> show (Array.length laws + 4 + Array.length projections) <> " laws over " <> show n <> " scripts, no distinguishing script found")
