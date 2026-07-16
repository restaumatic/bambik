module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (over, set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Cochoice (unleft)
import Data.Profunctor.Costrong (unfirst)
import PUI.Data.Profunctor.Row.RecordToRecord (colens, completed, feedback, property, focusRecord, recordToRecord)
import PUI.Data.Profunctor.Row.RecordToRecord as RecordToRecord
import PUI.Data.Profunctor.Row.VariantToRecord (coreel, coretain, unfolding, variantToRecord)
import PUI.Data.Profunctor.Row.VariantToRecord as VariantToRecord
import PUI.Data.Profunctor.Row.RecordToVariant (coresolve, coshutter, folding, recordToCase)
import PUI.Data.Profunctor.Row.VariantToVariant (coprism, iterate)
import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import PUI (PropagationStatus, UI(..), looped, resolveFor, updates, with)
import Unsafe.Coerce (unsafeCoerce)

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual msg expected actual =
  when (expected /= actual) $
    void $ throw (msg <> ": expected " <> show expected <> " but got " <> show actual)

-- A UI Effect operand whose user-output leg the test fires by hand: it ignores
-- toUser and stores the callback the merge registers via fromUser.
probe :: forall i o. Ref.Ref (Maybe (o -> Effect PropagationStatus)) -> UI Effect i o
probe propRef = UI $ pure
  { toUser: \_ -> pure unit
  , fromUser: \prop -> Ref.write (Just prop) propRef
  }

fire :: forall o. Ref.Ref (Maybe (o -> Effect PropagationStatus)) -> o -> Effect Unit
fire propRef o = do
  mProp <- Ref.read propRef
  for_ mProp \prop -> void $ prop o

-- A probe that additionally records what its user-input leg receives.
probeIO :: forall i o. Ref.Ref (Array i) -> Ref.Ref (Maybe (o -> Effect PropagationStatus)) -> UI Effect i o
probeIO insRef propRef = UI $ pure
  { toUser: \i -> Ref.modify_ (_ <> [ i ]) insRef
  , fromUser: \prop -> Ref.write (Just prop) propRef
  }

main :: Effect Unit
main = do
  -- == focusRecord: row-typed Strong, focus a sub-record carrying the rest. On `(->)`. ==

  -- focusRecord: rows on both sides. Here a one-field sub-record { a } is transformed
  -- (Int -> String) while the complement { b } is carried unchanged.
  assertEqual "focusRecord"
    { a: "5", b: true }
    (focusRecord (\(r :: { a :: Int }) -> { a: show r.a }) { a: 5, b: true })

  -- multi-field sub-record { a, c } transformed, complement { b } carried.
  assertEqual "focusRecord/multi-field"
    { a: 50, c: 2, b: "x" }
    (focusRecord (\(r :: { a :: Int, c :: Int }) -> { a: r.a * 10, c: r.c + 1 }) { a: 5, c: 1, b: "x" })

  -- property = the value-level single-field lens — get / set / over.
  assertEqual "property/view" 7 (view (property @"foo") { foo: 7, bar: "x" })
  assertEqual "property/set" { foo: 9, bar: "x" } (set (property @"foo") 9 { foo: 7, bar: "x" })
  assertEqual "property/over" { foo: 14, bar: "x" } (over (property @"foo") (_ * 2) { foo: 7, bar: "x" })

  -- recordToCase (x -> +): whole record computes a value, emitted unconditionally
  -- as case l — the introduce-family member Choice can't have, free on any Profunctor.
  assertEqual "recordToCase"
    (.total 8 :: [ total :: Int, other :: String ])
    (recordToCase @"total" (\r -> r.a + r.b) { a: 3, b: 5 })

  -- == Merge unit laws on the UI carrier: each merge class carries its own ==
  -- == nullary operator `pempty`. At record outputs the unit *announces* its ==
  -- == informationless {} (the parametric `silence` couldn't), so the merge ==
  -- == gates never starve against it; at variant outputs it coincides with ==
  -- == `silence`. ==

  -- ×→× unit law: recordToRecord pempty g = g (the output leg — g's emissions
  -- must pass through undisturbed, not starve against the unit).
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (recordToRecord RecordToRecord.pempty (probe gProp :: UI Effect {} { a :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp { a: 1 }
    Ref.read outs >>= assertEqual "unit law ×→×: recordToRecord pempty g = g" [ { a: 1 } ]

  -- and on the right: recordToRecord g pempty = g.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (recordToRecord (probe gProp :: UI Effect {} { a :: Int }) RecordToRecord.pempty)
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp { a: 2 }
    Ref.read outs >>= assertEqual "unit law ×→×: recordToRecord g pempty = g" [ { a: 2 } ]

  -- ×→× runtime-exactness: the merge widens each operand's input by coercion,
  -- so an operand that echoes or lens-rebuilds its input emits an object
  -- runtime-carrying stale copies of *sibling* fields while typed at its own
  -- narrow row. The gate must trim to the declared output row, so the stale
  -- copy cannot shadow the sibling's genuine contribution in the left-biased
  -- union. (The fat emission below is exactly what `widenRecordInput`'s
  -- coercion hands an echo wire.)
  do
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int, b :: String })
    m <- unwrap (recordToRecord
      (probe p1Prop :: UI Effect {} { a :: Int })
      (probe p2Prop :: UI Effect {} { b :: String }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire p2Prop { b: "fresh" }
    fire p1Prop (unsafeCoerce { a: 1, b: "stale" } :: { a :: Int })
    Ref.read outs >>= assertEqual "×→× exactness: stale runtime sibling must not shadow" [ { a: 1, b: "fresh" } ]

  -- +→× runtime-exactness: same guarantee on the other gated merge.
  do
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int, b :: String })
    m <- unwrap (variantToRecord
      (probe p1Prop :: UI Effect [ x :: Unit ] { a :: Int })
      (probe p2Prop :: UI Effect [ y :: Unit ] { b :: String }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire p2Prop { b: "fresh" }
    fire p1Prop (unsafeCoerce { a: 4, b: "stale" } :: { a :: Int })
    Ref.read outs >>= assertEqual "+→× exactness: stale runtime sibling must not shadow" [ { a: 4, b: "fresh" } ]

  -- +→× unit law: variantToRecord pempty g = g.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (variantToRecord VariantToRecord.pempty (probe gProp :: UI Effect [ x :: Unit ] { a :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp { a: 3 }
    Ref.read outs >>= assertEqual "unit law +→×: variantToRecord pempty g = g" [ { a: 3 } ]

  -- +→× knowledge-gating: with two non-empty operands, nothing propagates
  -- until every field of the merged record is known; then each emission
  -- propagates the complete record.
  do
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int, b :: String })
    m <- unwrap (variantToRecord
      (probe p1Prop :: UI Effect [ x :: Unit ] { a :: Int })
      (probe p2Prop :: UI Effect [ y :: Unit ] { b :: String }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire p1Prop { a: 1 }
    Ref.read outs >>= assertEqual "+→× gating: incomplete record withheld" []
    fire p2Prop { b: "s" }
    Ref.read outs >>= assertEqual "+→× gating: completed record propagates" [ { a: 1, b: "s" } ]
    fire p1Prop { a: 2 }
    Ref.read outs >>= assertEqual "+→× gating: later emissions merge with retained side" [ { a: 1, b: "s" }, { a: 2, b: "s" } ]

  -- == The trace quartet: each co-strength ties the knot its strength adds. ==

  -- Costrong/unfirst (×-diagonal trace, state): gated until a first state
  -- emission arrives; thereafter inputs are paired with the retained state.
  do
    ins <- Ref.new ([] :: Array (Tuple Int Boolean))
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (unfirst (probeIO ins gProp :: UI Effect (Tuple Int Boolean) (Tuple String Boolean)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 1
    Ref.read ins >>= assertEqual "unfirst: gated before state exists" []
    fire gProp (Tuple "x" true)
    Ref.read outs >>= assertEqual "unfirst: value leg passes" [ "x" ]
    m.toUser 2
    Ref.read ins >>= assertEqual "unfirst: input paired with retained state" [ Tuple 2 true ]

  -- Cochoice/unleft (+-diagonal trace, iteration): looped-branch emissions
  -- re-enter, exit-branch emissions pass.
  do
    ins <- Ref.new ([] :: Array (Either Int Int))
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (unleft (probeIO ins gProp :: UI Effect (Either Int Int) (Either String Int)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 5
    Ref.read ins >>= assertEqual "unleft: input enters Left" [ Left 5 ]
    fire gProp (Right 7)
    Ref.read outs >>= assertEqual "unleft: looped branch withheld" []
    Ref.read ins >>= assertEqual "unleft: looped branch re-enters" [ Left 5, Right 7 ]
    fire gProp (Left "done")
    Ref.read outs >>= assertEqual "unleft: exit branch passes" [ "done" ]

  -- Coresolving/coresolve (terminating fold): the looped branch accumulates
  -- state silently, the exit branch passes; inputs gated until a first state.
  do
    ins <- Ref.new ([] :: Array (Tuple Int Boolean))
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coresolve (probeIO ins gProp :: UI Effect (Tuple Int Boolean) (Either String Boolean)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 1
    Ref.read ins >>= assertEqual "coresolve: gated before state" []
    fire gProp (Right true)
    Ref.read outs >>= assertEqual "coresolve: folded state withheld" []
    Ref.read ins >>= assertEqual "coresolve: fold step re-fed eagerly" [ Tuple 1 true ]
    m.toUser 2
    Ref.read ins >>= assertEqual "coresolve: input paired with folded state" [ Tuple 1 true, Tuple 2 true ]
    fire gProp (Left "done")
    Ref.read outs >>= assertEqual "coresolve: exit passes" [ "done" ]

  -- Coretaining/coretain (productive unfold): every emission yields its
  -- value and immediately resumes the widget with the new state.
  do
    ins <- Ref.new ([] :: Array (Either Int Boolean))
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coretain (probeIO ins gProp :: UI Effect (Either Int Boolean) (Tuple String Boolean)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 1
    Ref.read ins >>= assertEqual "coretain: input enters fresh" [ Left 1 ]
    fire gProp (Tuple "out" true)
    Ref.read outs >>= assertEqual "coretain: value leg passes" [ "out" ]
    Ref.read ins >>= assertEqual "coretain: state resumes the widget" [ Left 1, Right true ]

  -- looped (the ×-diagonal self-trace): every emission is re-fed (guarded)
  -- and propagated.
  do
    ins <- Ref.new ([] :: Array Int)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array Int)
    m <- unwrap (looped (probeIO ins gProp :: UI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 5
    Ref.read ins >>= assertEqual "looped: input feeds through" [ 5 ]
    fire gProp 7
    Ref.read ins >>= assertEqual "looped: emission re-fed" [ 5, 7 ]
    Ref.read outs >>= assertEqual "looped: emission propagates" [ 7 ]

  -- iterate (the +-trace at row granularity): `again` cases loop back,
  -- `done` cases exit.
  do
    ins <- Ref.new ([] :: Array [ again :: Int ])
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ done :: String ])
    m <- unwrap (iterate (probeIO ins gProp :: UI Effect [ again :: Int ] [ done :: String, again :: Int ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (.again 1)
    fire gProp (.again 2)
    Ref.read outs >>= assertEqual "iterate: again loops silently" []
    Ref.read ins >>= assertEqual "iterate: again re-enters" [ .again 1, .again 2 ]
    fire gProp (.done "d")
    Ref.read outs >>= assertEqual "iterate: done exits" [ .done "d" ]

  -- == The co-strengths' row forms: labeled channels for each trace. ==

  -- feedback (×-trace at row granularity): the state sub-record loops from
  -- output to input, primed by the widget's first emission.
  do
    ins <- Ref.new ([] :: Array { a :: Int, acc :: Int })
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { o :: Int })
    m <- unwrap (feedback (probeIO ins gProp :: UI Effect { a :: Int, acc :: Int } { o :: Int, acc :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser { a: 1 }
    Ref.read ins >>= assertEqual "feedback: gated before state" []
    fire gProp { o: 10, acc: 100 }
    Ref.read outs >>= assertEqual "feedback: value fields pass" [ { o: 10 } ]
    m.toUser { a: 2 }
    Ref.read ins >>= assertEqual "feedback: input joined with looped state" [ { a: 2, acc: 100 } ]

  -- folding @w (terminating fold at row granularity): case w continues the
  -- fold silently, done cases exit.
  do
    ins <- Ref.new ([] :: Array { a :: Int, acc :: Int })
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ done :: String ])
    m <- unwrap (folding @"fold" (probeIO ins gProp :: UI Effect { a :: Int, acc :: Int } [ done :: String, fold :: { acc :: Int } ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser { a: 1 }
    Ref.read ins >>= assertEqual "folding: gated before state" []
    fire gProp (.fold { acc: 5 })
    Ref.read outs >>= assertEqual "folding: fold case withheld" []
    Ref.read ins >>= assertEqual "folding: fold step re-fed eagerly" [ { a: 1, acc: 5 } ]
    m.toUser { a: 2 }
    Ref.read ins >>= assertEqual "folding: input joined with folded state" [ { a: 1, acc: 5 }, { a: 2, acc: 5 } ]
    fire gProp (.done "d")
    Ref.read outs >>= assertEqual "folding: done exits" [ .done "d" ]

  -- unfolding @w (productive unfold at row granularity): value fields pass,
  -- state fields resume the widget as case w.
  do
    ins <- Ref.new ([] :: Array [ start :: Int, resume :: { acc :: Int } ])
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { o :: String })
    m <- unwrap (unfolding @"resume" (probeIO ins gProp :: UI Effect [ start :: Int, resume :: { acc :: Int } ] { o :: String, acc :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (.start 1)
    Ref.read ins >>= assertEqual "unfolding: fresh input enters" [ .start 1 ]
    fire gProp { o: "x", acc: 7 }
    Ref.read outs >>= assertEqual "unfolding: value fields pass" [ { o: "x" } ]
    Ref.read ins >>= assertEqual "unfolding: state resumes as its case" [ .start 1, .resume { acc: 7 } ]

  -- Resolving/resolveFor (the quiescence step): every emission loops
  -- immediately (Right, gated on a first state), and the last emission of a
  -- burst resolves (Left) once the widget stays quiet for the window —
  -- transiency derived from time, no wire-level flag.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array (Either String Int))
    m <- unwrap (resolveFor (Milliseconds 40.0) (probe gProp :: UI Effect Int String))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp "burst1"
    Ref.read outs >>= assertEqual "resolveFor: loop withheld before state" []
    m.toUser (Tuple 1 7)
    fire gProp "burst2"
    Ref.read outs >>= assertEqual "resolveFor: emission loops immediately" [ Right 7 ]
    launchAff_ do
      delay (Milliseconds 100.0)
      liftEffect do
        Ref.read outs >>= assertEqual "resolveFor: only the last burst value resolves, after quiescence"
          [ Right 7, Left "burst2" ]

  -- == The co-optics: each co-strength induces the reversed optic. ==

  -- Colens (unfirst; ≅ Lens b a t s): each input read against the last
  -- output, outputs mapped; gated before a first emission.
  do
    ins <- Ref.new ([] :: Array String)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array Int)
    m <- unwrap (colens (\s b -> s <> show b) (_ * 10) (probeIO ins gProp :: UI Effect String Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser "x"
    Ref.read ins >>= assertEqual "colens: gated before first emission" []
    fire gProp 3
    Ref.read outs >>= assertEqual "colens: output mapped" [ 30 ]
    m.toUser "y"
    Ref.read ins >>= assertEqual "colens: input joined with last output" [ "y3" ]

  -- Coprism (unleft; ≅ Prism b a t s): every input embeds as a focus; each
  -- result exits or re-enters as the next focus — tailRec at the optic level.
  do
    ins <- Ref.new ([] :: Array Int)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coprism identity (\b -> if b > 10 then Left (show b) else Right (b + 1)) (probeIO ins gProp :: UI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 3
    Ref.read ins >>= assertEqual "coprism: input embeds as focus" [ 3 ]
    fire gProp 5
    Ref.read outs >>= assertEqual "coprism: looped result withheld" []
    Ref.read ins >>= assertEqual "coprism: looped result re-enters" [ 3, 6 ]
    fire gProp 11
    Ref.read outs >>= assertEqual "coprism: exit passes" [ "11" ]

  -- Coshutter (coresolve; ≅ Reel b a t s): the fold state is a reader —
  -- each emission exits or yields a new way to read inputs; gated until a
  -- first reader exists.
  do
    ins <- Ref.new ([] :: Array Int)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coshutter (\b -> if b >= 100 then Left (show b) else Right (_ + b)) (probeIO ins gProp :: UI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 1
    Ref.read ins >>= assertEqual "coshutter: gated before a first reader" []
    fire gProp 10
    Ref.read ins >>= assertEqual "coshutter: new reader re-reads last input" [ 11 ]
    m.toUser 2
    Ref.read ins >>= assertEqual "coshutter: input read through the fold reader" [ 11, 12 ]
    fire gProp 100
    Ref.read outs >>= assertEqual "coshutter: exit passes" [ "100" ]

  -- Coreel (coretain; ≅ Shutter b a t s): every emission both leaves and
  -- re-enters — a generator.
  do
    ins <- Ref.new ([] :: Array Int)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coreel identity show (_ + 1) (probeIO ins gProp :: UI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser 5
    Ref.read ins >>= assertEqual "coreel: input embeds" [ 5 ]
    fire gProp 7
    Ref.read outs >>= assertEqual "coreel: emission leaves" [ "7" ]
    Ref.read ins >>= assertEqual "coreel: emission resumes as next input" [ 5, 8 ]

  -- updates (the Mealy update stage): each event emission is paired with
  -- the retained model and folded; gated before a first model arrives.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { n :: Int })
    m <- unwrap (updates (\e s -> { n: s.n + e }) (probe gProp :: UI Effect { n :: Int } Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp 3
    Ref.read outs >>= assertEqual "updates: gated before a model" []
    m.toUser { n: 10 }
    Ref.read outs >>= assertEqual "updates: value passes through" [ { n: 10 } ]
    fire gProp 3
    Ref.read outs >>= assertEqual "updates: event folded into retained model" [ { n: 10 }, { n: 13 } ]

  -- completed (output completion): fields the widget doesn't produce are
  -- carried from the retained input; emissions are trimmed first, so a fat
  -- runtime emission cannot shadow carried fields.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int, b :: String })
    m <- unwrap (completed (probe gProp :: UI Effect { a :: Int, b :: String } { a :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp { a: 1 }
    Ref.read outs >>= assertEqual "completed: gated before input" []
    m.toUser { a: 0, b: "kept" }
    fire gProp { a: 9 }
    Ref.read outs >>= assertEqual "completed: emission over carried input" [ { a: 9, b: "kept" } ]
    fire gProp (unsafeCoerce { a: 7, b: "stale" } :: { a :: Int })
    Ref.read outs >>= assertEqual "completed: fat emission trimmed, carried field kept"
      [ { a: 9, b: "kept" }, { a: 7, b: "kept" } ]

  -- with (seeded's composition closure): the wrapped stage receives the
  -- seed at registration, then inputs pass through
  do
    ins <- Ref.new ([] :: Array { n :: Int })
    gProp <- Ref.new Nothing
    m <- unwrap (with { n: 1 } (probeIO ins gProp :: UI Effect { n :: Int } { n :: Int }))
    m.fromUser \_ -> pure Nothing
    Ref.read ins >>= assertEqual "with: seed fed at registration" [ { n: 1 } ]
    m.toUser { n: 2 }
    Ref.read ins >>= assertEqual "with: inputs pass through" [ { n: 1 }, { n: 2 } ]
