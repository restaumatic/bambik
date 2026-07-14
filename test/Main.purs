module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (over, set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Cochoice (unleft)
import Data.Profunctor.Costrong (unfirst)
import Data.Profunctor.Row.RecordToRecord (property, focusRecord, recordToRecord)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord (coretain, variantToRecord)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.RecordToVariant (coresolve, recordToCase)
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import UI (New(..), PropagationStatus, UI(..), looped)

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual msg expected actual =
  when (expected /= actual) $
    void $ throw (msg <> ": expected " <> show expected <> " but got " <> show actual)

-- A UI Effect operand whose user-output leg the test fires by hand: it ignores
-- toUser and stores the callback the merge registers via fromUser.
probe :: forall i o. Ref.Ref (Maybe (New o -> Effect PropagationStatus)) -> UI Effect i o
probe propRef = UI $ pure
  { toUser: \_ -> pure unit
  , fromUser: \prop -> Ref.write (Just prop) propRef
  }

fire :: forall o. Ref.Ref (Maybe (New o -> Effect PropagationStatus)) -> o -> Effect Unit
fire propRef o = do
  mProp <- Ref.read propRef
  for_ mProp \prop -> void $ prop (New o false)

-- A probe that additionally records what its user-input leg receives.
probeIO :: forall i o. Ref.Ref (Array i) -> Ref.Ref (Maybe (New o -> Effect PropagationStatus)) -> UI Effect i o
probeIO insRef propRef = UI $ pure
  { toUser: \(New i _) -> Ref.modify_ (_ <> [ i ]) insRef
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
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp { a: 1 }
    Ref.read outs >>= assertEqual "unit law ×→×: recordToRecord pempty g = g" [ { a: 1 } ]

  -- and on the right: recordToRecord g pempty = g.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (recordToRecord (probe gProp :: UI Effect {} { a :: Int }) RecordToRecord.pempty)
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    fire gProp { a: 2 }
    Ref.read outs >>= assertEqual "unit law ×→×: recordToRecord g pempty = g" [ { a: 2 } ]

  -- +→× unit law: variantToRecord pempty g = g.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (variantToRecord VariantToRecord.pempty (probe gProp :: UI Effect [ x :: Unit ] { a :: Int }))
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
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
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
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
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (New 1 false)
    Ref.read ins >>= assertEqual "unfirst: gated before state exists" []
    fire gProp (Tuple "x" true)
    Ref.read outs >>= assertEqual "unfirst: value leg passes" [ "x" ]
    m.toUser (New 2 false)
    Ref.read ins >>= assertEqual "unfirst: input paired with retained state" [ Tuple 2 true ]

  -- Cochoice/unleft (+-diagonal trace, iteration): looped-branch emissions
  -- re-enter, exit-branch emissions pass.
  do
    ins <- Ref.new ([] :: Array (Either Int Int))
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (unleft (probeIO ins gProp :: UI Effect (Either Int Int) (Either String Int)))
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (New 5 false)
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
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (New 1 false)
    Ref.read ins >>= assertEqual "coresolve: gated before state" []
    fire gProp (Right true)
    Ref.read outs >>= assertEqual "coresolve: folded state withheld" []
    m.toUser (New 2 false)
    Ref.read ins >>= assertEqual "coresolve: input paired with folded state" [ Tuple 2 true ]
    fire gProp (Left "done")
    Ref.read outs >>= assertEqual "coresolve: exit passes" [ "done" ]

  -- Coretaining/coretain (productive unfold): every emission yields its
  -- value and immediately resumes the widget with the new state.
  do
    ins <- Ref.new ([] :: Array (Either Int Boolean))
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coretain (probeIO ins gProp :: UI Effect (Either Int Boolean) (Tuple String Boolean)))
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (New 1 false)
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
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (New 5 false)
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
    m.fromUser \(New o _) -> Ref.modify_ (_ <> [ o ]) outs $> Nothing
    m.toUser (New (.again 1) false)
    fire gProp (.again 2)
    Ref.read outs >>= assertEqual "iterate: again loops silently" []
    Ref.read ins >>= assertEqual "iterate: again re-enters" [ .again 1, .again 2 ]
    fire gProp (.done "d")
    Ref.read outs >>= assertEqual "iterate: done exits" [ .done "d" ]
