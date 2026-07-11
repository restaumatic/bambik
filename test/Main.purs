module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.Lens (over, set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Row.RecordToRecord (property, eliminateProperty, focusRecord, recordToProperty, recordToRecord)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord (variantToRecord)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant (case_, caseToVariant, focusVariant)
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import UI (New(..), PropagationStatus, UI(..))

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

  -- recordToProperty grows the record; the source reads the accumulator (the `p s r` shape).
  assertEqual "recordToProperty"
    { a: 1, b: 101 }
    (recordToProperty @"b" (\r -> r.a + 100) { a: 1 })

  -- eliminateProperty is the transpose — it drops a field, consuming its value.
  assertEqual "eliminateProperty"
    { a: 1 }
    (eliminateProperty @"b" (const unit) { a: 1, b: 101 })

  -- introduce-then-eliminate round-trips (focus = identity-pinned merge).
  assertEqual "introduce >>> eliminate = id"
    { a: 1 }
    (eliminateProperty @"b" (const unit) (recordToProperty @"b" (\r -> r.a + 100) { a: 1 }))

  -- == focusVariant: row-typed Choice, focus a sub-variant carrying the rest. On `(->)`. ==

  -- focusVariant: dispatch on the sub-variant { x }, carry the complement { y }.
  assertEqual "focusVariant/sub-case carried"
    (.x 5 :: [ x :: Int, y :: String ])
    (focusVariant (identity :: [ x :: Int ] -> [ x :: Int ]) (.x 5))
  assertEqual "focusVariant/rest-case carried"
    (.y "a" :: [ x :: Int, y :: String ])
    (focusVariant (identity :: [ x :: Int ] -> [ x :: Int ]) (.y "a"))

  -- transforming the focused sub-case (not identity), complement carried.
  assertEqual "focusVariant/transform sub-case"
    (.x 6 :: [ x :: Int, y :: String ])
    (focusVariant (over (case_ @"x") (_ + 1) :: [ x :: Int ] -> [ x :: Int ]) (.x 5))

  -- case_ = the value-level single-case prism — over the matching case only.
  assertEqual "case_/match"
    (.x 10 :: [ x :: Int, y :: String ])
    (over (case_ @"x") (_ * 2) (.x 5))
  assertEqual "case_/miss"
    (.y "a" :: [ x :: Int, y :: String ])
    (over (case_ @"x") (_ * 2) (.y "a"))

  -- caseToVariant = accept an extra input case and dispatch it into the remaining cases
  -- (dual of recordToProperty); every other case passes through.
  let adapt = caseToVariant @"legacyInc" (\(_ :: String) -> .inc 100) :: [ legacyInc :: String, inc :: Int, reset :: Unit ] -> [ inc :: Int, reset :: Unit ]
  assertEqual "caseToVariant/adapted"
    (.inc 100 :: [ inc :: Int, reset :: Unit ])
    (adapt (.legacyInc "bump"))
  assertEqual "caseToVariant/direct passthrough"
    (.inc 5 :: [ inc :: Int, reset :: Unit ])
    (adapt (.inc 5))
  assertEqual "caseToVariant/other passthrough"
    (.reset unit :: [ inc :: Int, reset :: Unit ])
    (adapt (.reset unit))

  -- case elimination = caseToVariant with the dispatch pinned unreachable (Void).
  let elim = caseToVariant @"gone" (absurd :: Void -> [ keep :: Int ]) :: [ gone :: Void, keep :: Int ] -> [ keep :: Int ]
  assertEqual "caseToVariant/eliminate passthrough"
    (.keep 7 :: [ keep :: Int ])
    (elim (.keep 7))

  -- recordToCase (x -> +): whole record computes a value, emitted unconditionally
  -- as case l — the introduce-family member Choice can't have, free on any Profunctor.
  assertEqual "recordToCase"
    (.total 8 :: [ total :: Int, other :: String ])
    (recordToCase @"total" (\r -> r.a + r.b) { a: 3, b: 5 })

  -- == Merge unit laws on the UI carrier: each merge class carries its own ==
  -- == nullary operator `pempty`. At record outputs the unit *announces* its ==
  -- == informationless {} (the parametric silent `mempty` couldn't), so the ==
  -- == merge gates never starve against it; at variant outputs it coincides ==
  -- == with `mempty`. ==

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
