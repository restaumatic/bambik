module Test.Main where

import Prelude

import Data.Lens (over, set, view)
import Data.Profunctor.RowToRow.Case (editCase, eliminateCase)
import Data.Profunctor.RowToRow.Property (editProperty, eliminateProperty, introduceProperty)
import Data.Profunctor.RowToRow.RowChoice (focusCase)
import Data.Profunctor.RowToRow.RowStrong (focusField)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Exception (throw)
import Type.Proxy (Proxy(..))

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual msg expected actual =
  when (expected /= actual) $
    void $ throw (msg <> ": expected " <> show expected <> " but got " <> show actual)

main :: Effect Unit
main = do
  -- == RowStrong: row-typed Strong (focus a field). Runs on `(->)`. ==

  -- editProperty = focusField specialized type-preserving — get / set / over.
  assertEqual "editProperty/view" 7 (view (editProperty @"foo") { foo: 7, bar: "x" })
  assertEqual "editProperty/set" { foo: 9, bar: "x" } (set (editProperty @"foo") 9 { foo: 7, bar: "x" })
  assertEqual "editProperty/over" { foo: 14, bar: "x" } (over (editProperty @"foo") (_ * 2) { foo: 7, bar: "x" })

  -- focusField, type-changing: the field's type changes a -> b, the rest is carried.
  assertEqual "focusField/type-changing"
    { a: "5", b: true }
    (focusField (Proxy :: Proxy "a") show { a: 5, b: true })

  -- introduceProperty grows the record; the source reads the accumulator (the `p s r` shape).
  assertEqual "introduceProperty"
    { a: 1, b: 101 }
    (introduceProperty @"b" (\r -> r.a + 100) { a: 1 })

  -- eliminateProperty is the transpose — it drops a field, consuming its value.
  assertEqual "eliminateProperty"
    { a: 1 }
    (eliminateProperty @"b" (const unit) { a: 1, b: 101 })

  -- introduce-then-eliminate round-trips (half-optic = identity-pinned recordToRecord).
  assertEqual "introduce >>> eliminate = id"
    { a: 1 }
    (eliminateProperty @"b" (const unit) (introduceProperty @"b" (\r -> r.a + 100) { a: 1 }))

  -- == RowChoice: row-typed Choice (focus a case). Runs on `(->)`. ==

  -- editCase = focusCase specialized type-preserving — over the matching case only.
  assertEqual "editCase/match"
    (inj (Proxy @"x") 10 :: Variant (x :: Int, y :: String))
    (over (editCase @"x") (_ * 2) (inj (Proxy @"x") 5))
  assertEqual "editCase/miss"
    (inj (Proxy @"y") "a" :: Variant (x :: Int, y :: String))
    (over (editCase @"x") (_ * 2) (inj (Proxy @"y") "a"))

  -- focusCase, type-changing: the case payload changes a -> b, other cases carried.
  assertEqual "focusCase/type-changing"
    (inj (Proxy @"x") "5" :: Variant (x :: String, y :: Boolean))
    (focusCase (Proxy :: Proxy "x") show (inj (Proxy @"x") 5 :: Variant (x :: Int, y :: Boolean)))

  -- eliminateCase (RowChoice via `left`): survivors pass through (eliminated case is Void).
  let elim = eliminateCase @"gone" (identity :: Void -> Void) :: Variant (gone :: Void, keep :: Int) -> Variant (keep :: Int)
  assertEqual "eliminateCase/passthrough"
    (inj (Proxy @"keep") 7 :: Variant (keep :: Int))
    (elim (inj (Proxy @"keep") 7))
