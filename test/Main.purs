module Test.Main where

import Prelude

import Data.Lens (over, set, view)
import Data.Profunctor.RowToRow.Case (editCase, eliminateCase)
import Data.Profunctor.RowToRow.Property (editProperty, eliminateProperty, introduceProperty)
import Data.Profunctor.RowToRow.RowChoice (focusVariant)
import Data.Profunctor.RowToRow.RowStrong (focusRecord)
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
  -- == RowStrong: row-typed Strong, focus a sub-record carrying the rest. On `(->)`. ==

  -- focusRecord: rows on both sides. Here a one-field sub-record { a } is transformed
  -- (Int -> String) while the complement { b } is carried unchanged.
  assertEqual "focusRecord"
    { a: "5", b: true }
    (focusRecord (\(r :: { a :: Int }) -> { a: show r.a }) { a: 5, b: true })

  -- editProperty = the value-level single-field lens — get / set / over.
  assertEqual "editProperty/view" 7 (view (editProperty @"foo") { foo: 7, bar: "x" })
  assertEqual "editProperty/set" { foo: 9, bar: "x" } (set (editProperty @"foo") 9 { foo: 7, bar: "x" })
  assertEqual "editProperty/over" { foo: 14, bar: "x" } (over (editProperty @"foo") (_ * 2) { foo: 7, bar: "x" })

  -- introduceProperty grows the record; the source reads the accumulator (the `p s r` shape).
  assertEqual "introduceProperty"
    { a: 1, b: 101 }
    (introduceProperty @"b" (\r -> r.a + 100) { a: 1 })

  -- eliminateProperty is the transpose — it drops a field, consuming its value.
  assertEqual "eliminateProperty"
    { a: 1 }
    (eliminateProperty @"b" (const unit) { a: 1, b: 101 })

  -- introduce-then-eliminate round-trips (focus = identity-pinned merge).
  assertEqual "introduce >>> eliminate = id"
    { a: 1 }
    (eliminateProperty @"b" (const unit) (introduceProperty @"b" (\r -> r.a + 100) { a: 1 }))

  -- == RowChoice: row-typed Choice, focus a sub-variant carrying the rest. On `(->)`. ==

  -- focusVariant: dispatch on the sub-variant { x }, carry the complement { y }.
  assertEqual "focusVariant/sub-case carried"
    (inj (Proxy @"x") 5 :: Variant (x :: Int, y :: String))
    (focusVariant (identity :: Variant (x :: Int) -> Variant (x :: Int)) (inj (Proxy @"x") 5))
  assertEqual "focusVariant/rest-case carried"
    (inj (Proxy @"y") "a" :: Variant (x :: Int, y :: String))
    (focusVariant (identity :: Variant (x :: Int) -> Variant (x :: Int)) (inj (Proxy @"y") "a"))

  -- editCase = the value-level single-case prism — over the matching case only.
  assertEqual "editCase/match"
    (inj (Proxy @"x") 10 :: Variant (x :: Int, y :: String))
    (over (editCase @"x") (_ * 2) (inj (Proxy @"x") 5))
  assertEqual "editCase/miss"
    (inj (Proxy @"y") "a" :: Variant (x :: Int, y :: String))
    (over (editCase @"x") (_ * 2) (inj (Proxy @"y") "a"))

  -- eliminateCase (RowChoice via `left`): survivors pass through (eliminated case is Void).
  let elim = eliminateCase @"gone" (identity :: Void -> Void) :: Variant (gone :: Void, keep :: Int) -> Variant (keep :: Int)
  assertEqual "eliminateCase/passthrough"
    (inj (Proxy @"keep") 7 :: Variant (keep :: Int))
    (elim (inj (Proxy @"keep") 7))
