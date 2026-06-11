module Test.Main where

import Prelude

import Data.Lens (over, set, view)
import Data.Profunctor.Row.RecordToRecord (editProperty, eliminateProperty, focusRecord, introduceProperty)
import Data.Profunctor.Row.VariantToVariant (editCase, eliminateCase, focusVariant)
import Effect (Effect)
import Effect.Exception (throw)

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual msg expected actual =
  when (expected /= actual) $
    void $ throw (msg <> ": expected " <> show expected <> " but got " <> show actual)

main :: Effect Unit
main = do
  -- == StrongRecordToRecord: row-typed Strong, focus a sub-record carrying the rest. On `(->)`. ==

  -- focusRecord: rows on both sides. Here a one-field sub-record { a } is transformed
  -- (Int -> String) while the complement { b } is carried unchanged.
  assertEqual "focusRecord"
    { a: "5", b: true }
    (focusRecord (\(r :: { a :: Int }) -> { a: show r.a }) { a: 5, b: true })

  -- multi-field sub-record { a, c } transformed, complement { b } carried.
  assertEqual "focusRecord/multi-field"
    { a: 50, c: 2, b: "x" }
    (focusRecord (\(r :: { a :: Int, c :: Int }) -> { a: r.a * 10, c: r.c + 1 }) { a: 5, c: 1, b: "x" })

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

  -- == ChoiceVariantToVariant: row-typed Choice, focus a sub-variant carrying the rest. On `(->)`. ==

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
    (focusVariant (over (editCase @"x") (_ + 1) :: [ x :: Int ] -> [ x :: Int ]) (.x 5))

  -- editCase = the value-level single-case prism — over the matching case only.
  assertEqual "editCase/match"
    (.x 10 :: [ x :: Int, y :: String ])
    (over (editCase @"x") (_ * 2) (.x 5))
  assertEqual "editCase/miss"
    (.y "a" :: [ x :: Int, y :: String ])
    (over (editCase @"x") (_ * 2) (.y "a"))

  -- eliminateCase (ChoiceVariantToVariant via `left`): survivors pass through (eliminated case is Void).
  let elim = eliminateCase @"gone" (identity :: Void -> Void) :: [ gone :: Void, keep :: Int ] -> [ keep :: Int ]
  assertEqual "eliminateCase/passthrough"
    (.keep 7 :: [ keep :: Int ])
    (elim (.keep 7))
