module Test.Main where

import Prelude

import Data.Lens (over, set, view)
import Data.Profunctor.HalfOptic.Case (eliminateCase, focusCase)
import Data.Profunctor.HalfOptic.Property (edit, eliminateProperty, introduceProperty)
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
  -- == Product side = Strong. All three combinators run on the `(->)` profunctor. ==

  -- `edit` is the standard field lens (formerly EditPropP) — get / set / over.
  assertEqual "edit/view" 7 (view (edit @"foo") { foo: 7, bar: "x" })
  assertEqual "edit/set" { foo: 9, bar: "x" } (set (edit @"foo") 9 { foo: 7, bar: "x" })
  assertEqual "edit/over" { foo: 14, bar: "x" } (over (edit @"foo") (_ * 2) { foo: 7, bar: "x" })

  -- `introduceProperty` grows the record; the source reads the accumulator (the `p s r` shape).
  assertEqual "introduceProperty"
    { a: 1, b: 101 }
    (introduceProperty @"b" (\r -> r.a + 100) { a: 1 })

  -- `eliminateProperty` is the transpose — it drops a field, consuming its value.
  assertEqual "eliminateProperty"
    { a: 1 }
    (eliminateProperty @"b" (const unit) { a: 1, b: 101 })

  -- The view's identity: introduce-then-eliminate round-trips to the original record
  -- (half-lens = identity-pinned recordToRecord).
  assertEqual "introduce >>> eliminate = id"
    { a: 1 }
    (eliminateProperty @"b" (const unit) (introduceProperty @"b" (\r -> r.a + 100) { a: 1 }))

  -- == Sum side = Choice. `focusCase` and `eliminateCase` fold onto Choice on `(->)`. ==

  -- `focusCase` is the Choice prism: modify the matching case, leave others alone.
  assertEqual "focusCase/match"
    (inj (Proxy @"x") 10 :: Variant (x :: Int, y :: String))
    (over (focusCase @"x") (_ * 2) (inj (Proxy @"x") 5))
  assertEqual "focusCase/miss"
    (inj (Proxy @"y") "a" :: Variant (x :: Int, y :: String))
    (over (focusCase @"x") (_ * 2) (inj (Proxy @"y") "a"))

  -- `eliminateCase` (Choice via `left`): the survivors pass through (the eliminated case
  -- here is `Void`, so only the passthrough branch is inhabited on `(->)`).
  let elim = eliminateCase @"gone" (identity :: Void -> Void) :: Variant (gone :: Void, keep :: Int) -> Variant (keep :: Int)
  assertEqual "eliminateCase/passthrough"
    (inj (Proxy @"keep") 7 :: Variant (keep :: Int))
    (elim (inj (Proxy @"keep") 7))
