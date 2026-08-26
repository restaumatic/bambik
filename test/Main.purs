module Test.Main where

import Prelude

import Data.Array (length, (!!))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (over, set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Choice (left, right)
import Data.Profunctor.Cochoice (unleft, unright)
import Data.Profunctor.Costrong (unfirst)
import Data.Profunctor.Strong (first)
import Data.Lens.Colens (colens)
import Data.Lens.Coprism (coprism)
import Data.Lens.Coreel (coreel)
import Data.Lens.Coshutter (coshutter)
import Data.Profunctor.Coresolving (coresolve)
import Data.Profunctor.Coretaining (coretain)
import Data.Profunctor.Row.RecordToRecord (feedback, field, muted, subStrong, recordToRecord)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord (unfolding, variantToRecord)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.RecordToVariant (folding, recordToCase, recordToVariant, toCase)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant (focusCase, iterate, variantToVariant)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant.Case (caseText)
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import PUI (PUI(..), accumulated, acted, announce, applied, dispatched, edited, foreach, joint, looped, optioned, resolveFor, seeded, updated, with)
import Unsafe.Coerce (unsafeCoerce)

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual msg expected actual =
  when (expected /= actual) $
    void $ throw (msg <> ": expected " <> show expected <> " but got " <> show actual)

-- A PUI Effect operand whose user-output leg the test fires by hand: it ignores
-- toUser and stores the callback the merge registers via fromUser.
probe :: forall i o. Ref.Ref (Maybe (o -> Effect Unit)) -> PUI Effect i o
probe propRef = PUI $ pure
  { toUser: \_ -> pure unit
  , fromUser: \prop -> Ref.write (Just prop) propRef
  }

fire :: forall o. Ref.Ref (Maybe (o -> Effect Unit)) -> o -> Effect Unit
fire propRef o = do
  mProp <- Ref.read propRef
  for_ mProp \prop -> prop o

-- A probe that additionally records what its user-input leg receives.
probeIO :: forall i o. Ref.Ref (Array i) -> Ref.Ref (Maybe (o -> Effect Unit)) -> PUI Effect i o
probeIO insRef propRef = PUI $ pure
  { toUser: \i -> Ref.modify_ (_ <> [ i ]) insRef
  , fromUser: \prop -> Ref.write (Just prop) propRef
  }

-- An element probe for the container action: `acted` instantiates the wrapped
-- UI component once per key, so each instantiation registers its own channel legs in
-- the roster (build order) and bumps the build counter — making instance reuse
-- (identity-follows-key) and per-element firing observable at the value level.
type ElemHandle i o = { ins :: Ref.Ref (Array i), prop :: Ref.Ref (Maybe (o -> Effect Unit)) }

elemProbe :: forall i o. Ref.Ref Int -> Ref.Ref (Array (ElemHandle i o)) -> PUI Effect i o
elemProbe builds roster = PUI do
  Ref.modify_ (_ + 1) builds
  ins <- Ref.new []
  propR <- Ref.new Nothing
  Ref.modify_ (_ <> [ { ins, prop: propR } ]) roster
  pure
    { toUser: \i -> Ref.modify_ (_ <> [ i ]) ins
    , fromUser: \prop -> Ref.write (Just prop) propR
    }

fireElem :: forall i o. Ref.Ref (Array (ElemHandle i o)) -> Int -> o -> Effect Unit
fireElem roster n o = do
  handles <- Ref.read roster
  for_ (handles !! n) \h -> fire h.prop o

main :: Effect Unit
main = do
  -- == subStrong: row-typed Strong, focus a sub-record carrying the rest. On `(->)`. ==

  -- subStrong: rows on both sides. Here a one-field sub-record { a } is transformed
  -- (Int -> String) while the complement { b } is carried unchanged.
  assertEqual "subStrong"
    { a: "5", b: true }
    (subStrong (\(r :: { a :: Int }) -> { a: show r.a }) { a: 5, b: true })

  -- multi-field sub-record { a, c } transformed, complement { b } carried.
  assertEqual "subStrong/multi-field"
    { a: 50, c: 2, b: "x" }
    (subStrong (\(r :: { a :: Int, c :: Int }) -> { a: r.a * 10, c: r.c + 1 }) { a: 5, c: 1, b: "x" })

  -- field = the value-level single-field lens — get / set / over.
  assertEqual "field/view" 7 (view (field @"foo") { foo: 7, bar: "x" })
  assertEqual "field/set" { foo: 9, bar: "x" } (set (field @"foo") 9 { foo: 7, bar: "x" })
  assertEqual "field/over" { foo: 14, bar: "x" } (over (field @"foo") (_ * 2) { foo: 7, bar: "x" })

  -- recordToCase (x -> +): whole record computes a value, emitted unconditionally
  -- as case l — the introduce-family member Choice can't have, free on any Profunctor.
  assertEqual "recordToCase"
    (.total 8 :: [ total :: Int, other :: String ])
    (recordToCase @"total" (\r -> r.a + r.b) { a: 3, b: 5 })

  -- toCase: a bare output introduced as case l at the closed singleton row —
  -- recordToCase without the record-input constraint.
  assertEqual "toCase"
    (.picked 7 :: [ picked :: Int ])
    (toCase @"picked" _.key identity { key: 7, label: "x" })

  -- caseText (label-is-copy): caseText (inj @l a) = reflectSymbol (Proxy @l) —
  -- the case label of a variant value, verbatim, whichever case is inhabited.
  assertEqual "caseText" "Medium roast" (caseText (."Medium roast" {} :: [ "Light roast" :: {}, "Medium roast" :: {} ]))
  assertEqual "caseText/other case" "Light roast" (caseText (."Light roast" {} :: [ "Light roast" :: {}, "Medium roast" :: {} ]))

  -- == Merge unit laws on the PUI carrier: each merge class carries its own ==
  -- == nullary operator `pempty`. At record outputs the unit *announces* its ==
  -- == informationless {} (the parametric `silence` couldn't), so the merge ==
  -- == gates never starve against it; at variant outputs it coincides with ==
  -- == `silence`. ==

  -- ×→× unit law: recordToRecord pempty g = g (the output leg — g's emissions
  -- must pass through undisturbed, not starve against the unit).
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (recordToRecord RecordToRecord.pempty (probe gProp :: PUI Effect {} { a :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp { a: 1 }
    Ref.read outs >>= assertEqual "unit law ×→×: recordToRecord pempty g = g" [ { a: 1 } ]

  -- and on the right: recordToRecord g pempty = g.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (recordToRecord (probe gProp :: PUI Effect {} { a :: Int }) RecordToRecord.pempty)
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp { a: 2 }
    Ref.read outs >>= assertEqual "unit law ×→×: recordToRecord g pempty = g" [ { a: 2 } ]

  -- ×→× silence law: an operand owning zero fields is **pre-satisfied** — its
  -- only possible contribution is the informationless {}, so the gate must
  -- not wait for it. Sharper than the unit law: this operand never emits at
  -- all (a detached pane, an empty collection), where pempty announces.
  -- This law is what makes the display-as-stage a derived form (the merge
  -- with the echo wire) rather than a carrier primitive.
  do
    gProp <- Ref.new Nothing
    silentProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (recordToRecord (probe silentProp :: PUI Effect {} {}) (probe gProp :: PUI Effect {} { a :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp { a: 3 }
    Ref.read outs >>= assertEqual "silence law ×→×: a zero-field operand never starves the gate" [ { a: 3 } ]

  -- a gated display inside feedback: the display renders the seed at
  -- registration — pins the merge-with-wire operand order (display first,
  -- wire second: render before release, since the release may re-enter the
  -- loop mid-registration)
  do
    shown <- Ref.new ([] :: Array { top :: Int })
    outs <- Ref.new ([] :: Array {})
    let display = PUI (pure { toUser: \s -> Ref.modify_ (_ <> [ s ]) shown, fromUser: \_ -> pure unit }) :: PUI Effect { top :: Int } {}
    m <- unwrap (feedback { top: 0 } (recordToRecord display identity) :: PUI Effect {} {})
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    Ref.read shown >>= assertEqual "feedback + display beside the wire: the seed renders at registration" [ { top: 0 } ]

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
      (probe p1Prop :: PUI Effect {} { a :: Int })
      (probe p2Prop :: PUI Effect {} { b :: String }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire p2Prop { b: "fresh" }
    fire p1Prop (unsafeCoerce { a: 1, b: "stale" } :: { a :: Int })
    Ref.read outs >>= assertEqual "×→× exactness: stale runtime sibling must not shadow" [ { a: 1, b: "fresh" } ]

  -- +→× runtime-exactness: same guarantee on the other gated merge.
  do
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int, b :: String })
    m <- unwrap (variantToRecord
      (probe p1Prop :: PUI Effect [ x :: Unit ] { a :: Int })
      (probe p2Prop :: PUI Effect [ y :: Unit ] { b :: String }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire p2Prop { b: "fresh" }
    fire p1Prop (unsafeCoerce { a: 4, b: "stale" } :: { a :: Int })
    Ref.read outs >>= assertEqual "+→× exactness: stale runtime sibling must not shadow" [ { a: 4, b: "fresh" } ]

  -- +→× unit law: variantToRecord pempty g = g.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (variantToRecord VariantToRecord.pempty (probe gProp :: PUI Effect [ x :: Unit ] { a :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
      (probe p1Prop :: PUI Effect [ x :: Unit ] { a :: Int })
      (probe p2Prop :: PUI Effect [ y :: Unit ] { b :: String }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (unfirst (probeIO ins gProp :: PUI Effect (Tuple Int Boolean) (Tuple String Boolean)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (unleft (probeIO ins gProp :: PUI Effect (Either Int Int) (Either String Int)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (coresolve (probeIO ins gProp :: PUI Effect (Tuple Int Boolean) (Either String Boolean)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
  -- value and immediately resumes the UI component with the new state.
  do
    ins <- Ref.new ([] :: Array (Either Int Boolean))
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coretain (probeIO ins gProp :: PUI Effect (Either Int Boolean) (Tuple String Boolean)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser 1
    Ref.read ins >>= assertEqual "coretain: input enters fresh" [ Left 1 ]
    fire gProp (Tuple "out" true)
    Ref.read outs >>= assertEqual "coretain: value leg passes" [ "out" ]
    Ref.read ins >>= assertEqual "coretain: state resumes the UI component" [ Left 1, Right true ]

  -- looped (the ×-diagonal self-trace): every emission is re-fed (guarded)
  -- and propagated.
  do
    ins <- Ref.new ([] :: Array { n :: Int })
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { n :: Int })
    m <- unwrap (looped (probeIO ins gProp :: PUI Effect { n :: Int } { n :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser { n: 5 }
    Ref.read ins >>= assertEqual "looped: input feeds through" [ { n: 5 } ]
    fire gProp { n: 7 }
    Ref.read ins >>= assertEqual "looped: emission re-fed" [ { n: 5 }, { n: 7 } ]
    Ref.read outs >>= assertEqual "looped: emission propagates" [ { n: 7 } ]

  -- iterate (the +-trace at row granularity): `again` cases loop back,
  -- `done` cases exit.
  do
    ins <- Ref.new ([] :: Array [ again :: Int ])
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ done :: String ])
    m <- unwrap (iterate (probeIO ins gProp :: PUI Effect [ again :: Int ] [ done :: String, again :: Int ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser (.again 1)
    fire gProp (.again 2)
    Ref.read outs >>= assertEqual "iterate: again loops silently" []
    Ref.read ins >>= assertEqual "iterate: again re-enters" [ .again 1, .again 2 ]
    fire gProp (.done "d")
    Ref.read outs >>= assertEqual "iterate: done exits" [ .done "d" ]

  -- == The co-strengths' row forms: labeled channels for each trace. ==

  -- feedback (×-trace at row granularity): the traced chain's initial
  -- state is the argument — fed once at registration, so the chain renders
  -- and its first emission primes the loop before any input arrives.
  do
    ins <- Ref.new ([] :: Array { a :: Int, acc :: Int })
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { o :: Int })
    m <- unwrap (feedback { a: 0, acc: 0 } (probeIO ins gProp :: PUI Effect { a :: Int, acc :: Int } { o :: Int, acc :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    Ref.read ins >>= assertEqual "feedback: seed feeds the chain at registration" [ { a: 0, acc: 0 } ]
    fire gProp { o: 10, acc: 100 }
    Ref.read outs >>= assertEqual "feedback: value fields pass" [ { o: 10 } ]
    m.toUser { a: 2 }
    Ref.read ins >>= assertEqual "feedback: input joined with looped state" [ { a: 0, acc: 0 }, { a: 2, acc: 100 } ]

  -- folding @w (terminating fold at row granularity): the fold state's
  -- initial value is the argument — emitted once as case w at
  -- registration, priming the fold before any input; case w continues the
  -- fold silently, done cases exit.
  do
    ins <- Ref.new ([] :: Array { a :: Int, acc :: Int })
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ done :: String ])
    m <- unwrap (folding @"fold" { acc: 5 } (probeIO ins gProp :: PUI Effect { a :: Int, acc :: Int } [ done :: String, fold :: { acc :: Int } ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser { a: 1 }
    Ref.read ins >>= assertEqual "folding: first input joined with the seed" [ { a: 1, acc: 5 } ]
    fire gProp (.fold { acc: 6 })
    Ref.read outs >>= assertEqual "folding: fold case withheld" []
    Ref.read ins >>= assertEqual "folding: fold step re-fed eagerly" [ { a: 1, acc: 5 }, { a: 1, acc: 6 } ]
    m.toUser { a: 2 }
    Ref.read ins >>= assertEqual "folding: input joined with folded state" [ { a: 1, acc: 5 }, { a: 1, acc: 6 }, { a: 2, acc: 6 } ]
    fire gProp (.done "d")
    Ref.read outs >>= assertEqual "folding: done exits" [ .done "d" ]

  -- unfolding @w (productive unfold at row granularity): the unfold
  -- state's initial value is the argument — fed once as case w at
  -- registration; value fields pass, state fields resume the UI component as
  -- case w.
  do
    ins <- Ref.new ([] :: Array [ start :: Int, resume :: { acc :: Int } ])
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { o :: String })
    m <- unwrap (unfolding @"resume" { acc: 0 } (probeIO ins gProp :: PUI Effect [ start :: Int, resume :: { acc :: Int } ] { o :: String, acc :: Int }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    Ref.read ins >>= assertEqual "unfolding: seed enters as a first resume" [ .resume { acc: 0 } ]
    m.toUser (.start 1)
    Ref.read ins >>= assertEqual "unfolding: fresh input enters" [ .resume { acc: 0 }, .start 1 ]
    fire gProp { o: "x", acc: 7 }
    Ref.read outs >>= assertEqual "unfolding: value fields pass" [ { o: "x" } ]
    Ref.read ins >>= assertEqual "unfolding: state resumes as its case" [ .resume { acc: 0 }, .start 1, .resume { acc: 7 } ]

  -- Resolving/resolveFor (the quiescence step): every emission loops
  -- immediately (Right, gated on a first state), and the last emission of a
  -- burst resolves (Left) once the UI component stays quiet for the window —
  -- transiency derived from time, no wire-level flag.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array (Either String Int))
    m <- unwrap (resolveFor { ms: 40.0 } (probe gProp :: PUI Effect Int String))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (colens (\s b -> s <> show b) (_ * 10) (probeIO ins gProp :: PUI Effect String Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (coprism identity (\b -> if b > 10 then Left (show b) else Right (b + 1)) (probeIO ins gProp :: PUI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (coshutter (\b -> if b >= 100 then Left (show b) else Right (_ + b)) (probeIO ins gProp :: PUI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (coreel identity show (_ + 1) (probeIO ins gProp :: PUI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
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
    m <- unwrap (updated (\e s -> { n: s.n + e }) (probe gProp :: PUI Effect { n :: Int } Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp 3
    Ref.read outs >>= assertEqual "updated: gated before a model" []
    m.toUser { n: 10 }
    Ref.read outs >>= assertEqual "updated: value passes through" [ { n: 10 } ]
    fire gProp 3
    Ref.read outs >>= assertEqual "updated: event folded into retained model" [ { n: 10 }, { n: 13 } ]

  -- applied (the occurrence stage): a state transformer over the retained
  -- row, stepped on every emission — law: applied f ≡ updated (const f).
  -- The emitter is fed the row it acts on; whatever it emits is discarded
  -- for the retained row (fired here with a foreign payload to prove it).
  do
    gProp <- Ref.new Nothing
    ins <- Ref.new ([] :: Array { n :: Int })
    outs <- Ref.new ([] :: Array { n :: Int })
    m <- unwrap (applied (\s -> { n: s.n + 1 }) (probeIO ins gProp :: PUI Effect { n :: Int } [ count :: { n :: Int } ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp (.count { n: 99 })
    Ref.read outs >>= assertEqual "applied: gated before a model" []
    m.toUser { n: 10 }
    Ref.read ins >>= assertEqual "applied: the emitter is fed the row it acts on" [ { n: 10 } ]
    Ref.read outs >>= assertEqual "applied: value passes through" [ { n: 10 } ]
    fire gProp (.count { n: 99 })
    Ref.read outs >>= assertEqual "applied: the occurrence steps the retained row, the replay payload discarded" [ { n: 10 }, { n: 11 } ]
    fire gProp (.count { n: 99 })
    Ref.read outs >>= assertEqual "applied: each occurrence steps once more" [ { n: 10 }, { n: 11 }, { n: 12 } ]

  -- field (the leaf lift): a scalar control lifted under a label is a
  -- whole-row citizen — the background is retained by the Strong state
  -- channel and re-attached to every emission, so the stage is
  -- runtime-complete by construction and no output completion exists.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int, b :: String })
    m <- unwrap (field @"a" (probe gProp :: PUI Effect Int Int))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp 1
    Ref.read outs >>= assertEqual "field: gated before input" []
    m.toUser { a: 0, b: "kept" }
    fire gProp 9
    Ref.read outs >>= assertEqual "field: emission over carried background" [ { a: 9, b: "kept" } ]
    m.toUser { a: 2, b: "fresh" }
    fire gProp 7
    Ref.read outs >>= assertEqual "field: background follows the latest feed"
      [ { a: 9, b: "kept" }, { a: 7, b: "fresh" } ]

  -- with (the discharge form, announce's composition closure): the wrapped
  -- stage receives its initial state at registration and the result is
  -- closed — input {}, nothing left to feed
  do
    ins <- Ref.new ([] :: Array { n :: Int })
    gProp <- Ref.new Nothing
    m <- unwrap (with { n: 1 } (probeIO ins gProp :: PUI Effect { n :: Int } { n :: Int }))
    m.fromUser \_ -> pure unit
    Ref.read ins >>= assertEqual "with: seed fed at registration" [ { n: 1 } ]
    m.toUser {}
    Ref.read ins >>= assertEqual "with: closed — the informationless {} feeds nothing further" [ { n: 1 } ]

  -- seeded (the pointedness primitive): one emission of the seed at
  -- registration, then a plain wire
  do
    outs <- Ref.new ([] :: Array Int)
    m <- unwrap (seeded 1 :: PUI Effect Int Int)
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    Ref.read outs >>= assertEqual "seeded: point — the seed emits once at registration" [ 1 ]
    m.toUser 2
    Ref.read outs >>= assertEqual "seeded: wire — inputs forward unchanged" [ 1, 2 ]

  -- == The remaining merge laws: gating on ×→×, the right +→× unit, and ==
  -- == the two ungated merges (×→+ broadcast, +→+ dispatch) with their ==
  -- == units — completing one test per stated merge law. ==

  -- ×→× knowledge-gating (mirror of the +→× test): nothing propagates until
  -- every field of the merged record is known; later emissions merge with
  -- the other side's retained contribution.
  do
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int, b :: String })
    m <- unwrap (recordToRecord
      (probe p1Prop :: PUI Effect {} { a :: Int })
      (probe p2Prop :: PUI Effect {} { b :: String }))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire p1Prop { a: 1 }
    Ref.read outs >>= assertEqual "×→× gating: incomplete record withheld" []
    fire p2Prop { b: "s" }
    Ref.read outs >>= assertEqual "×→× gating: completed record propagates" [ { a: 1, b: "s" } ]
    fire p1Prop { a: 2 }
    Ref.read outs >>= assertEqual "×→× gating: later emissions merge with retained side" [ { a: 1, b: "s" }, { a: 2, b: "s" } ]

  -- the joint merge (Joining): broadcast in — every side is fed every
  -- input — interleave out: either side's emission forwards unchanged,
  -- and both groupings observe the same stream (associativity).
  do
    p1Ins <- Ref.new ([] :: Array Int)
    p2Ins <- Ref.new ([] :: Array Int)
    p3Ins <- Ref.new ([] :: Array Int)
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    p3Prop <- Ref.new Nothing
    outsL <- Ref.new ([] :: Array String)
    outsR <- Ref.new ([] :: Array String)
    l <- unwrap (joint (joint (probeIO p1Ins p1Prop) (probeIO p2Ins p2Prop)) (probeIO p3Ins p3Prop) :: PUI Effect Int String)
    l.fromUser \o -> Ref.modify_ (_ <> [ o ]) outsL
    l.toUser 7
    Ref.read p1Ins >>= assertEqual "joint: broadcast reaches the left side" [ 7 ]
    Ref.read p2Ins >>= assertEqual "joint: broadcast reaches the middle side" [ 7 ]
    Ref.read p3Ins >>= assertEqual "joint: broadcast reaches the right side" [ 7 ]
    fire p2Prop "mid"
    fire p3Prop "right"
    fire p1Prop "left"
    Ref.read outsL >>= assertEqual "joint: emissions interleave in firing order" [ "mid", "right", "left" ]
    r <- unwrap (joint (probeIO p1Ins p1Prop) (joint (probeIO p2Ins p2Prop) (probeIO p3Ins p3Prop)) :: PUI Effect Int String)
    r.fromUser \o -> Ref.modify_ (_ <> [ o ]) outsR
    fire p2Prop "mid"
    fire p3Prop "right"
    fire p1Prop "left"
    Ref.read outsR >>= assertEqual "joint: re-association changes nothing observable" [ "mid", "right", "left" ]

  -- +→× unit law on the right: variantToRecord g pempty = g.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { a :: Int })
    m <- unwrap (variantToRecord (probe gProp :: PUI Effect [ x :: Unit ] { a :: Int }) VariantToRecord.pempty)
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp { a: 3 }
    Ref.read outs >>= assertEqual "unit law +→×: variantToRecord g pempty = g" [ { a: 3 } ]

  -- ×→+ merge (ungated broadcast): every operand sees the record, either
  -- operand's case exits immediately — no gate on either side.
  do
    ins1 <- Ref.new ([] :: Array { a :: Int })
    ins2 <- Ref.new ([] :: Array { a :: Int })
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ x :: Int, y :: String ])
    m <- unwrap (recordToVariant
      (probeIO ins1 p1Prop :: PUI Effect { a :: Int } [ x :: Int ])
      (probeIO ins2 p2Prop :: PUI Effect { a :: Int } [ y :: String ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser { a: 1 }
    Ref.read ins1 >>= assertEqual "×→+ broadcast: left operand sees the record" [ { a: 1 } ]
    Ref.read ins2 >>= assertEqual "×→+ broadcast: right operand sees the record" [ { a: 1 } ]
    fire p1Prop (.x 7)
    fire p2Prop (.y "e")
    Ref.read outs >>= assertEqual "×→+ broadcast: either operand's case exits, ungated" [ .x 7, .y "e" ]

  -- ×→+ unit law: recordToVariant pempty g = g (the unit is the silent
  -- source — uninhabited variant output, so silence is forced).
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ x :: Int ])
    m <- unwrap (recordToVariant RecordToVariant.pempty (probe gProp :: PUI Effect {} [ x :: Int ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp (.x 1)
    Ref.read outs >>= assertEqual "unit law ×→+: recordToVariant pempty g = g" [ .x 1 ]

  -- +→+ merge (dispatch): each input case is routed to exactly its one
  -- handler; outputs may overlap and both exit.
  do
    ins1 <- Ref.new ([] :: Array [ x :: Int ])
    ins2 <- Ref.new ([] :: Array [ y :: String ])
    p1Prop <- Ref.new Nothing
    p2Prop <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ ok :: Int, err :: String ])
    m <- unwrap (variantToVariant
      (probeIO ins1 p1Prop :: PUI Effect [ x :: Int ] [ ok :: Int ])
      (probeIO ins2 p2Prop :: PUI Effect [ y :: String ] [ ok :: Int, err :: String ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser (.x 5)
    Ref.read ins1 >>= assertEqual "+→+ dispatch: case routed to its one handler" [ .x 5 ]
    Ref.read ins2 >>= assertEqual "+→+ dispatch: the other handler not invoked" []
    m.toUser (.y "boom")
    Ref.read ins2 >>= assertEqual "+→+ dispatch: second case routed to its handler" [ .y "boom" ]
    fire p1Prop (.ok 1)
    fire p2Prop (.err "e")
    Ref.read outs >>= assertEqual "+→+ dispatch: outputs may overlap, both exit" [ .ok 1, .err "e" ]

  -- +→+ unit law: variantToVariant pempty g = g (both unit ends uninhabited,
  -- so the unit neither receives nor emits).
  do
    ins <- Ref.new ([] :: Array [ x :: Unit ])
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array [ ok :: Int ])
    m <- unwrap (variantToVariant VariantToVariant.pempty (probeIO ins gProp :: PUI Effect [ x :: Unit ] [ ok :: Int ]))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser (.x unit)
    Ref.read ins >>= assertEqual "unit law +→+: input reaches g" [ .x unit ]
    fire gProp (.ok 9)
    Ref.read outs >>= assertEqual "unit law +→+: variantToVariant pempty g = g" [ .ok 9 ]

  -- == The hand-written ecosystem instances, tested directly, and the ==
  -- == retraction laws that hold outright (the ungated + channel). ==

  -- Strong.first: emissions are withheld until a first input pairs them
  -- (the gating principle all stateful instances share); thereafter each
  -- emission is paired with the retained pair state.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array (Tuple String Boolean))
    m <- unwrap (first (probe gProp :: PUI Effect Int String))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp "early"
    Ref.read outs >>= assertEqual "Strong.first: emission withheld before pair state" []
    m.toUser (Tuple 1 true)
    fire gProp "b"
    Ref.read outs >>= assertEqual "Strong.first: emission paired with retained state" [ Tuple "b" true ]

  -- Choice.left: Left feeds the UI component, Right passes through (after
  -- registration), UI component emissions exit as Left.
  do
    ins <- Ref.new ([] :: Array Int)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array (Either String Boolean))
    m <- unwrap (left (probeIO ins gProp :: PUI Effect Int String))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser (Left 1)
    Ref.read ins >>= assertEqual "Choice.left: Left feeds the UI component" [ 1 ]
    m.toUser (Right true)
    Ref.read outs >>= assertEqual "Choice.left: Right passes through" [ Right true ]
    fire gProp "b"
    Ref.read outs >>= assertEqual "Choice.left: UI component emission exits as Left" [ Right true, Left "b" ]

  -- Retraction law, the + channel: unleft (left g) = g — Choice's channel
  -- tied back shut is the identity on g, and (unlike the gated × channel)
  -- it holds outright, no priming needed.
  do
    ins <- Ref.new ([] :: Array Int)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (unleft (left (probeIO ins gProp :: PUI Effect Int String)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser 5
    Ref.read ins >>= assertEqual "unleft (left g) = g: input reaches g" [ 5 ]
    fire gProp "out"
    Ref.read outs >>= assertEqual "unleft (left g) = g: emission passes" [ "out" ]

  -- and its mirror: unright (right g) = g.
  do
    ins <- Ref.new ([] :: Array Int)
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (unright (right (probeIO ins gProp :: PUI Effect Int String)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser 6
    Ref.read ins >>= assertEqual "unright (right g) = g: input reaches g" [ 6 ]
    fire gProp "ret"
    Ref.read outs >>= assertEqual "unright (right g) = g: emission passes" [ "ret" ]

  -- The debouncing theorem, observable half: coresolve (resolveFor w g)
  -- lets only the last emission of a burst through, after quiescence —
  -- `coresolve (resolve g) = debounced g` on the output leg.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (coresolve (resolveFor { ms: 40.0 } (probe gProp :: PUI Effect Int String)))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    fire gProp "burst1"
    fire gProp "burst2"
    Ref.read outs >>= assertEqual "coresolve∘resolve: burst withheld before quiescence" []
    launchAff_ do
      delay (Milliseconds 100.0)
      liftEffect $ Ref.read outs >>= assertEqual
        "coresolve∘resolve = debounced: only the last of the burst passes, after quiescence" [ "burst2" ]

  -- == focusCase (the value-level case prism) on (->): the Choice strength at ==
  -- == row granularity, checked like subStrong/field above. ==

  assertEqual "focusCase/match"
    (.ok "5" :: [ ok :: String, err :: String ])
    (focusCase @"ok" (show :: Int -> String) (.ok 5 :: [ ok :: Int, err :: String ]))
  assertEqual "focusCase/pass-through"
    (.err "e" :: [ ok :: String, err :: String ])
    (focusCase @"ok" (show :: Int -> String) (.err "e" :: [ ok :: Int, err :: String ]))

  -- == Registration protocol: the announcing leaves and the compose order ==
  -- == that lets their announcements be heard. ==

  -- announce: exactly one registration-time emission, then silence.
  do
    outs <- Ref.new ([] :: Array { n :: Int })
    m <- unwrap (announce { n: 42 } :: PUI Effect {} { n :: Int })
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser {}
    Ref.read outs >>= assertEqual "announce: exactly one registration emission" [ { n: 42 } ]

  -- compose registers downstream first, so an upstream registration
  -- announcement finds downstream's wiring already listening.
  do
    ins <- Ref.new ([] :: Array { n :: Int })
    gProp <- Ref.new Nothing
    m <- unwrap ((announce { n: 5 } :: PUI Effect {} { n :: Int }) >>> probeIO ins gProp)
    m.fromUser \_ -> pure unit
    Ref.read ins >>= assertEqual "compose: registration announcement reaches downstream" [ { n: 5 } ]

  -- identity: the echo wire — whatever comes in goes straight back out.
  do
    outs <- Ref.new ([] :: Array Int)
    m <- unwrap (identity :: PUI Effect Int Int)
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser 3
    Ref.read outs >>= assertEqual "identity: the echo wire" [ 3 ]

  -- display beside the wire: unconditional pass-through — no echo needed
  -- from the wrapped display (its zero-field side is pre-satisfied), and
  -- any {} it does emit re-emits the retained value.
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { v :: Int })
    m <- unwrap (recordToRecord (probe gProp :: PUI Effect { v :: Int } {}) identity)
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser { v: 1 }
    Ref.read outs >>= assertEqual "display beside the wire: every value forwarded, no echo needed" [ { v: 1 } ]
    fire gProp {}
    Ref.read outs >>= assertEqual "display beside the wire: a {} emission re-emits the retained value" [ { v: 1 }, { v: 1 } ]

  -- muted: the counit — render, discard the output deliberately. On (->):
  -- muted g = const {}. On the PUI carrier it makes any emitting component
  -- a lawful display ({} output), so the merge-with-wire re-emits the
  -- retained value instead of losing an edit silently.
  do
    assertEqual "muted on (->): the counit" {} (muted (\(x :: Int) -> x + 1) 5)
  do
    gProp <- Ref.new Nothing
    outs <- Ref.new ([] :: Array { v :: Int })
    m <- unwrap (recordToRecord (muted (probe gProp :: PUI Effect { v :: Int } { edit :: Int })) identity)
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser { v: 4 }
    fire gProp { edit: 9 }
    Ref.read outs >>= assertEqual "muted beside the wire: a discarded emission re-emits the retained value, losing nothing not written off" [ { v: 4 }, { v: 4 } ]

  -- == The container action (Data.Profunctor.Acting): the Array case of ==
  -- == p a b -> p (F a) (F b), keyed. Laws from the module header. ==

  -- acted on (->): pure carriers have no identity — acted _ = map.
  assertEqual "acted/(->) = map, key re-attached" [ { k: "a", v: 2 }, { k: "b", v: 6 } ] (acted @"k" (\(r :: { k :: String, v :: Int }) -> { v: r.v * 2 }) [ { k: "a", v: 1 }, { k: "b", v: 3 } ])

  -- optioned on (->): the Maybe = 1 + a container action, via the Array embedding.
  assertEqual "optioned/(->): Just" (Just 6) (optioned (_ * 2) (Just 3))
  assertEqual "optioned/(->): Nothing" (Nothing :: Maybe Int) (optioned (_ * 2) Nothing)

  -- empty: fed [], emits [] — the inhabited nullary of μx. 1 + a×x. And only
  -- on feed: registration alone announces nothing ([] is not the only Array b).
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle { k :: String, v :: Int } { txt :: String }))
    outs <- Ref.new ([] :: Array (Array { k :: String, txt :: String }))
    m <- unwrap (acted @"k" (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    Ref.read outs >>= assertEqual "acted: registration announces nothing" []
    m.toUser []
    Ref.read outs >>= assertEqual "acted: fed [] emits []" [ [] ]

  -- singleton retraction: fed [a], behaves as the element fed a; an element
  -- emission b emits [b].
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle { k :: String, v :: Int } { txt :: String }))
    outs <- Ref.new ([] :: Array (Array { k :: String, txt :: String }))
    m <- unwrap (acted @"k" (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser [ { k: "a", v: 1 } ]
    handles <- Ref.read roster
    for_ (handles !! 0) \h -> Ref.read h.ins >>= assertEqual "acted/singleton: element fed its value" [ { k: "a", v: 1 } ]
    Ref.read outs >>= assertEqual "acted/singleton: withheld before the element speaks" []
    fireElem roster 0 { txt: "b" }
    Ref.read outs >>= assertEqual "acted/singleton: emission emits the keyed row — key from the input, unforgeable" [ [ { k: "a", txt: "b" } ] ]

  -- gather gate: withheld until every element has spoken; thereafter any
  -- element emission re-emits the whole array from retained last outputs.
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle { k :: String, v :: Int } { txt :: String }))
    outs <- Ref.new ([] :: Array (Array { k :: String, txt :: String }))
    m <- unwrap (acted @"k" (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser [ { k: "a", v: 1 }, { k: "b", v: 2 } ]
    fireElem roster 0 { txt: "x" }
    Ref.read outs >>= assertEqual "acted/gate: withheld until every element spoke" []
    fireElem roster 1 { txt: "y" }
    Ref.read outs >>= assertEqual "acted/gate: completes on the last voice" [ [ { k: "a", txt: "x" }, { k: "b", txt: "y" } ] ]
    fireElem roster 0 { txt: "x2" }
    Ref.read outs >>= assertEqual "acted/gate: retain-last re-emits the whole array" [ [ { k: "a", txt: "x" }, { k: "b", txt: "y" } ], [ { k: "a", txt: "x2" }, { k: "b", txt: "y" } ] ]

  -- identity follows key: re-feeding survivors builds nothing new; permuting
  -- keys reorders the gathered output without rebuilding; dropping a key
  -- keeps the survivors' instances and re-gathers immediately.
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle { k :: String, v :: Int } { txt :: String }))
    outs <- Ref.new ([] :: Array (Array { k :: String, txt :: String }))
    m <- unwrap (acted @"k" (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser [ { k: "a", v: 1 }, { k: "b", v: 2 } ]
    Ref.read builds >>= assertEqual "acted/keys: two entrants built" 2
    fireElem roster 0 { txt: "x" }
    fireElem roster 1 { txt: "y" }
    m.toUser [ { k: "a", v: 10 }, { k: "b", v: 20 } ]
    Ref.read builds >>= assertEqual "acted/keys: survivors re-fed, nothing rebuilt" 2
    handles <- Ref.read roster
    for_ (handles !! 0) \h -> Ref.read h.ins >>= assertEqual "acted/keys: survivor re-fed in place" [ { k: "a", v: 1 }, { k: "a", v: 10 } ]
    m.toUser [ { k: "b", v: 21 }, { k: "a", v: 11 } ]
    Ref.read builds >>= assertEqual "acted/keys: permutation rebuilds nothing" 2
    Ref.read outs >>= \os -> assertEqual "acted/keys: output order follows the fed key order" (Just [ { k: "b", txt: "y" }, { k: "a", txt: "x" } ]) (os !! (length os - 1))
    m.toUser [ { k: "a", v: 12 } ]
    Ref.read builds >>= assertEqual "acted/keys: dropping a key rebuilds nothing" 2
    Ref.read outs >>= \os -> assertEqual "acted/keys: after drop, gathers the survivor" (Just [ { k: "a", txt: "x" } ]) (os !! (length os - 1))

  -- foreach (the collapsed, sum-flavored sibling): silent on empty, forwards each
  -- element emission as it happens — ungated.
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle { k :: String, v :: Int } String))
    outs <- Ref.new ([] :: Array String)
    m <- unwrap (foreach @"k" identity (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser []
    Ref.read outs >>= assertEqual "foreach: silent on empty" []
    m.toUser [ { k: "a", v: 1 }, { k: "b", v: 2 } ]
    fireElem roster 0 "z"
    Ref.read outs >>= assertEqual "foreach: forwards immediately, no gate" [ "z" ]

  -- == The keyed-input members — the runtime variant `{ key, value }` feeds ==
  -- == one case at a time; the tag arrives in the input, no key function. ==

  -- dispatched (+→+): an unknown key instantiates (a new runtime case), a
  -- known key re-feeds exactly its instance, and emissions leave tagged.
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle Int String))
    outs <- Ref.new ([] :: Array { key :: String, value :: String })
    m <- unwrap (dispatched identity (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser { key: "a", value: 1 }
    Ref.read builds >>= assertEqual "dispatched: first case instantiates" 1
    m.toUser { key: "b", value: 2 }
    Ref.read builds >>= assertEqual "dispatched: second case instantiates" 2
    m.toUser { key: "a", value: 3 }
    Ref.read builds >>= assertEqual "dispatched: a known key re-feeds, no rebuild" 2
    handles <- Ref.read roster
    for_ (handles !! 0) \h -> Ref.read h.ins >>= assertEqual "dispatched: targeted feeds reach exactly their case" [ 1, 3 ]
    for_ (handles !! 1) \h -> Ref.read h.ins >>= assertEqual "dispatched: the other case untouched" [ 2 ]
    fireElem roster 1 "hello"
    Ref.read outs >>= assertEqual "dispatched: emissions leave tagged with their case's key" [ { key: "b", value: "hello" } ]

  -- accumulated (+→×): the keyed Mealy — grows per new key, updates per
  -- known key, emits the whole array immediately (input-primed); element
  -- emissions fold back into their slot; order is first appearance.
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle Int Int))
    outs <- Ref.new ([] :: Array (Array Int))
    m <- unwrap (accumulated identity (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser { key: "a", value: 1 }
    Ref.read outs >>= assertEqual "accumulated: first case emits the singleton immediately" [ [ 1 ] ]
    m.toUser { key: "b", value: 2 }
    Ref.read outs >>= assertEqual "accumulated: a new key grows the array in first-appearance order" [ [ 1 ], [ 1, 2 ] ]
    m.toUser { key: "a", value: 10 }
    Ref.read outs >>= assertEqual "accumulated: a known key updated its slot in place" [ [ 1 ], [ 1, 2 ], [ 10, 2 ] ]
    Ref.read builds >>= assertEqual "accumulated: two keys, two instances" 2
    fireElem roster 1 20
    Ref.read outs >>= \os -> assertEqual "accumulated: an element emission folds into its slot" (Just [ 10, 20 ]) (os !! (length os - 1))

  -- edited @l: the key is data and the element's output row EXCLUDES it — the
  -- carrier re-attaches each emission's key (the return address), so an
  -- element structurally cannot change it; feeds re-emit (input-primed).
  do
    builds <- Ref.new 0
    roster <- Ref.new ([] :: Array (ElemHandle { id :: String, title :: String } { title :: String }))
    outs <- Ref.new ([] :: Array (Array { id :: String, title :: String }))
    m <- unwrap (edited @"id" (elemProbe builds roster))
    m.fromUser \o -> Ref.modify_ (_ <> [ o ]) outs
    m.toUser [ { id: "a", title: "x" }, { id: "b", title: "y" } ]
    Ref.read outs >>= assertEqual "edited: input-primed — every feed re-emits the array" [ [ { id: "a", title: "x" }, { id: "b", title: "y" } ] ]
    fireElem roster 0 { title: "X" }
    Ref.read outs >>= \os -> assertEqual "edited: an edit folds immediately, its key re-attached by the carrier" (Just [ { id: "a", title: "X" }, { id: "b", title: "y" } ]) (os !! (length os - 1))
    Ref.read builds >>= assertEqual "edited: survivors were re-fed, never rebuilt" 2
