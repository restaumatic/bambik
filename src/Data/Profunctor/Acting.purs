-- | The **container action**: lift a widget over a container of its focus —
-- | `p a b -> p (F a) (F b)` — here at `F = Array`, the container
-- | `μ x. 1 + a × x`. Containers are generated from `×`, `+` and fixpoints,
-- | so this class is not a fifth merge direction: it is the closure of
-- | `Strong` and `Choice` under `μ` (the profunctor traversal), with the
-- | **key function as the species refinement** — shapes carry a finite key
-- | set (`a -> String`), and on stateful carriers reconciliation is the
-- | functorial action along partial injections of key sets: survivors re-fed
-- | in place, entrants built, leavers removed. Pure carriers have no identity
-- | to preserve, so they ignore the key (`(->)`: `acted _ = map`).
-- | See doc/collections-profunctor-algebra.md.
-- |
-- | Laws (the `Array b` output side is a product, so per the unit and gate
-- | laws it announces and gates):
-- |
-- |   * **empty** — fed `[]`, emits `[]` (the inhabited nullary of the `μ`;
-- |     no starvation). No emission *before* the first feed: `[]` is not the
-- |     only `Array b`, so announcing it at registration would fabricate
-- |     knowledge (contrast `pempty`, whose `{}` is the only value there is).
-- |   * **singleton retraction** — fed `[a]`, behaves as the element fed `a`
-- |     and emits `[b]` per element emission `b` (yanking at the container).
-- |   * **gather gate** — `Array b` is withheld until *every* element has
-- |     emitted at least once; thereafter any element emission re-emits the
-- |     whole array from retained last outputs (the knowledge gate, at
-- |     runtime size).
-- |   * **identity follows key** (stateful carriers) — re-feeding a surviving
-- |     key reuses its instance; permuting keys reorders without rebuilding.
-- |
-- | The **collapsed** form (`collapsed`, re-exported as `PUI.HTML.foreach`)
-- | is the sum-flavored sibling: every element emission is forwarded onto one
-- | shared channel as it happens — ungated, and lawfully **silent on empty**
-- | (zero emitters: an uninhabited output side has nothing to announce). It
-- | is carrier-only: "which element fired" is a fact about *time*, exactly as
-- | `Resolving`/`Retaining` are `PUI`-only. Both forms share one keyed
-- | reconciler; they differ only in how element emissions exit.
module Data.Profunctor.Acting
  ( class Acting
  , acted
  , class Hosting
  , hosting
  , Hooks
  , collapsed
  , optioned
  ) where

import Prelude

import Control.Monad.State (gets)
import Data.Array (head) as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Set as Set
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import PUI (PUI)
import PUI.Web (Node, Web, appendChild, lastChild, removeChild, runDomInNode)

-- | Lift a widget over the keyed `Array` container (see the module header
-- | for the laws). Written trailing, like the merges' operands:
-- | `row # acted _.id`.
class Profunctor p <= Acting p where
  acted :: forall a b. (a -> String) -> p a b -> p (Array a) (Array b)

-- | Pure carriers have no element identity to preserve — the key is species
-- | bookkeeping for stateful instances, so `(->)` ignores it.
instance Acting (->) where
  acted _ = map

-- | The `Maybe = 1 + a` container action, derived: `Maybe` embeds in `Array`
-- | as the at-most-one-element arrays. Keeps the element *fed and live* on
-- | `Nothing`-to-`Just` transitions per the carrier's retention; contrast
-- | `PUI.HTML.provided`, the *detaching* visibility form with collapsed
-- | output.
optioned :: forall p a b. Acting p => p a b -> p (Maybe a) (Maybe b)
optioned = dimap (maybe [] pure) Array.head <<< acted (const "the")

-- The shared keyed reconciler: one entry per key, holding the element
-- instance's feed leg, its retained last output (the gather slot), and its
-- carrier node. Both emission modes are wired through `onEmit` at build time.
type Entry a b node =
  { key :: String
  , feed :: a -> Effect Unit
  , slot :: Ref.Ref (Maybe b)
  , node :: node
  }

-- What a carrier must supply: instantiate one element widget (returning its
-- channel legs and its node), detach a node, and restack all nodes into the
-- current key order.
type Hooks a b node =
  { instantiate :: Effect { feed :: a -> Effect Unit, subscribe :: (b -> Effect Unit) -> Effect Unit, node :: node }
  , detach :: node -> Effect Unit
  , restack :: Array node -> Effect Unit
  }

-- Reconcile the entry vector against a fed array: survivors re-fed in place,
-- entrants instantiated (their emissions wired to `onEmit` over their own
-- slot), leavers detached, nodes restacked only when the key sequence
-- changed. The busy guard stops an element echo from double-building
-- mid-reconcile.
reconcile
  :: forall a b node
   . (a -> String)
  -> Hooks a b node
  -> (Ref.Ref (Maybe b) -> b -> Effect Unit)
  -> Ref.Ref Boolean
  -> Ref.Ref (Array (Entry a b node))
  -> Array a
  -> Effect Unit
reconcile key hooks onEmit busyRef entriesRef items = do
  busy <- Ref.read busyRef
  unless busy do
    Ref.write true busyRef
    old <- Ref.read entriesRef
    let oldByKey = Map.fromFoldable (map (\e -> Tuple e.key e) old)
    entries <- for items \a -> do
      let k = key a
      case Map.lookup k oldByKey of
        Just e -> do
          e.feed a
          pure e
        Nothing -> do
          slot <- Ref.new Nothing
          inst <- hooks.instantiate
          inst.subscribe \b -> onEmit slot b
          inst.feed a
          pure { key: k, feed: inst.feed, slot, node: inst.node }
    let keep = Set.fromFoldable (map _.key entries)
    for_ old \e -> unless (Set.member e.key keep) (hooks.detach e.node)
    when (map _.key old /= map _.key entries) $ hooks.restack (map _.node entries)
    Ref.write entries entriesRef
    Ref.write false busyRef

-- Was this reconcile skipped by the re-entrancy guard? (A guarded skip must
-- also skip the post-reconcile gather, or a mid-reconcile echo would emit a
-- half-updated vector.)
guarded :: Ref.Ref Boolean -> Effect Unit -> Effect Unit
guarded busyRef act = do
  busy <- Ref.read busyRef
  unless busy act

-- The gather mode: element emissions land in their slot, then the whole
-- array re-emits from retained slots once every element has spoken —
-- including immediately after a reconcile, so `[]` emits `[]` and survivors'
-- retained outputs re-emit without waiting.
actedWith :: forall a b node. (a -> String) -> Hooks a b node -> Effect { toUser :: Array a -> Effect Unit, fromUser :: (Array b -> Effect Unit) -> Effect Unit }
actedWith key hooks = do
  propRef <- Ref.new Nothing
  entriesRef <- Ref.new []
  busyRef <- Ref.new false
  let
    gather = do
      entries <- Ref.read entriesRef
      slots <- for entries \e -> Ref.read e.slot
      for_ (sequence slots) \bs -> do
        mProp <- Ref.read propRef
        for_ mProp \prop -> prop bs
    onEmit slot b = do
      Ref.write (Just b) slot
      gather
  pure
    { toUser: \items -> guarded busyRef do
        reconcile key hooks onEmit busyRef entriesRef items
        gather
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- The forward (collapsed) mode: element emissions exit onto the shared
-- channel as they happen; the slot is kept written so a later gather-mode
-- reading of the same core stays possible, but nothing gates.
collapsedWith :: forall a o node. (a -> String) -> Hooks a o node -> Effect { toUser :: Array a -> Effect Unit, fromUser :: ((o -> Effect Unit) -> Effect Unit) }
collapsedWith key hooks = do
  propRef <- Ref.new Nothing
  entriesRef <- Ref.new []
  busyRef <- Ref.new false
  let
    onEmit slot o = do
      Ref.write (Just o) slot
      mProp <- Ref.read propRef
      for_ mProp \prop -> prop o
  pure
    { toUser: reconcile key hooks onEmit busyRef entriesRef
    , fromUser: \prop -> Ref.write (Just prop) propRef
    }

-- | What a stateful carrier contributes to the container action: how to
-- | **instantiate** one element widget at runtime and how to **place** it —
-- | detach a leaver, restack survivors into the current key order. The keyed
-- | reconciler and both emission modes are carrier-generic above this, so
-- | `Acting (PUI m)` holds for every hosting carrier.
class MonadEffect m <= Hosting m node | m -> node where
  hosting :: forall a b. PUI m a b -> m (Hooks a b node)

-- | The DOM carrier: instantiate under the enclosing parent (the freshly
-- | appended child is the instance's node), detach removes it, restack
-- | re-appends in key order (`appendChild` moves an existing node, so
-- | identity — focus, local state — travels with it).
instance Hosting Web Node where
  hosting w = do
    parent <- gets _.parent
    pure
      { instantiate: do
          inst <- runDomInNode parent (unwrap w)
          node <- lastChild parent
          pure { feed: inst.toUser, subscribe: inst.fromUser, node }
      , detach: \node -> removeChild node parent
      , restack: \nodes -> for_ nodes \node -> appendChild node parent
      }

-- | Placement-free: an element instance is just its channel legs — the probe
-- | carrier the value-level law tests run on (test/Main.purs).
instance Hosting Effect Unit where
  hosting w = pure
    { instantiate: do
        inst <- unwrap w
        pure { feed: inst.toUser, subscribe: inst.fromUser, node: unit }
    , detach: \_ -> pure unit
    , restack: \_ -> pure unit
    }

-- | Keyed, retaining collection on any hosting carrier (see the
-- | module-header laws).
instance Hosting m node => Acting (PUI m) where
  acted key w = wrap do
    hooks <- hosting w
    liftEffect $ actedWith key hooks

-- | The collapsed (sum-flavored) collection on any hosting carrier — every
-- | element emission forwarded onto one shared channel; ungated, silent on
-- | empty. `PUI.HTML.foreach = collapsed`.
collapsed :: forall m node a o. Hosting m node => (a -> String) -> PUI m a o -> PUI m (Array a) o
collapsed key w = wrap do
  hooks <- hosting w
  liftEffect $ collapsedWith key hooks
