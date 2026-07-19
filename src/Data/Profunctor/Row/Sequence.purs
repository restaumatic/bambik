-- | The **sequence direction** of the row-profunctor family: the collection as
-- | the runtime-sized, homogeneous generalization of a row merge. Where a record
-- | merge combines a fixed set of distinct-typed operands over static labels,
-- | the sequence merge combines a runtime-sized set of same-typed operands over
-- | an `Array`, keyed by `key a`.
-- |
-- | Class law (up to the `Array` wrapper), for the `PUI Web` instance:
-- |
-- |   * **singleton** — `sequenced key g` fed `[a]` behaves as `g` fed `a`
-- |     (one element, built and driven through its channel);
-- |   * **empty (the nullary unit)** — `sequenced key g` fed `[]` builds nothing
-- |     and emits nothing (it collapses to the shared output `o`, which is
-- |     uninhabited with no elements — so a terminal display pairs it with
-- |     `displayed`, whose unconditional carrier echo supplies the announcing
-- |     unit);
-- |   * **retraction / reconciliation** — feeding an array reuses the element
-- |     instance for each surviving `key`, so identity (DOM, focus) follows the
-- |     key, not the position.
-- |
-- | Only `PUI Web` has an instance: a dynamic DOM collection has no `(->)` or
-- | general-carrier meaning (there is no canonical `Array a -> o` from `a -> o`),
-- | exactly as `Resolving`/`Retaining` are `PUI`-only. `foreach` (PUI.HTML) is
-- | the friendly re-export of `sequenced`.
module Data.Profunctor.Row.Sequence
  ( class Sequencing
  , sequenced
  ) where

import Prelude

import Control.Monad.State (gets)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (class Profunctor)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (PUI)
import PUI.Web (Web, appendChild, lastChild, removeChild, runDomInNode)

-- | Lift a profunctor over the homogeneous `Array` shape, keyed by `key`.
class Profunctor p <= Sequencing p where
  sequenced :: forall a o. (a -> String) -> p a o -> p (Array a) o

-- | Keyed, retaining reconciliation on the DOM carrier (see the module header
-- | law). Matched keys are re-fed in place; new keys built; absent keys removed;
-- | the DOM reordered only when the key sequence changed. A re-entrancy guard
-- | stops an element's echo (looping back through `displayed`/`mvu`) from
-- | double-building mid-reconcile.
instance Sequencing (PUI Web) where
  sequenced key w = wrap do
    parent <- gets _.parent
    propRef <- liftEffect $ Ref.new Nothing
    entriesRef <- liftEffect $ Ref.new []
    busyRef <- liftEffect $ Ref.new false
    pure
      { toUser: \items -> do
          busy <- Ref.read busyRef
          unless busy do
            Ref.write true busyRef
            old <- Ref.read entriesRef
            mProp <- Ref.read propRef
            let oldByKey = Map.fromFoldable (map (\e -> Tuple e.key e) old)
            entries <- for items \a -> do
              let k = key a
              case Map.lookup k oldByKey of
                Just e -> do
                  void $ e.inst.toUser a
                  pure e
                Nothing -> do
                  inst <- runDomInNode parent (unwrap w)
                  for_ mProp \prop -> inst.fromUser prop
                  node <- lastChild parent
                  void $ inst.toUser a
                  pure { key: k, inst, node }
            let keep = Set.fromFoldable (map _.key entries)
            for_ old \e -> unless (Set.member e.key keep) (removeChild e.node parent)
            when (map _.key old /= map _.key entries) $ for_ entries \e -> appendChild e.node parent
            Ref.write entries entriesRef
            Ref.write false busyRef
      , fromUser: \prop -> Ref.write (Just prop) propRef
      }
