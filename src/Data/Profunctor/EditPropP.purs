module Data.Profunctor.EditPropP where

import Prelude

import Data.Lens (Optic)
import Data.Profunctor (class Profunctor, dimap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row (class Cons)
import Record (get, insert, set)
import Type.Proxy (Proxy(..))

class Profunctor p <= EditPropP p where
  liftEditProp :: forall s e. p e e -> p (Tuple s e) (Tuple s e) -- e edited, s context

-- `forall p. EditPropP p => Optic p s s a a` encodes `s -> e; s -> e -> s`
-- or
-- `forall p. EditPropP p => Optic p s s a a` encodes `s -> (e, e -> s)`

editProp' :: forall s e. (s -> e) -> (s -> e -> s) -> (forall p. EditPropP p => Optic p s s e e)
editProp' get set = liftEditProp >>> dimap (\s -> Tuple s (get s)) (\(Tuple s e) -> set s e)

editProp :: forall s e. (s -> Tuple e (e -> s)) -> (forall p. EditPropP p => Optic p s s e e)
editProp f = editProp' (f >>> fst) (f >>> snd)


-- TODO
-- editPropInv :: forall s e. (forall p. EditPropP p => Optic p s s e e) -> (s -> Tuple e (e -> s))
-- editPropInv f s i = run (f (introduce' i)) s

edit :: forall p @l r s e. IsSymbol l => Cons l e r s => EditPropP p => Optic p (Record s) (Record s) e e
edit = editProp' (get (Proxy @l)) (flip (set (Proxy @l)))


instance EditPropP (->) where
  liftEditProp f (Tuple s e) = Tuple s (f e)

--
-- instance EditPropP (Cont r) where
--   liftEditProp cont = wrap $ \si2r s -> unwrap cont (\i -> si2r (Tuple s i)) unit
  -- ((e -> r) -> (e -> r)) -> (Tuple s e -> r) -> Tuple s e -> r

-- EditPropP is not a subclass of Strong
-- EditPropP is a superclass of Strong:
strongToEditPropP :: forall p. Profunctor p => (forall a b c. p a b -> p (Tuple c a) (Tuple c b)) -> (forall s e. p e e -> p (Tuple s e) (Tuple s e))
strongToEditPropP second = second
