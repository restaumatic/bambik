module Data.Profunctor.Extra where

import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (class Choice, left)
import Data.Profunctor.Strong (class Strong, first)
import Data.Tuple (Tuple(..))

secondBasedOnFirst :: forall p a b c. Strong p => p a b -> p (Tuple c a) (Tuple c b)
-- equivalent to `second`
secondBasedOnFirst = dimap swap swap <<< first
  where
    swap :: forall x y. Tuple x y -> Tuple y x
    swap (Tuple a b) = Tuple b a

rightBasedOnLeft :: forall p a b c. Choice p => p a b -> p (Either c a) (Either c b)
-- equivalent to `right`
rightBasedOnLeft = dimap flip flip <<< left
  where
    flip :: forall x y. Either x y -> Either y x
    flip (Left a) = Right a
    flip (Right b) = Left b
