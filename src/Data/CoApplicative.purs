module Data.CoApplicative where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, head, tail, (:|))
import Data.Tuple (Tuple(..))

class Functor f <= CoApply f where -- TODO maybe Covariant?
  cozip :: forall a b. f (Either a b) -> Either (f a) (f b)

class CoApply f <= CoApplicative f where
  copure :: forall a. f a -> a

instance CoApply Identity where
  cozip (Identity (Left x)) = Left (Identity x)
  cozip (Identity (Right x)) = Right (Identity x)

instance CoApply Maybe where
  cozip :: forall a b. Maybe (Either a b) -> Either (Maybe a) (Maybe b)
  cozip Nothing = Left Nothing -- TODO or Right Nothing?
  cozip (Just (Left a)) = Left (Just a)
  cozip (Just (Right b)) = Right (Just b)

instance CoApplicative Identity where
  copure (Identity x) = x

instance CoApply (NonEmpty Array) where
  cozip ne = case head ne of
    Left x -> Left $ x :| mapMaybe (either Just (const Nothing)) (tail ne)
    Right y -> Right $ y :| mapMaybe (either (const Nothing) Just) (tail ne)

instance CoApplicative (NonEmpty Array) where
  copure = head

-- instance CoApplicative w => CoApplicative (StoreT s w) where
--   copure (StoreT (Tuple wsa s)) = (copure wsa) s
--   cozip (StoreT (Tuple wsa s))  = case (copure wsa) s of
--     Left  a -> Left  (StoreT (Tuple (map $ either identity (const a) <<< wsa) s))
--     Right b -> Right (StoreT (Tuple (map $ either (const b) identity <<< wsa) s))

-- instance CoApplicative Costate where
--   copure (Costate _ a) = a
--   cozip (Costate f (Left  a)) = Left  (Costate (f <<< Left ) a)
--   cozip (Costate f (Right a)) = Right (Costate (f <<< Right) a)

instance CoApply (Tuple s) where
  cozip (Tuple c (Left x)) = Left $ Tuple c x
  cozip (Tuple c (Right x)) = Right $ Tuple c x

instance CoApplicative (Tuple s) where
  copure (Tuple _  x) = x

