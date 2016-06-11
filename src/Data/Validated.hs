module Data.Validated where

import           Control.Applicative
import           Data.Bifunctor

data Validated a b = Invalid a | Valid b
  deriving ( Eq, Ord, Show )

class Validation v where
    -- Construct a valid value
    valid   :: a -> v x a
    -- Construct an invalid value
    invalid :: e -> v e x

instance Validation Validated where
  invalid = Invalid
  valid = Valid

isValid (Invalid _) = True
isValid (Valid _) = False

isInvalid (Invalid _) = True
isInvalid (Valid _) = False

instance Functor (Validated a) where
    fmap _ (Invalid e) =
        Invalid e
    fmap f (Valid a) =
        Valid (f a)

instance Bifunctor Validated where
    bimap f _ (Invalid e) =
        Invalid (f e)
    bimap _ g (Valid a) =
        Valid (g a)