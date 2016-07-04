module Data.Validated 
  where

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

instance Applicative (Validated e) where
    pure = Valid
    Invalid e1 <*> Invalid _ = Invalid e1
    Invalid e1 <*> Valid _ = Invalid e1
    Valid _ <*> Invalid e2 = Invalid e2
    Valid f <*> Valid a = Valid (f a)

instance Monad (Validated a) where
    return = Valid
    Invalid e >>= _ = Invalid e
    Valid a >>= f = f a

-- | Collect all failure results into a list
failures :: [a] -> [Validated a t] -> [a]
failures acc [] = acc
failures acc ((Invalid x) : ys) = failures (x : acc) ys
failures acc (_ : ys) = failures acc ys

failed :: [Validated a t] -> [a]
failed = failures []
