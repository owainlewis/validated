-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Validated
-- Copyright   :  (C) 2016 Owain Lewis
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  provisional
--
-- This module provides a Validated data type
-----------------------------------------------------------------------------

module Data.Validated
  ( Validated
  , isValid
  , isInvalid
  ) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Monoid

data Validated a b = Invalid a
                   | Valid b
  deriving ( Eq, Ord, Read, Show )

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

validated :: (a -> c) -> (b -> c) -> Validated a b -> c
validated f _ (Invalid x)  =  f x
validated _ g (Valid y)    =  g y

instance Functor (Validated a) where
    fmap _ (Invalid e) =
        Invalid e
    fmap f (Valid a) =
        Valid (f a)

instance Foldable (Validated e) where
    foldr f x (Valid a) = f a x
    foldr _ x (Invalid _) = x

instance Monoid e => Monoid (Validated e a) where
    Invalid e1 `mappend` Invalid e2 = Invalid (e1 <> e2)
    Invalid _ `mappend` Valid a2  = Valid a2
    Valid a1  `mappend` Invalid _ = Valid a1
    Valid a1 `mappend` Valid _ = Valid a1
    mempty = Invalid mempty

instance Bifunctor Validated where
    bimap f _ (Invalid e) = Invalid (f e)
    bimap _ g (Valid a)   = Valid (g a)

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

-- Transformer instances
--
newtype ValidatedT e m a = ValidatedT { runValidatedT :: m (Validated e a) }
