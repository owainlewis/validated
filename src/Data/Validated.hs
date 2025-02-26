{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Validated
-- Copyright   :  (C) 2016 Owain Lewis
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  provisional
--
-- This module provides a Validated data type for handling validation results
-- with accumulated errors. Similar to Either but with different semantics
-- for Applicative instance.
--
-- Example usage:
--
-- @
-- validateAge :: Int -> Validated String Int
-- validateAge n 
--   | n >= 0 && n <= 120 = Valid n
--   | otherwise = Invalid "Age must be between 0 and 120"
--
-- validateName :: String -> Validated String String
-- validateName name
--   | not (null name) = Valid name
--   | otherwise = Invalid "Name cannot be empty"
--
-- data Person = Person String Int
--
-- validatePerson :: String -> Int -> Validated String Person
-- validatePerson name age = 
--   Person <$> validateName name <*> validateAge age
-- @
--
-----------------------------------------------------------------------------

module Data.Validated
  ( -- * Type
    Validated(..)
  , Validation(..)
    -- * Functions
  , isValid
  , isInvalid
  , validated
  , fromValid
  , fromInvalid
    -- * Transformer
  , ValidatedT(..)
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

-- | Check if a Validated value is valid
isValid :: Validated a b -> Bool
isValid (Valid _) = True
isValid (Invalid _) = False

-- | Check if a Validated value is invalid
isInvalid :: Validated a b -> Bool
isInvalid = not . isValid

-- | Pattern match on Validated and apply appropriate function
validated :: (a -> c) -> (b -> c) -> Validated a b -> c
validated f _ (Invalid x) = f x
validated _ g (Valid y) = g y

-- | Extract the Valid value, or error if Invalid
fromValid :: Validated a b -> b
fromValid (Valid x) = x
fromValid (Invalid _) = error "fromValid: Invalid value"

-- | Extract the Invalid value, or error if Valid
fromInvalid :: Validated a b -> a
fromInvalid (Invalid x) = x
fromInvalid (Valid _) = error "fromInvalid: Valid value"

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
    Invalid e `mappend` Valid _  = Invalid e
    Valid _ `mappend` Invalid e = Invalid e
    Valid a `mappend` Valid _ = Valid a
    mempty = Valid mempty

instance Traversable (Validated e) where
    traverse _ (Invalid e) = pure (Invalid e)
    traverse f (Valid a) = Valid <$> f a

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
newtype ValidatedT e m a = ValidatedT { runValidatedT :: m (Validated e a) }

instance (Functor m) => Functor (ValidatedT e m) where
    fmap f = ValidatedT . fmap (fmap f) . runValidatedT

instance (Applicative m) => Applicative (ValidatedT e m) where
    pure = ValidatedT . pure . pure
    ValidatedT f <*> ValidatedT v = ValidatedT $ liftA2 (<*>) f v

instance (Monad m) => Monad (ValidatedT e m) where
    return = pure
    ValidatedT m >>= f = ValidatedT $ m >>= \case
        Invalid e -> return (Invalid e)
        Valid x -> runValidatedT (f x)
