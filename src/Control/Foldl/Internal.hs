-- | Strict data types for use as internal accumulators that don't space leak

module Control.Foldl.Internal (
    -- * Strict maybe
      Maybe'(..)
    , lazy
    , strict

    -- * Strict Either
    , Either'(..)
    , hush

    -- * Strict Pair
    , Pair(..)
    ) where

-- | A strict 'Maybe'
data Maybe' a = Just' !a | Nothing'

-- | Convert 'Maybe'' to 'Maybe'
lazy :: Maybe' a -> Maybe a
lazy  Nothing' = Nothing
lazy (Just' a) = Just a
{-# INLINABLE lazy #-}

-- | Convert 'Maybe' to 'Maybe''
strict :: Maybe a -> Maybe' a
strict  Nothing  = Nothing'
strict (Just a ) = Just' a
{-# INLINABLE strict #-}

-- | A strict 'Either'
data Either' a b = Left' !a | Right' !b

-- | Convert 'Either'' to 'Maybe'
hush :: Either' a b -> Maybe b
hush (Left'  _) = Nothing
hush (Right' b) = Just b
{-# INLINABLE hush #-}

data Pair a b = Pair !a !b
