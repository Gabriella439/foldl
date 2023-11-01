{-| This module provides a `Fold1` type that is a \"non-empty\" analog of the
    `Fold` type, meaning that it requires at least one input element in order to
    produce a result

    This module does not provide all of the same utilities as the
    "Control.Foldl" module.  Instead, this module only provides the utilities
    which can make use of the non-empty input guarantee (e.g. `head`).  For
    all other utilities you can convert them from the equivalent `Fold` using
    `fromFold`.
-}

module Control.Foldl.NonEmpty where

import Control.Applicative (liftA2)
import Control.Foldl (Fold(..))
import Control.Foldl.Internal (Either'(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Profunctor (Profunctor(..))
import Data.Semigroup.Foldable (Foldable1(..))
import Prelude hiding (head, last, minimum, maximum)

import qualified Control.Foldl as Foldl

{-| A `Fold1` is like a `Fold` except that it consumes at least one input
    element
-}
data Fold1 a b = Fold1 (a -> Fold a b)

instance Functor (Fold1 a) where
    fmap f (Fold1 k) = Fold1 (fmap (fmap f) k)
    {-# INLINE fmap #-}

instance Profunctor Fold1 where
    lmap f (Fold1 k) = Fold1 k'
      where
        k' a = lmap f (k (f a))
    {-# INLINE lmap #-}

    rmap = fmap
    {-# INLINE rmap #-}

instance Applicative (Fold1 a) where
    pure b = Fold1 (pure (pure b))
    {-# INLINE pure #-}

    Fold1 l <*> Fold1 r = Fold1 (liftA2 (<*>) l r)
    {-# INLINE (<*>) #-}

instance Semigroup b => Semigroup (Fold1 a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance Monoid b => Monoid (Fold1 a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance Num b => Num (Fold1 a b) where
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

    negate = fmap negate
    {-# INLINE negate #-}

    abs = fmap abs
    {-# INLINE abs #-}

    signum = fmap signum
    {-# INLINE signum #-}

    (+) = liftA2 (+)
    {-# INLINE (+) #-}

    (*) = liftA2 (*)
    {-# INLINE (*) #-}

    (-) = liftA2 (-)
    {-# INLINE (-) #-}

instance Fractional b => Fractional (Fold1 a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance Floating b => Floating (Fold1 a b) where
    pi = pure pi
    {-# INLINE pi #-}

    exp = fmap exp
    {-# INLINE exp #-}

    sqrt = fmap sqrt
    {-# INLINE sqrt #-}

    log = fmap log
    {-# INLINE log #-}

    sin = fmap sin
    {-# INLINE sin #-}

    tan = fmap tan
    {-# INLINE tan #-}

    cos = fmap cos
    {-# INLINE cos #-}

    asin = fmap asin
    {-# INLINE asin #-}

    atan = fmap atan
    {-# INLINE atan #-}

    acos = fmap acos
    {-# INLINE acos #-}

    sinh = fmap sinh
    {-# INLINE sinh #-}

    tanh = fmap tanh
    {-# INLINE tanh #-}

    cosh = fmap cosh
    {-# INLINE cosh #-}

    asinh = fmap asinh
    {-# INLINE asinh #-}

    atanh = fmap atanh
    {-# INLINE atanh #-}

    acosh = fmap acosh
    {-# INLINE acosh #-}

    (**) = liftA2 (**)
    {-# INLINE (**) #-}

    logBase = liftA2 logBase
    {-# INLINE logBase #-}

-- | Apply a strict left `Fold1` to a `NonEmpty` list
fold1 :: Foldable1 f => Fold1 a b -> f a -> b
fold1 (Fold1 k) as1 = Foldl.fold (k a) as
  where
    a :| as = toNonEmpty as1
{-# INLINABLE fold1 #-}

-- | Promote any `Fold` to an equivalent `Fold1`
fromFold :: Fold a b -> Fold1 a b
fromFold (Fold step begin done) = Fold1 (\a -> Fold step (step begin a) done)
{-# INLINABLE fromFold #-}

-- | Promote any `Fold1` to an equivalent `Fold`
toFold :: Fold1 a b -> Fold a (Maybe b)
toFold (Fold1 k0) = Fold step begin done
  where
    begin = Left' k0

    step (Left' k) a = Right' (k a)
    step (Right' (Fold step' begin' done')) a =
        Right' (Fold step' (step' begin' a) done')

    done (Right' (Fold _ begin' done')) = Just (done' begin')
    done (Left' _) = Nothing
{-# INLINABLE toFold #-}

-- | Fold all values within a non-empty container into a `NonEmpty` list
nonEmpty :: Fold1 a (NonEmpty a)
nonEmpty = Fold1 (\a -> fmap (a :|) Foldl.list)
{-# INLINEABLE nonEmpty #-}

-- | Fold all values within a non-empty container using (`<>`)
sconcat :: Semigroup a => Fold1 a a
sconcat = Fold1 (\begin -> Fold (<>) begin id)
{-# INLINABLE sconcat #-}

-- | Get the first element of a non-empty container
head :: Fold1 a a
head = Fold1 (\begin -> Fold step begin id)
  where
    step a _ = a
{-# INLINABLE head #-}

-- | Get the last element of a non-empty container
last :: Fold1 a a
last = Fold1 (\begin -> Fold step begin id)
  where
    step _ a = a
{-# INLINABLE last #-}

-- | Computes the maximum element
maximum :: Ord a => Fold1 a a
maximum = Fold1 (\begin -> Fold max begin id)
{-# INLINABLE maximum #-}

-- | Computes the maximum element with respect to the given comparison function
maximumBy :: (a -> a -> Ordering) -> Fold1 a a
maximumBy cmp = Fold1 (\begin -> Fold max' begin id)
  where
    max' x y = case cmp x y of
        GT -> x
        _  -> y
{-# INLINABLE maximumBy #-}

-- | Computes the minimum element
minimum :: Ord a => Fold1 a a
minimum = Fold1 (\begin -> Fold min begin id)
{-# INLINABLE minimum #-}

-- | Computes the minimum element with respect to the given comparison function
minimumBy :: (a -> a -> Ordering) -> Fold1 a a
minimumBy cmp = Fold1 (\begin -> Fold min' begin id)
  where
    min' x y = case cmp x y of
        GT -> y
        _  -> x
{-# INLINABLE minimumBy #-}
