-- | Composable left folds

{-# LANGUAGE ExistentialQuantification #-}

module Control.Foldl
    ( -- * The Fold Type
      Fold(..)
    , fold

      -- * Folds
    , head
    , last
    , null
    , length
    , and
    , or
    , all
    , any
    , sum
    , product
    , maximum
    , minimum
    , elem
    , notElem
    , find
    , index
    , elemIndex
    , findIndex

    -- * Generic Folds
    , genericLength
    , genericIndex
    ) where

import Control.Applicative
import Data.List (foldl')
import Prelude hiding
    ( head
    , last
    , null
    , length
    , and
    , or
    , all
    , any
    , sum
    , product
    , maximum
    , minimum
    , elem
    , notElem
    )

{-| Efficient representation of a left fold that preserves the fold's step
    function so that multiple folds can be combined into a single fold that
    traverses the container only once
-}
data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)

-- | Apply a 'Fold' to a container, computing the final result
fold :: Fold a b -> [a] -> b
fold (Fold step nil done) as = done (foldl' step nil as)
{-# INLINABLE fold #-}

data Pair a b = Pair !a !b

instance Functor (Fold a) where
    fmap f (Fold step nil done) = Fold step nil (f . done)
    {-# INLINABLE fmap #-}

instance Applicative (Fold a) where
    pure b    = Fold (\() _ -> ()) () (\() -> b)
    {-# INLINABLE pure #-}
    (Fold stepL nilL doneL) <*> (Fold stepR nilR doneR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            nil = Pair nilL nilR
            done (Pair xL xR) = (doneL xL) (doneR xR)
        in  Fold step nil done
    {-# INLINABLE (<*>) #-}

data Maybe' a = Just' !a | Nothing'

lazy :: Maybe' a -> Maybe a
lazy  Nothing'  = Nothing
lazy (Just' a') = Just a'

{-| Get the first element of a container or return 'Nothing' if the container is
    empty
-}
head :: Fold a (Maybe a)
head = Fold step Nothing' lazy
  where
    step x a = case x of
        Nothing' -> Just' a
        _        -> x
{-# INLINABLE head #-}

{-| Get the last element of a container or return 'Nothing' if the container is
    empty
-}
last :: Fold a (Maybe a)
last = Fold (\_ -> Just') Nothing' lazy
{-# INLINABLE last #-}

-- | Returns 'True' if the container is empty, 'False' otherwise
null :: Fold a Bool
null = Fold (\_ _ -> False) True id
{-# INLINABLE null #-}

-- | Return the length of the container
length :: Fold a Int
length = genericLength
{- Technically, 'length' is just 'genericLength' specialized to 'Int's.  I keep
   the two separate so that I can later provide an 'Int'-specialized
   implementation of 'length' for performance reasons like "GHC.List" does
   without breaking backwards compatibility.
-}
{-# INLINABLE length #-}

-- | Like 'length', except with a more general 'Num' return value
genericLength :: (Num b) => Fold a b
genericLength = Fold (\n _ -> n + 1) 0 id
{-# INLINABLE genericLength #-}

-- | Returns 'True' if all elements are 'True', 'False' otherwise
and :: Fold Bool Bool
and = Fold (&&) True id
{-# INLINABLE and #-}

-- | Returns 'True' if any element is 'True', 'False' otherwise
or :: Fold Bool Bool
or = Fold (||) False id
{-# INLINABLE or #-}

{-| @(all predicate)@ returns 'True' if all elements satisfy the predicate,
    'False' otherwise
-}
all :: (a -> Bool) -> Fold a Bool
all predicate = Fold (\x a -> x && predicate a) True id
{-# INLINABLE all #-}

{-| @(any predicate)@ returns 'True' is any element satisfies the predicate,
    'False' otherwise
-}
any :: (a -> Bool) -> Fold a Bool
any predicate = Fold (\x a -> x || predicate a) False id
{-# INLINABLE any #-}

-- | Computes the sum of all elements
sum :: (Num a) => Fold a a
sum = Fold (+) 0 id
{-# INLINABLE sum #-}

-- | Computes the product all elements
product :: (Num a) => Fold a a
product = Fold (*) 1 id
{-# INLINABLE product #-}

-- | Computes the maximum element
maximum :: (Ord a) => Fold a (Maybe a)
maximum = Fold step Nothing' lazy
  where
    step x a = Just' (case x of
        Nothing' -> a
        Just' a' -> max a a')
{-# INLINABLE maximum #-}

-- | Computes the minimum element
minimum :: (Ord a) => Fold a (Maybe a)
minimum = Fold step Nothing' lazy
  where
    step x a = Just' (case x of
        Nothing' -> a
        Just' a' -> min a a')
{-# INLINABLE minimum #-}

{-| @(elem a)@ returns 'True' if the container has an element equal to @a@,
    'False' otherwise
-}
elem :: (Eq a) => a -> Fold a Bool
elem a = any (a ==)
{-# INLINABLE elem #-}

{-| @(notElem a)@ returns 'False' if the container has an element equal to @a@,
    'True' otherwise
-}
notElem :: (Eq a) => a -> Fold a Bool
notElem a = all (a /=)
{-# INLINABLE notElem #-}

{-| @(find predicate)@ returns the first element that satisfies the predicate or
    'Nothing' if no element satisfies the predicate
-}
find :: (a -> Bool) -> Fold a (Maybe a)
find predicate = Fold step Nothing' lazy
  where
    step x a = case x of
        Nothing' -> if (predicate a) then Just' a else Nothing'
        _        -> x
{-# INLINABLE find #-}

data Either' a b = Left' !a | Right' !b

{-| @(index n)@ returns the @n@th element of the container, or 'Nothing' if the
    container has an insufficient number of elements
-}
index :: Int -> Fold a (Maybe a)
index = genericIndex
{-# INLINABLE index #-}

-- | Like 'index', except with a more general 'Integral' argument
genericIndex :: (Integral i) => i -> Fold a (Maybe a)
genericIndex i = Fold step (Left' 0) done
  where
    step x a = case x of
        Left'  j -> if (i == j) then Right' a else Left' (j + 1)
        _        -> x
    done x = case x of
        Left'  _ -> Nothing
        Right' a -> Just a
{-# INLINABLE genericIndex #-}

{-| @(elemIndex a)@ returns the index of the first element that equals @a@, or
    'Nothing' if no element matches
-}
elemIndex :: (Eq a) => a -> Fold a (Maybe Int)
elemIndex a = findIndex (a ==)
{-# INLINABLE elemIndex #-}

{-| @(findIndex predicate)@ returns the index of the first element that
    satisfies the predicate, or 'Nothing' if no element satisfies the predicate
-}
findIndex :: (a -> Bool) -> Fold a (Maybe Int)
findIndex predicate = Fold step (Pair 0 False) done
  where
    step x@(Pair i b) a =
        if b                  then x
        else if (predicate a) then Pair  i      True
        else                       Pair (i + 1) False
    done (Pair i b) = if b then Just i else Nothing
{-# INLINABLE findIndex #-}
