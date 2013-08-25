{-| This module provides efficient and streaming left folds that you can combine
    using 'Applicative' style.

    Import this module qualified to avoid clashing with the Prelude:

>>> import qualified Control.Foldl as L

    Use 'fold' to apply a 'Fold' to a list:

>>> L.fold L.sum [1..100]
5050

    'Fold's are 'Applicative's, so you can combine them using 'Applicative'
    combinators:

>>> import Control.Applicative
>>> let average = (/) <$> L.sum <*> L.genericLength

    These combined folds will still traverse the list only once, streaming
    efficiently over the list in constant space without space leaks:

>>> L.fold average [1..10000000]
5000000.5
>>> L.fold ((,) <$> L.minimum <*> L.maximum) [1..10000000]
(Just 1,Just 10000000)

    You can also unpack the `Fold` type if you want to extract the individual
    components of combined folds for use with your own customized folding
    utilities:

> case ((/) <$> L.sum <*> L.genericLength) of
>     L.Foldl step begin done -> ...
-}

{-# LANGUAGE ExistentialQuantification #-}

module Control.Foldl
    ( -- * Fold Types
      Fold(..)
    , fold
    , FoldM(..)
    , foldM

      -- * Folds
    , mconcat
    , foldMap
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

import Control.Applicative (Applicative(pure, (<*>)))
import Data.Monoid (Monoid(mempty, mappend))
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
    function, initial accumulator, and extraction function

    This allows the 'Applicative' instance to assemble derived folds that
    traverse the container only once
-}
data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)

-- | Apply a strict left 'Fold' to a list and extract the final result
fold :: Fold a b -> [a] -> b
fold (Fold step begin done) as = done (foldr step' id as begin)
  where
    step' x k z = k $! step z x
{-# INLINE fold #-}

data Pair a b = Pair !a !b

instance Functor (Fold a) where
    fmap f (Fold step begin done) = Fold step begin (f . done)
    {-# INLINABLE fmap #-}

instance Applicative (Fold a) where
    pure b    = Fold (\() _ -> ()) () (\() -> b)
    {-# INLINABLE pure #-}
    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            begin = Pair beginL beginR
            done (Pair xL xR) = (doneL xL) (doneR xR)
        in  Fold step begin done
    {-# INLINABLE (<*>) #-}

-- | Like 'Fold', but monadic
data FoldM m a b = forall x . FoldM (x -> a -> m x) (m x) (x -> m b)

instance (Monad m) => Functor (FoldM m a) where
    fmap f (FoldM step start done) = FoldM step start done'
      where
        done' x = do
            b <- done x
            return $! f b
    {-# INLINABLE fmap #-}

instance (Monad m) => Applicative (FoldM m a) where
    pure b = FoldM (\() _ -> return ()) (return ()) (\() -> return b)
    {-# INLINABLE pure #-}
    (FoldM stepL beginL doneL) <*> (FoldM stepR beginR doneR) =
        let step (Pair xL xR) a = do
                xL' <- stepL xL a
                xR' <- stepR xR a
                return $! Pair xL' xR'
            begin = do
                xL <- beginL
                xR <- beginR
                return $! Pair xL xR
            done (Pair xL xR) = do
                f <- doneL xL
                x <- doneR xR
                return $! f x
        in  FoldM step begin done
    {-# INLINABLE (<*>) #-}

-- | Like 'fold', but monadic
foldM :: (Monad m) => FoldM m a b -> [a] -> m b
foldM (FoldM step begin done) as0 = do
    x <- begin
    loop as0 $! x
  where
    loop  []    x = done x
    loop (a:as) x = do
        x' <- step x a
        loop as $! x'
{-# INLINABLE foldM #-}

-- | Fold all values within a container using 'mappend' and 'mempty'
mconcat :: (Monoid a) => Fold a a
mconcat = Fold mappend mempty id
{-# INLINABLE mconcat #-}

-- | Convert a \"@foldMap@\" to a 'Fold'
foldMap :: (Monoid w) => (a -> w) -> (w -> b) -> Fold a b
foldMap to from = Fold (\x a -> mappend x (to a)) mempty from
{-# INLINABLE foldMap #-}

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

-- | Like 'length', except with a more general 'Num' return value
genericLength :: (Num b) => Fold a b
genericLength = Fold (\n _ -> n + 1) 0 id
{-# INLINABLE genericLength #-}

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

-- | @fold (filtered pred folder) list == fold folder (filter pred list)@
filtered :: (a -> Bool) -> Fold a r -> Fold a r
filtered predicate (Fold step begin done) = Fold step' begin done
  where
    step' x a | predicate a = step x a
              | otherwise   = x

-- | @fold (mapped f folder) list == fold folder (map f list)@
mapped :: (a -> b) -> Fold b r -> Fold a r
mapped f (Fold step begin done) = Fold step' begin done
  where
    step' x = step x . f

-- | @fold (unzipped aFolder bFolder) list = let (aList, bList) = unzip list in (fold aFolder aList) (fold bFolder bList)@
unzipped :: Fold a a' -> Fold b b' -> Fold (a, b) (a', b')
unzipped (Fold stepA beginA doneA) (Fold stepB beginB doneB)
    = Fold step begin done
      where
        step (Pair xA xB) (a, b) = Pair (stepA xA a) (stepB xB b)
        begin = Pair beginA beginB
        done (Pair xA xB) = (doneA xA, doneB xB)

-- | @fold (tailed folder) list == fmap (fold folder) (tailMaybe list)@
-- | where tailMaybe is a safe replacement for tail
tailed :: Fold a r -> Fold a (Maybe r)
tailed (Fold step begin done) = Fold step' begin' done'
  where
    step' Nothing' _ = Just' begin
    step' (Just' x) a = Just' (step x a)
    begin' = Nothing'
    done' Nothing' = Nothing
    done' (Just' x) = Just (done x)

-- | @fold (interspersed x folder) list = fold folder (intersperse x list)@
interspersed :: a -> Fold a r -> Fold a r
interspersed a (Fold step begin done) = Fold step' begin' done'
  where
    step' Nothing' b = Just' (step begin b)
    step' (Just' x) b = Just' (step (step x a) b)
    begin' = Nothing'
    done' Nothing' = done begin
    done' (Just' x) = done x

-- | @fold (scannedl f z folder) list = fold folder (scanl f z list)@
scannedl :: (a -> b -> a) -> a -> Fold a r -> Fold b r
scannedl f z (Fold step begin done) = Fold step' begin' done'
  where
    step' (Pair s x) a = let s' = f s a in Pair s' (step x s')
    begin' = Pair z (step begin z)
    done' (Pair _ x) = done x

-- | @fold (taken n folder) list = fold folder (take n list)@
taken :: Int -> Fold a r -> Fold a r
taken n (Fold _ begin done) | n < 0 = pure (done begin)
taken n (Fold step begin done) = Fold step' begin' done'
  where
    step' (Pair 0 x) _ = Pair 0 x
    step' (Pair m x) a = Pair (m - 1) (step x a)
    begin' = Pair n begin
    done' (Pair _ x) = done x

-- | @fold (dropped n folder) list = fold folder (drop n list)@
dropped :: Int -> Fold a r -> Fold a r
dropped n folder | n <= 0 = folder
dropped n (Fold step begin done) = Fold step' begin' done'
  where
    step' (Left' 0) a = Right' (step begin a)
    step' (Left' m) _ = Left' (m - 1)
    step' (Right' x) a = Right' (step x a)
    begin' = Left' n
    done' (Left' _) = done begin
    done' (Right' x) = done x

-- | @fold (takenWhile pred folder) list = fold folder (takeWhile pred list)@
takenWhile :: (a -> Bool) -> Fold a r -> Fold a r
takenWhile predicate (Fold step begin done) = Fold step' begin' done'
  where
    step' (Pair False x) _ = Pair False x
    step' (Pair True x) a | predicate a = Pair True (step x a)
                          | otherwise   = Pair False x
    begin' = Pair True begin
    done' (Pair _ x) = done x

-- | @fold (droppedWhile pred folder) list = fold folder (dropWhile pred list)@
droppedWhile :: (a -> Bool) -> Fold a r -> Fold a r
droppedWhile predicate (Fold step begin done) = Fold step' begin' done'
  where
    step' (Pair False x) a = Pair False (step x a)
    step' (Pair True x) a | predicate a = Pair True x
                          | otherwise   = Pair False (step x a)
    begin' = Pair True begin
    done' (Pair _ x) = done x
