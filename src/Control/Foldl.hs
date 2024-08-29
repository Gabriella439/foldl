{-| This module provides efficient and streaming left folds that you can combine
    using 'Applicative' style.

    Import this module qualified to avoid clashing with the Prelude:

>>> import qualified Control.Foldl as Foldl

    Use 'fold' to apply a 'Fold' to a list:

>>> Foldl.fold Foldl.sum [1..100]
5050

    'Fold's are 'Applicative's, so you can combine them using 'Applicative'
    combinators:

>>> import Control.Applicative
>>> let average = (/) <$> Foldl.sum <*> Foldl.genericLength

    … or you can use @do@ notation if you enable the @ApplicativeDo@ language
    extension:

>>> :set -XApplicativeDo
>>> let average = do total <- Foldl.sum; count <- Foldl.genericLength; return (total / count)

    … or you can use the fact that the `Fold` type implements `Num` to do this:

>>> let average = Foldl.sum / Foldl.genericLength

    These combined folds will still traverse the list only once, streaming
    efficiently over the list in constant space without space leaks:

>>> Foldl.fold average [1..10000000]
5000000.5
>>> Foldl.fold ((,) <$> Foldl.minimum <*> Foldl.maximum) [1..10000000]
(Just 1,Just 10000000)

    You might want to try enabling the @-flate-dmd-anal@ flag when compiling
    executables that use this library to further improve performance.
-}

{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Trustworthy               #-}

module Control.Foldl (
    -- * Fold Types
      Fold(..)
    , FoldM(..)

    -- * Folding
    , fold
    , foldM
    , scan
    , prescan
    , postscan

    -- * Folds
    , Control.Foldl.mconcat
    , Control.Foldl.foldMap
    , head
    , last
    , lastDef
    , lastN
    , null
    , length
    , and
    , or
    , all
    , any
    , sum
    , product
    , mean
    , variance
    , std
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , elem
    , notElem
    , find
    , index
    , lookup
    , elemIndex
    , findIndex
    , random
    , randomN
    , Control.Foldl.mapM_
    , sink

    -- ** Generic Folds
    , genericLength
    , genericIndex

    -- ** Container Folds
    , list
    , revList
    , nub
    , eqNub
    , set
    , hashSet
    , map
    , foldByKeyMap
    , hashMap
    , foldByKeyHashMap
    , vector
    , vectorM

    -- * Utilities
    -- $utilities
    , purely
    , purely_
    , impurely
    , impurely_
    , generalize
    , simplify
    , hoists
    , duplicateM
    , _Fold1
    , premap
    , premapM
    , postmapM
    , prefilter
    , prefilterM
    , predropWhile
    , drop
    , dropM
    , Handler
    , handles
    , foldOver
    , EndoM(..)
    , HandlerM
    , handlesM
    , foldOverM
    , folded
    , filtered
    , groupBy
    , either
    , eitherM
    , nest

    -- * Re-exports
    -- $reexports
    , module Control.Monad.Primitive
    , module Data.Foldable
    , module Data.Vector.Generic
    ) where

import Control.Foldl.Optics (_Left, _Right)
import Control.Applicative
import Control.Foldl.Internal (Maybe'(..), lazy, Either'(..), Pair(..), hush)
import Control.Monad ((<=<))
import Control.Monad.Primitive (PrimMonad, RealWorld)
import Control.Comonad
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Functor.Contravariant (Contravariant(..))
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Data.Monoid hiding ((<>))
import Data.Semigroupoid (Semigroupoid)
import Data.Functor.Extend (Extend(..))
import Data.Profunctor
import Data.Profunctor.Sieve
import Data.Sequence ((|>))
import Data.Vector.Generic (Vector, Mutable)
import Data.Vector.Generic.Mutable (MVector)
import Data.Hashable (Hashable)
import Data.Traversable
import Numeric.Natural (Natural)
import System.Random (StdGen, newStdGen, uniformR)
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
    , lookup
    , map
    , either
    , drop
    )

import qualified Data.Foldable               as F
import qualified Data.List                   as List
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Data.Map.Strict             as Map
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.HashSet                as HashSet
import qualified Data.Vector.Generic         as V
import qualified Control.Foldl.Util.Vector   as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Semigroupoid

{- $setup

>>> import qualified Control.Foldl as Foldl
>>> import Data.Functor.Apply (Apply(..))

>>> _2 f (x, y) = fmap (\i -> (x, i)) (f y)

>>> :{
>>> _Just = let maybeEither Nothing = Left Nothing
>>>             maybeEither (Just x) = Right x
>>>         in Control.Foldl.Optics.prism Just maybeEither
>>> :}

>>> both f (x, y) = (,) <$> f x <.> f y

-}

{-| Efficient representation of a left fold that preserves the fold's step
    function, initial accumulator, and extraction function

    This allows the 'Applicative' instance to assemble derived folds that
    traverse the container only once

    A \''Fold' a b\' processes elements of type __a__ and results in a
    value of type __b__.
-}
data Fold a b
  -- | @Fold @ @ step @ @ initial @ @ extract@
  = forall x. Fold (x -> a -> x) x (x -> b)

instance Functor (Fold a) where
    fmap f (Fold step begin done) = Fold step begin (f . done)
    {-# INLINE fmap #-}

instance Profunctor Fold where
    lmap = premap
    rmap = fmap

instance Choice Fold where
    right' (Fold step begin done) = Fold (liftA2 step) (Right begin) (fmap done)
    {-# INLINE right' #-}

instance Cosieve Fold [] where
    cosieve = fold
    {-# INLINE cosieve #-}

instance Costrong Fold where
    unfirst p = fmap f list
      where
        f as = b
          where (b, d) = fold p [ (a, d) | a <- as ]
    {-# INLINE unfirst #-}

instance Comonad (Fold a) where
    extract (Fold _ begin done) = done begin
    {-#  INLINE extract #-}

    duplicate (Fold step begin done) = Fold step begin (\x -> Fold step x done)
    {-#  INLINE duplicate #-}

instance Applicative (Fold a) where
    pure b    = Fold (\() _ -> ()) () (\() -> b)
    {-# INLINE pure #-}

    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            begin = Pair beginL beginR
            done (Pair xL xR) = doneL xL (doneR xR)
        in  Fold step begin done
    {-# INLINE (<*>) #-}

instance Extend (Fold a) where
    duplicated = duplicate
    {-# INLINE duplicated #-}

instance Semigroup b => Semigroup (Fold a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance Semigroupoid Fold where
    o (Fold step1 begin1 done1) (Fold step2 begin2 done2) = Fold
        step
        (Pair begin1 begin2)
        (\(Pair x _) -> done1 x)
      where
        step (Pair c1 c2) a =
            let c2' = step2 c2 a
                c1' = step1 c1 (done2 c2')
            in  Pair c1' c2'
    {-# INLINE o #-}

instance Monoid b => Monoid (Fold a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance Num b => Num (Fold a b) where
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

instance Fractional b => Fractional (Fold a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance Floating b => Floating (Fold a b) where
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

{-| Like 'Fold', but monadic.

    A \''FoldM' m a b\' processes elements of type __a__ and
    results in a monadic value of type __m b__.
-}
data FoldM m a b =
  -- | @FoldM @ @ step @ @ initial @ @ extract@
  forall x . FoldM (x -> a -> m x) (m x) (x -> m b)

instance Functor m => Functor (FoldM m a) where
    fmap f (FoldM step start done) = FoldM step start done'
      where
        done' x = fmap f $! done x
    {-# INLINE fmap #-}

instance Applicative m => Applicative (FoldM m a) where
    pure b = FoldM (\() _ -> pure ()) (pure ()) (\() -> pure b)
    {-# INLINE pure #-}

    (FoldM stepL beginL doneL) <*> (FoldM stepR beginR doneR) =
        let step (Pair xL xR) a = Pair <$> stepL xL a <*> stepR xR a
            begin = Pair <$> beginL <*> beginR
            done (Pair xL xR) = doneL xL <*> doneR xR
        in  FoldM step begin done
    {-# INLINE (<*>) #-}

instance Monad m => Extend (FoldM m a) where
    duplicated = duplicateM
    {-# INLINE duplicated #-}

instance Functor m => Profunctor (FoldM m) where
    rmap = fmap
    lmap f (FoldM step begin done) = FoldM step' begin done
      where
        step' x a = step x (f a)

instance (Semigroup b, Monad m) => Semigroup (FoldM m a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance (Monoid b, Monad m) => Monoid (FoldM m a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance (Monad m, Num b) => Num (FoldM m a b) where
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

instance (Monad m, Fractional b) => Fractional (FoldM m a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance (Monad m, Floating b) => Floating (FoldM m a b) where
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

-- | Apply a strict left 'Fold' to a 'Foldable' container
fold :: Foldable f => Fold a b -> f a -> b
fold (Fold step begin done) as = F.foldr cons done as begin
  where
    cons a k x = k $! step x a
{-# INLINE fold #-}

-- | Like 'fold', but monadic
foldM :: (Foldable f, Monad m) => FoldM m a b -> f a -> m b
foldM (FoldM step begin done) as0 = do
    x0 <- begin
    F.foldr step' done as0 $! x0
  where
    step' a k x = do
        x' <- step x a
        k $! x'
{-# INLINE foldM #-}

{-| Convert a strict left 'Fold' into a scan

    >>> Foldl.scan Foldl.length [1..5]
    [0,1,2,3,4,5]
-}
scan :: Fold a b -> [a] -> [b]
scan (Fold step begin done) as = foldr cons nil as begin
  where
    nil      x = done x:[]
    cons a k x = done x:(k $! step x a)
{-# INLINE scan #-}

{-| Convert a `Fold` into a prescan for any `Traversable` type

    \"Prescan\" means that the last element of the scan is not included

    >>> Foldl.prescan Foldl.length [1..5]
    [0,1,2,3,4]
-}
prescan :: Traversable t => Fold a b -> t a -> t b
prescan (Fold step begin done) as = bs
  where
    step' x a = (x', b)
      where
        x' = step x a
        b  = done x
    (_, bs) = mapAccumL step' begin as
{-# INLINE prescan #-}

{-| Convert a `Fold` into a postscan for any `Traversable` type

    \"Postscan\" means that the first element of the scan is not included

    >>> Foldl.postscan Foldl.length [1..5]
    [1,2,3,4,5]
-}
postscan :: Traversable t => Fold a b -> t a -> t b
postscan (Fold step begin done) as = bs
  where
    step' x a = (x', b)
      where
        x' = step x a
        b  = done x'
    (_, bs) = mapAccumL step' begin as
{-# INLINE postscan #-}

-- | Fold all values within a container using 'mappend' and 'mempty'
mconcat :: Monoid a => Fold a a
mconcat = Fold mappend mempty id
{-# INLINABLE mconcat #-}

-- | Convert a \"@foldMap@\" to a 'Fold'
foldMap :: Monoid w => (a -> w) -> (w -> b) -> Fold a b
foldMap to = Fold (\x a -> mappend x (to a)) mempty
{-# INLINABLE foldMap #-}

{-| Get the first element of a container or return 'Nothing' if the container is
    empty
-}
head :: Fold a (Maybe a)
head = _Fold1 const
{-# INLINABLE head #-}

{-| Get the last element of a container or return 'Nothing' if the container is
    empty
-}
last :: Fold a (Maybe a)
last = _Fold1 (flip const)
{-# INLINABLE last #-}

{-| Get the last element of a container or return a default value if the container
    is empty
-}
lastDef :: a -> Fold a a
lastDef a = Fold (\_ a' -> a') a id
{-# INLINABLE lastDef #-}

{-| Return the last N elements

-}
lastN :: Int -> Fold a [a]
lastN n = Fold step begin done
  where
    step s a = s' |> a
      where
        s' =
            if Seq.length s < n
            then s
            else Seq.drop 1 s
    begin = Seq.empty
    done  = F.toList
{-# INLINABLE lastN #-}

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

{-| @(any predicate)@ returns 'True' if any element satisfies the predicate,
    'False' otherwise
-}
any :: (a -> Bool) -> Fold a Bool
any predicate = Fold (\x a -> x || predicate a) False id
{-# INLINABLE any #-}

-- | Computes the sum of all elements
sum :: Num a => Fold a a
sum = Fold (+) 0 id
{-# INLINABLE sum #-}

-- | Computes the product of all elements
product :: Num a => Fold a a
product = Fold (*) 1 id
{-# INLINABLE product #-}

-- | Compute a numerically stable arithmetic mean of all elements
mean :: Fractional a => Fold a a
mean = Fold step begin done
  where
    begin = Pair 0 0
    step (Pair x n) y = let n' = n+1 in Pair (x + (y - x) /n') n'
    done (Pair x _) = x
{-# INLINABLE mean #-}

-- | Compute a numerically stable (population) variance over all elements
variance :: Fractional a => Fold a a
variance = Fold step begin done
  where
    begin = Pair3 0 0 0

    step (Pair3 n mean_ m2) x = Pair3 n' mean' m2'
      where
        n'     = n + 1
        mean'  = (n * mean_ + x) / (n + 1)
        delta  = x - mean_
        m2'    = m2 + delta * delta * n / (n + 1)

    done (Pair3 n _ m2) = m2 / n
{-# INLINABLE variance #-}

{-| Compute a numerically stable (population) standard deviation over all
    elements
-}
std :: Floating a => Fold a a
std = sqrt variance
{-# INLINABLE std #-}

-- | Computes the maximum element
maximum :: Ord a => Fold a (Maybe a)
maximum = _Fold1 max
{-# INLINABLE maximum #-}

{-| Computes the maximum element with respect to the given comparison
    function
-}
maximumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
maximumBy cmp = _Fold1 max'
  where
    max' x y = case cmp x y of
        GT -> x
        _  -> y
{-# INLINABLE maximumBy #-}

-- | Computes the minimum element
minimum :: Ord a => Fold a (Maybe a)
minimum = _Fold1 min
{-# INLINABLE minimum #-}

{-| Computes the minimum element with respect to the given comparison
    function
-}
minimumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
minimumBy cmp = _Fold1 min'
  where
    min' x y = case cmp x y of
        GT -> y
        _  -> x
{-# INLINABLE minimumBy #-}

{-| @(elem a)@ returns 'True' if the container has an element equal to @a@,
    'False' otherwise
-}
elem :: Eq a => a -> Fold a Bool
elem a = any (a ==)
{-# INLINABLE elem #-}

{-| @(notElem a)@ returns 'False' if the container has an element equal to @a@,
    'True' otherwise
-}
notElem :: Eq a => a -> Fold a Bool
notElem a = all (a /=)
{-# INLINABLE notElem #-}

{-| @(find predicate)@ returns the first element that satisfies the predicate or
    'Nothing' if no element satisfies the predicate
-}
find :: (a -> Bool) -> Fold a (Maybe a)
find predicate = Fold step Nothing' lazy
  where
    step x a = case x of
        Nothing' -> if predicate a then Just' a else Nothing'
        _        -> x
{-# INLINABLE find #-}

{-| @(index n)@ returns the @n@th element of the container, or 'Nothing' if the
    container has an insufficient number of elements
-}
index :: Int -> Fold a (Maybe a)
index = genericIndex
{-# INLINABLE index #-}

{-| @(elemIndex a)@ returns the index of the first element that equals @a@, or
    'Nothing' if no element matches
-}
elemIndex :: Eq a => a -> Fold a (Maybe Int)
elemIndex a = findIndex (a ==)
{-# INLINABLE elemIndex #-}

{-| @(findIndex predicate)@ returns the index of the first element that
    satisfies the predicate, or 'Nothing' if no element satisfies the predicate
-}
findIndex :: (a -> Bool) -> Fold a (Maybe Int)
findIndex predicate = Fold step (Left' 0) hush
  where
    step x a = case x of
        Left' i ->
            if predicate a
            then Right' i
            else Left' (i + 1)
        _       -> x
{-# INLINABLE findIndex #-}


{-| @(lookup a)@ returns the element paired with the first matching item, or
    'Nothing' if none matches
-}
lookup :: Eq a => a -> Fold (a,b) (Maybe b)
lookup a0 = Fold step Nothing' lazy
  where
    step x (a,b) = case x of
      Nothing' -> if a == a0
        then Just' b
        else Nothing'
      _ -> x
{-# INLINABLE lookup #-}

data Pair3 a b c = Pair3 !a !b !c

-- | Pick a random element, using reservoir sampling
random :: FoldM IO a (Maybe a)
random = FoldM step begin done
  where
    begin = do
        g <- newStdGen
        return $! Pair3 g Nothing' (1 :: Int)

    step (Pair3 g Nothing'  _) a = return $! Pair3 g (Just' a) 2
    step (Pair3 g (Just' a) m) b = do
        let (n, g') = uniformR (1, m) g
        let c = if n == 1 then b else a
        return $! Pair3 g' (Just' c) (m + 1)

    done (Pair3 _ ma _) = return (lazy ma)
{-# INLINABLE random #-}

data VectorState = Incomplete {-# UNPACK #-} !Int | Complete

data RandomNState v a = RandomNState
    { _size      ::                !VectorState
    , _reservoir ::                !(Mutable v RealWorld a)
    , _position  :: {-# UNPACK #-} !Int
    , _gen       :: {-# UNPACK #-} !StdGen
    }

-- | Pick several random elements, using reservoir sampling
randomN :: Vector v a => Int -> FoldM IO a (Maybe (v a))
randomN n = FoldM step begin done
  where
    step
        :: MVector (Mutable v) a
        => RandomNState v a -> a -> IO (RandomNState v a)
    step (RandomNState (Incomplete m) mv i g) a = do
        M.write mv m a
        let m' = m + 1
        let s  = if n <= m' then Complete else Incomplete m'
        return $! RandomNState s mv (i + 1) g
    step (RandomNState  Complete      mv i g) a = do
        let (r, g') = uniformR (0, i - 1) g
        if r < n
            then M.unsafeWrite mv r a
            else return ()
        return (RandomNState Complete mv (i + 1) g')

    begin = do
        mv  <- M.new n
        gen <- newStdGen
        let s = if n <= 0 then Complete else Incomplete 0
        return (RandomNState s mv 1 gen)

    done :: Vector v a => RandomNState v a -> IO (Maybe (v a))
    done (RandomNState (Incomplete _) _  _ _) = return Nothing
    done (RandomNState  Complete      mv _ _) = do
        v <- V.freeze mv
        return (Just v)

-- | Converts an effectful function to a fold. Specialized version of 'sink'.
mapM_ :: Monad m => (a -> m ()) -> FoldM m a ()
mapM_ = sink
{-# INLINABLE mapM_ #-}

{-| Converts an effectful function to a fold

> sink (f <> g) = sink f <> sink g -- if `(<>)` is commutative
> sink mempty = mempty
-}


sink ::  (Monoid w, Monad m) => (a -> m w) -> FoldM m a w
sink act = FoldM step begin done  where
  done = return
  begin = return mempty
  step m a = do
    m' <- act a
    return $! mappend m m'
{-# INLINABLE sink #-}

-- | Like 'length', except with a more general 'Num' return value
genericLength :: Num b => Fold a b
genericLength = Fold (\n _ -> n + 1) 0 id
{-# INLINABLE genericLength #-}

-- | Like 'index', except with a more general 'Integral' argument
genericIndex :: Integral i => i -> Fold a (Maybe a)
genericIndex i = Fold step (Left' 0) done
  where
    step x a = case x of
        Left'  j -> if i == j then Right' a else Left' (j + 1)
        _        -> x
    done x = case x of
        Left'  _ -> Nothing
        Right' a -> Just a
{-# INLINABLE genericIndex #-}

-- | Fold all values into a list
list :: Fold a [a]
list = Fold (\x a -> x . (a:)) id ($ [])
{-# INLINABLE list #-}

-- | Fold all values into a list, in reverse order
revList :: Fold a [a]
revList = Fold (\x a -> a:x) [] id
{-# INLINABLE revList #-}

{-| /O(n log n)/.  Fold values into a list with duplicates removed, while
    preserving their first occurrences
-}
nub :: Ord a => Fold a [a]
nub = Fold step (Pair Set.empty id) fin
  where
    step (Pair s r) a = if Set.member a s
      then Pair s r
      else Pair (Set.insert a s) (r . (a :))
    fin (Pair _ r) = r []
{-# INLINABLE nub #-}

{-| /O(n^2)/.  Fold values into a list with duplicates removed, while preserving
    their first occurrences
-}
eqNub :: Eq a => Fold a [a]
eqNub = Fold step (Pair [] id) fin
  where
    step (Pair known r) a = if List.elem a known
      then Pair known r
      else Pair (a : known) (r . (a :))
    fin (Pair _ r) = r []
{-# INLINABLE eqNub #-}

-- | Fold values into a set
set :: Ord a => Fold a (Set.Set a)
set = Fold (flip Set.insert) Set.empty id
{-# INLINABLE set #-}

-- | Fold values into a hash-set
hashSet :: (Eq a, Hashable a) => Fold a (HashSet.HashSet a)
hashSet = Fold (flip HashSet.insert) HashSet.empty id
{-# INLINABLE hashSet #-}

{-|
Fold pairs into a map.
-}
map :: Ord a => Fold (a, b) (Map.Map a b)
map = Fold step begin done
  where
    begin = mempty
    step m (k, v) = Map.insert k v m
    done = id
{-# INLINABLE map #-}


{- | Given a 'Fold', produces a 'Map' which applies that fold to each @a@ separated by key @k@.

>>> fold (foldByKeyMap Control.Foldl.sum) [("a",1), ("b",2), ("b",20), ("a",10)]
fromList [("a",11),("b",22)]
-}
foldByKeyMap :: forall k a b. Ord k => Fold a b -> Fold (k, a) (Map k b)
foldByKeyMap f = case f of
  Fold (step0 :: x -> a -> x) (ini0 :: x) (end0 :: x -> b) ->
    let
      step :: Map k x -> (k,a) -> Map k x
      step !mp (k,a) = Map.alter addToMap k mp where
        addToMap Nothing         = Just $ step0 ini0 a
        addToMap (Just existing) = Just $ step0 existing a

      ini :: Map k x
      ini = Map.empty

      end :: Map k x -> Map k b
      end = fmap end0
    in Fold step ini end where

{-|
Fold pairs into a hash-map.
-}
hashMap :: (Eq a, Hashable a) => Fold (a, b) (HashMap.HashMap a b)
hashMap = Fold step begin done
  where
    begin = mempty
    step m (k, v) = HashMap.insert k v m
    done = id
{-# INLINABLE hashMap #-}

{- | Given a 'Fold', produces a 'HashMap' which applies that fold to each @a@ separated by key @k@.

>>> List.sort (HashMap.toList (fold (foldByKeyHashMap Control.Foldl.sum) [("a",1), ("b",2), ("b",20), ("a",10)]))
[("a",11),("b",22)]
-}
foldByKeyHashMap :: forall k a b. (Hashable k, Eq k) => Fold a b -> Fold (k, a) (HashMap k b)
foldByKeyHashMap f = case f of
  Fold (step0 :: x -> a -> x) (ini0 :: x) (end0 :: x -> b) ->
    let
      step :: HashMap k x -> (k,a) -> HashMap k x
      step mp (k,a) = HashMap.alter addToHashMap k mp where
        addToHashMap Nothing         = Just $ step0 ini0 a
        addToHashMap (Just existing) = Just $ step0 existing a

      ini :: HashMap k x
      ini = HashMap.empty

      end :: HashMap k x -> HashMap k b
      end = fmap end0
    in Fold step ini end where

-- | Fold all values into a vector
vector :: Vector v a => Fold a (v a)
vector = V.fromReverseListN <$> length <*> revList
{-# INLINABLE vector #-}

maxChunkSize :: Int
maxChunkSize = 8 * 1024 * 1024

{-| Fold all values into a vector

    This is more efficient than `vector` but is impure
-}
vectorM :: (PrimMonad m, Vector v a) => FoldM m a (v a)
vectorM = FoldM step begin done
  where
    begin = do
        mv <- M.unsafeNew 10
        return (Pair mv 0)
    step (Pair mv idx) a = do
        let len = M.length mv
        mv' <- if idx >= len
            then M.unsafeGrow mv (min len maxChunkSize)
            else return mv
        M.unsafeWrite mv' idx a
        return (Pair mv' (idx + 1))
    done (Pair mv idx) = do
        v <- V.freeze mv
        return (V.unsafeTake idx v)
{-# INLINABLE vectorM #-}

{- $utilities
    'purely' and 'impurely' allow you to write folds compatible with the @foldl@
    library without incurring a @foldl@ dependency.  Write your fold to accept
    three parameters corresponding to the step function, initial
    accumulator, and extraction function and then users can upgrade your
    function to accept a 'Fold' or 'FoldM' using the 'purely' or 'impurely'
    combinators.

    For example, the @pipes@ library implements @fold@ and @foldM@ functions in
    @Pipes.Prelude@ with the following type:

> Pipes.Prelude.fold
>     :: Monad m
>     -> (x -> a -> x) -> x -> (x -> b) -> Producer a m () -> m b
>
> Pipes.Prelude.foldM
>     :: Monad m
>     => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m () -> m b

    Both @fold@ and @foldM@ is set up so that you can wrap them with either
    'purely' or 'impurely' to accept a 'Fold' or 'FoldM', respectively:

> purely Pipes.Prelude.fold
>     :: Monad m => Fold a b -> Producer a m () -> m b
>
> impurely Pipes.Prelude.foldM
>     :: Monad m => FoldM m a b -> Producer a m () -> m b

    Other streaming libraries supporting 'purely' and 'impurely' include @io-streams@ and @streaming@.
    So for example we have:

> purely System.IO.Streams.fold_
>     :: Fold a b -> Streams.InputStream a -> IO b
>
> impurely System.IO.Streams.foldM_
>     :: FoldM IO a b -> Streams.InputStream a -> IO b

    The @monotraversable@ package makes it convenient to apply a
    'Fold' or 'FoldM' to pure containers that do not allow
    a general 'Foldable' instance, like unboxed vectors:

> purely ofoldlUnwrap
>     :: MonoFoldable mono
>     => Fold (Element mono) b -> mono -> b
>
> impurely ofoldMUnwrap
>     :: MonoFoldable mono
>     => FoldM m (Element mono) b -> mono -> m b
-}

-- | Upgrade a fold to accept the 'Fold' type
purely :: (forall x . (x -> a -> x) -> x -> (x -> b) -> r) -> Fold a b -> r
purely f (Fold step begin done) = f step begin done
{-# INLINABLE purely #-}

-- | Upgrade a more traditional fold to accept the `Fold` type
purely_ :: (forall x . (x -> a -> x) -> x -> x) -> Fold a b -> b
purely_ f (Fold step begin done) = done (f step begin)
{-# INLINABLE purely_ #-}

-- | Upgrade a monadic fold to accept the 'FoldM' type
impurely
    :: (forall x . (x -> a -> m x) -> m x -> (x -> m b) -> r)
    -> FoldM m a b
    -> r
impurely f (FoldM step begin done) = f step begin done
{-# INLINABLE impurely #-}

-- | Upgrade a more traditional monadic fold to accept the `FoldM` type
impurely_
    :: Monad m
    => (forall x . (x -> a -> m x) -> m x -> m x) -> FoldM m a b -> m b
impurely_ f (FoldM step begin done) = do
    x <- f step begin
    done x
{-# INLINABLE impurely_ #-}

{-| Generalize a `Fold` to a `FoldM`

> generalize (pure r) = pure r
>
> generalize (f <*> x) = generalize f <*> generalize x
-}
generalize :: Monad m => Fold a b -> FoldM m a b
generalize (Fold step begin done) = FoldM step' begin' done'
  where
    step' x a = return (step x a)
    begin'    = return  begin
    done' x   = return (done x)
{-# INLINABLE generalize #-}

{-| Simplify a pure `FoldM` to a `Fold`

> simplify (pure r) = pure r
>
> simplify (f <*> x) = simplify f <*> simplify x
-}
simplify :: FoldM Identity a b -> Fold a b
simplify (FoldM step begin done) = Fold step' begin' done'
  where
    step' x a = runIdentity (step x a)
    begin'    = runIdentity  begin
    done' x   = runIdentity (done x)
{-# INLINABLE simplify #-}

{- | Shift a 'FoldM' from one monad to another with a morphism such as 'lift' or 'liftIO';
     the effect is the same as 'Control.Monad.Morph.hoist'.
-}
hoists :: (forall x . m x -> n x) -> FoldM m a b -> FoldM n a b
hoists phi (FoldM step begin done) = FoldM (\a b -> phi (step a b)) (phi begin) (phi . done)
{-# INLINABLE hoists #-}

{-| Allows to continue feeding a 'FoldM' even after passing it to a function
that closes it.

For pure 'Fold's, this is provided by the 'Control.Comonad.Comonad' instance.
-}
duplicateM :: Applicative m => FoldM m a b -> FoldM m a (FoldM m a b)
duplicateM (FoldM step begin done) =
    FoldM step begin (\x -> pure (FoldM step (pure x) done))
{-# INLINABLE duplicateM #-}

{-| @_Fold1 step@ returns a new 'Fold' using just a step function that has the
same type for the accumulator and the element. The result type is the
accumulator type wrapped in 'Maybe'. The initial accumulator is retrieved from
the 'Foldable', the result is 'None' for empty containers.
 -}
_Fold1 :: (a -> a -> a) -> Fold a (Maybe a)
_Fold1 step = Fold step_ Nothing' lazy
  where
    step_ mx a = Just' (case mx of
        Nothing' -> a
        Just' x -> step x a)
{-# INLINABLE _Fold1 #-}

{-| @(premap f folder)@ returns a new 'Fold' where f is applied at each step

> fold (premap f folder) list = fold folder (List.map f list)

>>> fold (premap Sum Foldl.mconcat) [1..10]
Sum {getSum = 55}

>>> fold Foldl.mconcat (List.map Sum [1..10])
Sum {getSum = 55}

> premap id = id
>
> premap (f . g) = premap g . premap f

> premap k (pure r) = pure r
>
> premap k (f <*> x) = premap k f <*> premap k x
-}
premap :: (a -> b) -> Fold b r -> Fold a r
premap f (Fold step begin done) = Fold step' begin done
  where
    step' x a = step x (f a)
{-# INLINABLE premap #-}

{-| @(premapM f folder)@ returns a new 'FoldM' where f is applied to each input
    element

> premapM return = id
>
> premapM (f <=< g) = premap g . premap f

> premapM k (pure r) = pure r
>
> premapM k (f <*> x) = premapM k f <*> premapM k x
-}
premapM :: Monad m => (a -> m b) -> FoldM m b r -> FoldM m a r
premapM f (FoldM step begin done) = FoldM step' begin done
  where
    step' x a = f a >>= step x
{-# INLINABLE premapM #-}

{-| @(postmapM f folder)@ returns a new 'FoldM' where f is applied to the final value.

> postmapM return = id
>
> postmapM (f >=> g) = postmapM g . postmapM f

> postmapM k (pure r) = k r
-}
postmapM :: Monad m => (a -> m r) -> FoldM m x a -> FoldM m x r
postmapM f (FoldM step begin done) = FoldM step begin done'
  where done' x = done x >>= f
{-# INLINABLE postmapM #-}

{-| @(prefilter f folder)@ returns a new 'Fold' where the folder's input is used
  only when the input satisfies a predicate f

  This can also be done with 'handles' (@handles (filtered f)@) but @prefilter@
  does not need you to depend on a lens library.

> fold (prefilter p folder) list = fold folder (filter p list)

>>> fold (prefilter (>5) Control.Foldl.sum) [1..10]
40

>>> fold Control.Foldl.sum (filter (>5) [1..10])
40
-}
prefilter :: (a -> Bool) -> Fold a r -> Fold a r
prefilter f (Fold step begin done) = Fold step' begin done
  where
    step' x a = if f a then step x a else x
{-# INLINABLE prefilter #-}

{-| @(prefilterM f folder)@ returns a new 'FoldM' where the folder's input is used
  only when the input satisfies a monadic predicate f.
-}
prefilterM :: (Monad m) => (a -> m Bool) -> FoldM m a r -> FoldM m a r
prefilterM f (FoldM step begin done) = FoldM step' begin done
  where
    step' x a = do
      use <- f a
      if use then step x a else return x
{-# INLINABLE prefilterM #-}

{-| Transforms a 'Fold' into one which ignores elements
    until they stop satisfying a predicate

> fold (predropWhile p folder) list = fold folder (dropWhile p list)

>>> fold (predropWhile (>5) Control.Foldl.sum) [10,9,5,9]
14
-}
predropWhile :: (a -> Bool) -> Fold a r -> Fold a r
predropWhile f (Fold step begin done) = Fold step' begin' done'
  where
    step' (Pair dropping x) a = if dropping && f a
      then Pair True x
      else Pair False (step x a)
    begin' = Pair True begin
    done' (Pair _ state) = done state
{-# INLINABLE predropWhile #-}

{-| @(drop n folder)@ returns a new 'Fold' that ignores the first @n@ inputs but
otherwise behaves the same as the original fold.

> fold (drop n folder) list = fold folder (Data.List.genericDrop n list)

>>> Foldl.fold (Foldl.drop 3 Foldl.sum) [10, 20, 30, 1, 2, 3]
6

>>> Foldl.fold (Foldl.drop 10 Foldl.sum) [10, 20, 30, 1, 2, 3]
0
-}

drop :: Natural -> Fold a b -> Fold a b
drop n (Fold step begin done) = Fold step' begin' done'
  where
    begin'          = (n, begin)
    step' (0,  s) x = (0, step s x)
    step' (n', s) _ = (n' - 1, s)
    done' (_,  s)   = done s
{-# INLINABLE drop #-}

{-| @(dropM n folder)@ returns a new 'FoldM' that ignores the first @n@ inputs but
otherwise behaves the same as the original fold.

> foldM (dropM n folder) list = foldM folder (Data.List.genericDrop n list)

>>> Foldl.foldM (Foldl.dropM 3 (Foldl.generalize Foldl.sum)) [10, 20, 30, 1, 2, 3]
6

>>> Foldl.foldM (Foldl.dropM 10 (Foldl.generalize Foldl.sum)) [10, 20, 30, 1, 2, 3]
0
-}

dropM :: Monad m => Natural -> FoldM m a b -> FoldM m a b
dropM n (FoldM step begin done) = FoldM step' begin' done'
  where
    begin'          = fmap (\s  -> (n, s))  begin
    step' (0,  s) x = fmap (\s' -> (0, s')) (step s x)
    step' (n', s) _ = return (n' - 1, s)
    done' (_,  s)   = done s
{-# INLINABLE dropM #-}

{-| A handler for the upstream input of a `Fold`

    Any lens, traversal, or prism will type-check as a `Handler`
-}
type Handler a b =
    forall x . (b -> Const (Dual (Endo x)) b) -> a -> Const (Dual (Endo x)) a

{-| @(handles t folder)@ transforms the input of a `Fold` using a lens,
    traversal, or prism:

> handles _1       :: Fold a r -> Fold (a, b) r
> handles _Left    :: Fold a r -> Fold (Either a b) r
> handles traverse :: Traversable t => Fold a r -> Fold (t a) r
> handles folded   :: Foldable    t => Fold a r -> Fold (t a) r

>>> fold (handles traverse sum) [[1..5],[6..10]]
55

>>> fold (handles (traverse . traverse) sum) [[Nothing, Just 2, Just 7],[Just 13, Nothing, Just 20]]
42

>>> fold (handles (filtered even) sum) [1..10]
30

>>> fold (handles _2 Foldl.mconcat) [(1,"Hello "),(2,"World"),(3,"!")]
"Hello World!"

> handles id = id
>
> handles (f . g) = handles f . handles g

> handles t (pure r) = pure r
>
> handles t (f <*> x) = handles t f <*> handles t x
-}
handles :: Handler a b -> Fold b r -> Fold a r
handles k (Fold step begin done) = Fold step' begin done
  where
    step' = flip (appEndo . getDual . getConst . k (Const . Dual . Endo . flip step))
{-# INLINABLE handles #-}

{- | @(foldOver f folder xs)@ folds all values from a Lens, Traversal, Prism or Fold with the given folder

>>> foldOver (_Just . both) Foldl.sum (Just (2, 3))
5

>>> foldOver (_Just . both) Foldl.sum Nothing
0

> Foldl.foldOver f folder xs == Foldl.fold folder (xs^..f)

> Foldl.foldOver (folded . f) folder == Foldl.fold (handles f folder)

> Foldl.foldOver folded == Foldl.fold

-}
foldOver :: Handler s a -> Fold a b -> s -> b
foldOver l (Fold step begin done) =
  done . flip appEndo begin . getDual . getConst . l (Const . Dual . Endo . flip step)
{-# INLINABLE foldOver #-}

{-|
> instance Monad m => Monoid (EndoM m a) where
>     mempty = EndoM return
>     mappend (EndoM f) (EndoM g) = EndoM (f <=< g)
-}
newtype EndoM m a = EndoM { appEndoM :: a -> m a }

instance Monad m => Semigroup (EndoM m a) where
    (EndoM f) <> (EndoM g) = EndoM (f <=< g)
    {-# INLINE (<>) #-}

instance Monad m => Monoid (EndoM m a) where
    mempty = EndoM return
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

{-| A Handler for the upstream input of `FoldM`

    Any lens, traversal, or prism will type-check as a `HandlerM`
-}
type HandlerM m a b =
    forall x . (b -> Const (Dual (EndoM m x)) b) -> a -> Const (Dual (EndoM m x)) a

{-| @(handlesM t folder)@ transforms the input of a `FoldM` using a lens,
    traversal, or prism:

> handlesM _1       :: FoldM m a r -> FoldM (a, b) r
> handlesM _Left    :: FoldM m a r -> FoldM (Either a b) r
> handlesM traverse :: Traversable t => FoldM m a r -> FoldM m (t a) r
> handlesM folded   :: Foldable    t => FoldM m a r -> FoldM m (t a) r

    `handlesM` obeys these laws:

> handlesM id = id
>
> handlesM (f . g) = handlesM f . handlesM g

> handlesM t (pure r) = pure r
>
> handlesM t (f <*> x) = handlesM t f <*> handlesM t x
-}
handlesM :: HandlerM m a b -> FoldM m b r -> FoldM m a r
handlesM k (FoldM step begin done) = FoldM step' begin done
  where
    step' = flip (appEndoM . getDual . getConst . k (Const . Dual . EndoM . flip step))
{-# INLINABLE handlesM #-}

{- | @(foldOverM f folder xs)@ folds all values from a Lens, Traversal, Prism or Fold monadically with the given folder

> Foldl.foldOverM (folded . f) folder == Foldl.foldM (handlesM f folder)

> Foldl.foldOverM folded == Foldl.foldM

-}
foldOverM :: Monad m => HandlerM m s a -> FoldM m a b -> s -> m b
foldOverM l (FoldM step begin done) s = do
  b <- begin
  r <- (flip appEndoM b . getDual . getConst . l (Const . Dual . EndoM . flip step)) s
  done r
{-# INLINABLE foldOverM #-}

{-|
> folded :: Foldable t => Fold (t a) a
>
> handles folded :: Foldable t => Fold a r -> Fold (t a) r
-}
folded
    :: (Contravariant f, Applicative f, Foldable t)
    => (a -> f a) -> (t a -> f (t a))
folded k ts = contramap (\_ -> ()) (F.traverse_ k ts)
{-# INLINABLE folded #-}

{-|
>>> fold (handles (filtered even) sum) [1..10]
30

>>> foldM (handlesM (filtered even) (Foldl.mapM_ print)) [1..10]
2
4
6
8
10

-}
filtered :: Monoid m => (a -> Bool) -> (a -> m) -> a -> m
filtered p k x
    | p x = k x
    | otherwise = mempty
{-# INLINABLE filtered #-}

{-| Perform a 'Fold' while grouping the data according to a specified group
projection function. Returns the folded result grouped as a map keyed by the
group.

-}
groupBy :: Ord k => (a -> k) -> Fold a b -> Fold a (Map k b)
groupBy f = premap (\(!a) -> (f a, a)) . foldByKeyMap
{-# INLINABLE groupBy #-}

{-| Combine two folds into a fold over inputs for either of them.
-}
either :: Fold a1 b1 -> Fold a2 b2 -> Fold (Either a1 a2) (b1, b2)
either l r = (,) <$> handles _Left l <*> handles _Right r
{-# INLINABLE either #-}

{-| Combine two monadic folds into a fold over inputs for either of them.
-}
eitherM :: Monad m => FoldM m a1 b1 -> FoldM m a2 b2 -> FoldM m (Either a1 a2) (b1, b2)
eitherM l r = (,) <$> handlesM _Left l <*> handlesM _Right r
{-# INLINABLE eitherM #-}

{-| Nest a fold in an applicative.
-}
nest :: Applicative f => Fold a b -> Fold (f a) (f b)
nest (Fold s i e) =
    Fold (\xs as -> liftA2 s xs as)
         (pure i)
         (\xs -> fmap e xs)
{-# INLINABLE nest #-}

{- $reexports
    @Control.Monad.Primitive@ re-exports the 'PrimMonad' type class

    @Data.Foldable@ re-exports the 'Foldable' type class

    @Data.Vector.Generic@ re-exports the 'Vector' type class
-}
