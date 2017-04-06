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

    Taking the sum, the sum of squares, ..., upto the sum of x^5

>>> import Data.Traversable
>>> let powerSums = sequenceA [L.premap (^n) L.sum | n <- [1..5]]
>>> L.fold powerSums [1..10]
[55,385,3025,25333,220825]

    These combined folds will still traverse the list only once, streaming
    efficiently over the list in constant space without space leaks:

>>> L.fold average [1..10000000]
5000000.5
>>> L.fold ((,) <$> L.minimum <*> L.maximum) [1..10000000]
(Just 1,Just 10000000)

    You might want to try enabling the @-flate-dmd-anal@ flag when compiling
    executables that use this library to further improve performance.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE Trustworthy               #-}

module Control.Foldl (
    -- * Fold Types
      Fold(..)
    , FoldM(..)

    -- * Folding
    , fold
    , foldM
    , scan

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

    -- * Generic Folds
    , genericLength
    , genericIndex

    -- * Container folds
    , list
    , revList
    , nub
    , eqNub
    , set
    , vector

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
    , Handler
    , handles
    , foldOver
    , EndoM(..)
    , HandlerM
    , handlesM
    , foldOverM
    , folded
    , filtered

    -- * Re-exports
    -- $reexports
    , module Control.Monad.Primitive
    , module Data.Foldable
    , module Data.Vector.Generic
    ) where

import Control.Applicative
import Control.Foldl.Internal (Maybe'(..), lazy, Either'(..), hush)
import Control.Monad ((<=<))
import Control.Monad.Primitive (PrimMonad, RealWorld)
import Control.Comonad
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Monoid
import Data.Profunctor
import Data.Sequence ((|>))
import Data.Vector.Generic (Vector, Mutable)
import Data.Vector.Generic.Mutable (MVector)
import System.Random.MWC (GenIO, createSystemRandom, uniformR)
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
    )

import qualified Data.Foldable               as F
import qualified Data.List                   as List
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set
import qualified Data.Map.Strict             as Map
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as M

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

data Pair a b = Pair !a !b

instance Functor (Fold a) where
    fmap f (Fold step begin done) = Fold step begin (f . done)
    {-# INLINE fmap #-}

instance Profunctor Fold where
    lmap = premap
    rmap = fmap

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

instance Monoid b => Monoid (Fold a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = liftA2 mappend
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

    asin = fmap sin
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

instance Monad m => Functor (FoldM m a) where
    fmap f (FoldM step start done) = FoldM step start done'
      where
        done' x = do
            b <- done x
            return $! f b
    {-# INLINE fmap #-}

instance Monad m => Applicative (FoldM m a) where
    pure b = FoldM (\() _ -> return ()) (return ()) (\() -> return b)
    {-# INLINE pure #-}

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
    {-# INLINE (<*>) #-}

instance Monad m => Profunctor (FoldM m) where
    rmap = fmap
    lmap = premapM

instance (Monoid b, Monad m) => Monoid (FoldM m a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = liftA2 mappend
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

    asin = fmap sin
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

-- | Convert a strict left 'Fold' into a scan
scan :: Fold a b -> [a] -> [b]
scan (Fold step begin done) as = foldr cons nil as begin
  where
    nil      x = done x:[]
    cons a k x = done x:(k $! step x a)
{-# INLINE scan #-}

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
    step (Pair x n) y = Pair ((x * n + y) / (n + 1)) (n + 1)
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
        g <- createSystemRandom
        return $! Pair3 g Nothing' (1 :: Int)

    step (Pair3 g Nothing'  _) a = return $! Pair3 g (Just' a) 2
    step (Pair3 g (Just' a) m) b = do
        n <- uniformR (1, m) g
        let c = if n == 1 then b else a
        return $! Pair3 g (Just' c) (m + 1)

    done (Pair3 _ ma _) = return (lazy ma)
{-# INLINABLE random #-}

data VectorState = Incomplete {-# UNPACK #-} !Int | Complete

data RandomNState v a = RandomNState
    { _size      ::                !VectorState
    , _reservoir ::                !(Mutable v RealWorld a)
    , _position  :: {-# UNPACK #-} !Int
    , _gen       :: {-# UNPACK #-} !GenIO
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
        r <- uniformR (0, i - 1) g
        if r < n
            then M.unsafeWrite mv r a
            else return ()
        return (RandomNState Complete mv (i + 1) g)

    begin = do
        mv  <- M.new n
        gen <- createSystemRandom
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

maxChunkSize :: Int
maxChunkSize = 8 * 1024 * 1024

-- | Fold all values into a vector
vector :: (PrimMonad m, Vector v a) => FoldM m a (v a)
vector = FoldM step begin done
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
{-# INLINABLE vector #-}

{-|
Fold pairs into a map.
-}
map :: Ord a => Fold (a, b) (Map.Map a b)
map = Fold step begin done
  where
    begin = id
    step fn (k, v) = M.insert k v . fn
    done fn = fn mempty

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

> fold (premap f folder) list = fold folder (map f list)

>>> fold (premap Sum mconcat) [1..10]
Sum {getSum = 55}

>>> fold mconcat (map Sum [1..10])
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

> foldM (premapM f folder) list = foldM folder (map f list)

> premapM id = id
>
> premapM (f . g) = premap g . premap f

> premapM k (pure r) = pure r
>
> premapM k (f <*> x) = premapM k f <*> premapM k x
-}
premapM :: (a -> b) -> FoldM m b r -> FoldM m a r
premapM f (FoldM step begin done) = FoldM step' begin done
  where
    step' x a = step x (f a)
{-# INLINABLE premapM #-}

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

>>> fold (handles (traverse.traverse) sum) [[Nothing, Just 2, Just 7],[Just 13, Nothing, Just 20]]
42

>>> fold (handles (filtered even) sum) [1..10]
30

>>> fold (handles _2 mconcat) [(1,"Hello "),(2,"World"),(3,"!")]
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

>>> foldOver (_Just . both) L.sum (Just (2, 3))
5

>>> foldOver (_Just . both) L.sum Nothing
0

> L.foldOver f folder xs == L.fold folder (xs^..f)

> L.foldOver (folded.f) folder == L.fold (handles f folder)

> L.foldOver folded == L.fold

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

instance Monad m => Monoid (EndoM m a) where
    mempty = EndoM return
    {-# INLINE mempty #-}

    mappend (EndoM f) (EndoM g) = EndoM (f <=< g)
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

> L.foldOverM (folded.f) folder == L.foldM (handlesM f folder)

> L.foldOverM folded == L.foldM

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

>>> foldM (handlesM (filtered even) (mapM_ print)) [1..10]
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

{- $reexports
    @Control.Monad.Primitive@ re-exports the 'PrimMonad' type class

    @Data.Foldable@ re-exports the 'Foldable' type class

    @Data.Vector.Generic@ re-exports the 'Vector' type class
-}
