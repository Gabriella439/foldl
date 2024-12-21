{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-| This module provides a `Fold1` type that is a \"non-empty\" analog of the
    `Fold` type, meaning that it requires at least one input element in order to
    produce a result

    This module does not provide all of the same utilities as the
    "Control.Foldl" module.  Instead, this module only provides the utilities
    which can make use of the non-empty input guarantee (e.g. `head`).  For
    all other utilities you can convert them from the equivalent `Fold` using
    `fromFold`.

    Import this module qualified to avoid clashing with the Prelude:

>>> import qualified Control.Foldl.NonEmpty as Foldl1

    Use 'fold1' to apply a 'Fold1' to a non-empty list:

>>> Foldl1.fold1 Foldl1.last (1 :| [2..10])
10

-}

module Control.Foldl.NonEmpty (
    -- * Fold Types
      Fold1(.., Fold1_)
    , FoldM1(FoldM1, FoldM1_)

    -- * Folding
    , Control.Foldl.NonEmpty.fold1
    , foldM1

    -- * Conversion between Fold and Fold1
    , fromFold
    , toFold
    , fromFoldM
    , toFoldM

    -- * Folds
    , sconcat
    , head
    , last
    , maximum
    , maximumBy
    , minimum
    , minimumBy

    -- ** Non-empty Container Folds
    , nonEmpty

    -- * Utilities
    , purely
    , purely_
    , impurely
    , impurely_
    , generalize
    , simplify
    , lifts
    , hoists
    , duplicateM
    , premap
    , premapM
    , postmapM
    , FromMaybe(..)
    , Handler1
    , handles
    , foldOver
    , FromMaybeM(..)
    , HandlerM1
    , handlesM
    , foldOverM
    , folded1
    , nest
    ) where

import Control.Applicative (liftA2, Const(..))
import Control.Arrow (Arrow (..))
import Control.Category (Category ())
import qualified Control.Category
import Control.Comonad (Comonad(..))
import Control.Foldl (Fold(..), FoldM(..))
import Control.Foldl.Internal (Either'(..))
import Control.Monad ((<=<))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Dual(..))
import Data.Functor.Apply (Apply (..))
import Data.Functor.Extend (Extend (..))
import Data.Profunctor
import Data.Profunctor.Sieve (Cosieve(..))
import Data.Semigroup.Foldable (Foldable1(..), traverse1_)
import Data.Semigroupoid (Semigroupoid (..))
import Data.Functor.Identity
import Data.Functor.Contravariant (Contravariant(..))

import Prelude hiding (head, last, minimum, maximum)

import qualified Control.Foldl as Foldl

{- $setup

>>> import qualified Control.Foldl.NonEmpty as Foldl1
>>> import qualified Data.List.NonEmpty as NonEmpty
>>> import Data.Functor.Apply (Apply(..))
>>> import Data.Semigroup.Traversable (Traversable1(..))
>>> import Data.Monoid (Sum(..))

>>> _2 f (x, y) = fmap (\i -> (x, i)) (f y)

>>> both1 f (x, y) = (,) <$> f x <.> f y

-}

{-| A `Fold1` is like a `Fold` except that it consumes at least one input
    element
-}
data Fold1 a b = Fold1 (a -> Fold a b)

{-| @Fold1_@ is an alternative to the @Fold1@ constructor if you need to
    explicitly work with an initial, step and extraction function.

    @Fold1_@ is similar to the @Fold@ constructor, which also works with an
    initial, step and extraction function. However, note that @Fold@ takes the
    step function as the first argument and the initial accumulator as the
    second argument, whereas @Fold1_@ takes them in swapped order:

    @Fold1_ @ @ initial @ @ step @ @ extract@

    While @Fold@ resembles 'Prelude.foldl', @Fold1_@ resembles
    'Data.Foldable1.foldlMap1'.
-}
pattern Fold1_ :: forall a b. forall x. (a -> x) -> (x -> a -> x) -> (x -> b) -> Fold1 a b
pattern Fold1_ begin step done <- (toFold_ -> (begin, step, done))
  where Fold1_ begin step done = Fold1 $ \a -> Fold step (begin a) done
#if __GLASGOW_HASKELL__ >= 902
{-# INLINABLE Fold1_ #-}
#endif
{-# COMPLETE Fold1_ :: Fold1 #-}

toFold_ :: Fold1 a b -> (a -> Fold a b, Fold a b -> a -> Fold a b, Fold a b -> b)
toFold_ (Fold1 (f :: a -> Fold a b)) = (begin', step', done')
  where
    done' :: Fold a b -> b
    done' (Fold _step begin done) = done begin

    step' :: Fold a b -> a -> Fold a b
    step' (Fold step begin done) a = Fold step (step begin a) done

    begin' :: a -> Fold a b
    begin' = f
{-# INLINABLE toFold_ #-}

instance Functor (Fold1 a) where
    fmap f (Fold1 k) = Fold1 (fmap (fmap f) k)
    {-# INLINE fmap #-}

instance Profunctor Fold1 where
    lmap = premap
    {-# INLINE lmap #-}

    rmap = fmap
    {-# INLINE rmap #-}

instance Choice Fold1 where
    right' = nest
    {-# INLINE right' #-}

instance Closed Fold1 where
    closed = nest
    {-# INLINE closed #-}

instance Cosieve Fold1 NonEmpty where
    cosieve = Control.Foldl.NonEmpty.fold1
    {-# INLINE cosieve #-}

instance Applicative (Fold1 a) where
    pure b = Fold1 (pure (pure b))
    {-# INLINE pure #-}

    Fold1 l <*> Fold1 r = Fold1 (liftA2 (<*>) l r)
    {-# INLINE (<*>) #-}

instance Extend (Fold1 a) where
    duplicated (Fold1 f) = Fold1 $ fmap fromFold . duplicated . f
    {-# INLINE duplicated #-}

instance Semigroup b => Semigroup (Fold1 a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance Monoid b => Monoid (Fold1 a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance Semigroupoid Fold1 where
    o (Fold1 l1) (Fold1 r1) = Fold1 f1
      where
        f1 a = let r = r1 a
                   l = l1 $ extract r
               in o l r
    {-# INLINE o #-}

instance Category Fold1 where
    (.) = o
    {-# INLINE (.) #-}

    id = last
    {-# INLINE id #-}

instance Strong Fold1 where
    first' f = (,) <$> lmap fst f <*> lmap snd last
    {-# INLINE first' #-}

instance Arrow Fold1 where
    arr f = f <$> last
    {-# INLINE arr #-}

    first = first'
    {-# INLINE first #-}

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

{-| Like t'Fold1', but monadic.

    A \''FoldM1' m a b\' processes elements of type __a__ and
    results in a monadic value of type __m b__. A t'FoldM1' is similar
    to a t'FoldM' except that it consumes at least one input
    element.
-}
data FoldM1 m a b =
  FoldM1' (a -> FoldM m a b)

{-| Like v'Fold1', but monadic.
-}
pattern FoldM1 :: Monad m => (a -> FoldM m a b) -> FoldM1 m a b
pattern FoldM1 f = FoldM1' f
#if __GLASGOW_HASKELL__ >= 902
{-# INLINABLE FoldM1 #-}
#endif
{-# COMPLETE FoldM1 :: FoldM1 #-}

{-| Like 'Fold1_', but monadic.
-}
pattern FoldM1_ :: forall m a b. Monad m => forall x. (a -> m x) -> (x -> a -> m x) -> (x -> m b) -> FoldM1 m a b
pattern FoldM1_ begin step done <- (toFoldM_ -> (begin, step, done))
  where FoldM1_ begin step done = FoldM1 $ \a -> FoldM step (begin a) done
#if __GLASGOW_HASKELL__ >= 902
{-# INLINABLE FoldM1_ #-}
#endif
{-# COMPLETE FoldM1_ :: FoldM1 #-}

toFoldM_ :: Monad m => FoldM1 m a b -> (a -> m (FoldM m a b), FoldM m a b -> a -> m (FoldM m a b), FoldM m a b -> m b)
toFoldM_ (FoldM1 (f :: a -> FoldM m a b)) = (begin', step', done')
  where
    done' :: FoldM m a b -> m b
    done' (FoldM _step begin done) = do
      x <- begin
      done x

    step' :: FoldM m a b -> a -> m (FoldM m a b)
    step' (FoldM step begin done) a = pure $ FoldM step begin'' done
        where
          begin'' = do
            x <- begin
            step x a

    begin' :: a -> m (FoldM m a b)
    begin' = pure . f
{-# INLINABLE toFoldM_ #-}

instance Functor m => Functor (FoldM1 m a) where
    fmap f (FoldM1' k) = FoldM1' (fmap (fmap f) k)
    {-# INLINE fmap #-}

instance Functor m => Profunctor (FoldM1 m) where
    lmap f (FoldM1' k) = FoldM1' k'
      where
        k' a = lmap f (k (f a))
    {-# INLINE lmap #-}

    rmap = fmap
    {-# INLINE rmap #-}

instance Applicative m => Applicative (FoldM1 m a) where
    pure b = FoldM1' (pure (pure b))
    {-# INLINE pure #-}

    FoldM1' l <*> FoldM1' r = FoldM1' (liftA2 (<*>) l r)
    {-# INLINE (<*>) #-}

instance Monad m => Extend (FoldM1 m a) where
    duplicated = duplicateM
    {-# INLINE duplicated #-}

instance (Semigroup b, Monad m) => Semigroup (FoldM1 m a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance (Monoid b, Monad m) => Monoid (FoldM1 m a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance (Monad m, Num b) => Num (FoldM1 m a b) where
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

instance (Monad m, Fractional b) => Fractional (FoldM1 m a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance (Monad m, Floating b) => Floating (FoldM1 m a b) where
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

-- | Like 'fold1', but monadic
foldM1 :: (Foldable1 t, Monad m) => FoldM1 m a b -> t a -> m b
foldM1 (FoldM1 k) as1 = Foldl.foldM (k a) as
  where
    a :| as = toNonEmpty as1
{-# INLINE foldM1 #-}

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

-- | Promote any `FoldM` to an equivalent `FoldM1`
fromFoldM :: Monad m => FoldM m a b -> FoldM1 m a b
fromFoldM (FoldM step begin done) = FoldM1 f
  where
    f a = FoldM step begin' done
      where
        begin' = do
            x <- begin
            step x a
{-# INLINABLE fromFoldM #-}

-- | Promote any `FoldM1` to an equivalent `FoldM`
toFoldM :: Monad m => FoldM1 m a b -> FoldM m a (Maybe b)
toFoldM (FoldM1 k0) = FoldM step begin done
  where
    begin = pure $ Left' k0

    step e a = pure $ case e of
      Left' k                           -> Right' (k a)
      Right' (FoldM step' begin' done') -> Right' (FoldM step' begin'' done')
        where
          begin'' = do
            x <- begin'
            step' x a

    done e = case e of
      Left' _                       -> pure Nothing
      Right' (FoldM _ begin' done') -> do
        x <- begin'
        Just <$> done' x
{-# INLINABLE toFoldM #-}

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

-- | Upgrade a \"non-empty\" fold to accept the 'Fold1' type
purely :: (forall x . (a -> x) -> (x -> a -> x) -> (x -> b) -> r) -> Fold1 a b -> r
purely f (Fold1_ begin step done) = f begin step done
{-# INLINABLE purely #-}

-- | Upgrade a more traditional \"non-empty\" fold to accept the `Fold1` type
purely_ :: (forall x . (a -> x) -> (x -> a -> x) -> x) -> Fold1 a b -> b
purely_ f (Fold1_ begin step done) = done (f begin step)
{-# INLINABLE purely_ #-}

-- | Upgrade a monadic \"non-empty\" fold to accept the 'FoldM1' type
impurely
    :: Monad m
    => (forall x . (a -> m x) -> (x -> a -> m x) -> (x -> m b) -> r)
    -> FoldM1 m a b
    -> r
impurely f (FoldM1_ begin step done) = f begin step done
{-# INLINABLE impurely #-}

-- | Upgrade a more traditional monadic \"non-empty\" fold to accept the `FoldM1` type
impurely_
    :: Monad m
    => (forall x . (a -> m x) -> (x -> a -> m x) -> m x) -> FoldM1 m a b -> m b
impurely_ f (FoldM1_ begin step done) = do
    x <- f begin step
    done x
{-# INLINABLE impurely_ #-}

{-| Generalize a `Fold1` to a `FoldM1`

> generalize (pure r) = pure r
>
> generalize (f <*> x) = generalize f <*> generalize x
-}
generalize :: Monad m => Fold1 a b -> FoldM1 m a b
generalize (Fold1 f) = FoldM1 $ Foldl.generalize . f
{-# INLINABLE generalize #-}

{-| Simplify a pure `FoldM1` to a `Fold1`

> simplify (pure r) = pure r
>
> simplify (f <*> x) = simplify f <*> simplify x
-}
simplify :: FoldM1 Identity a b -> Fold1 a b
simplify (FoldM1 f) = Fold1 $ Foldl.simplify . f
{-# INLINABLE simplify #-}

{- | Shift a 'FoldM1' from one monad to another with a morphism such as 'lift' or 'liftIO';
     the effect is the same as 'Control.Monad.Morph.hoist'.
-}
hoists :: (forall x. m x -> n x) -> FoldM1 m a b -> FoldM1 n a b
hoists phi (FoldM1' f) = FoldM1' $ Foldl.hoists phi . f
{-# INLINABLE hoists #-}

{- | Lift a monadic value to a 'FoldM1';
     works like 'Control.Monad.Trans.Class.lift'.

> lifts . pure = pure
-}
lifts :: Monad m => m b -> FoldM1 m a b
lifts mb = FoldM1 $ \_ -> Foldl.lifts mb

{-| Allows to continue feeding a 'FoldM1' even after passing it to a function
that closes it.

For pure 'Fold's, this is provided by the 'Control.Comonad.Comonad' instance.
-}
duplicateM :: Monad m => FoldM1 m a b -> FoldM1 m a (FoldM1 m a b)
duplicateM (FoldM1 f) = FoldM1 $ fmap fromFoldM . Foldl.duplicateM . f
{-# INLINABLE duplicateM #-}

{-| @(premap f folder)@ returns a new 'Fold1' where f is applied at each step

> Foldl1.fold1 (premap f folder) list = Foldl1.fold1 folder (NonEmpty.map f list)

>>> Foldl1.fold1 (premap Sum Foldl1.sconcat) (1 :| [2..10])
Sum {getSum = 55}

>>> Foldl1.fold1 Foldl1.sconcat $ NonEmpty.map Sum (1 :| [2..10])
Sum {getSum = 55}

> premap id = id
>
> premap (f . g) = premap g . premap f

> premap k (pure r) = pure r
>
> premap k (f <*> x) = premap k f <*> premap k x
-}
premap :: (a -> b) -> Fold1 b r -> Fold1 a r
premap f (Fold1 k) = Fold1 k'
  where
    k' a = lmap f (k (f a))
{-# INLINABLE premap #-}

{-| @(premapM f folder)@ returns a new 'FoldM1' where f is applied to each input
    element

> premapM return = id
>
> premapM (f <=< g) = premap g . premap f

> premapM k (pure r) = pure r
>
> premapM k (f <*> x) = premapM k f <*> premapM k x
-}
premapM :: forall m a b r. Monad m => (a -> m b) -> FoldM1 m b r -> FoldM1 m a r
premapM f (FoldM1_ begin step done) = FoldM1_ begin' step' done
  where
    begin' a = f a >>= begin
    step' x a = f a >>= step x
{-# INLINABLE premapM #-}

{-| @(postmapM f folder)@ returns a new 'FoldM1' where f is applied to the final value.

> postmapM pure = id
>
> postmapM (f >=> g) = postmapM g . postmapM f

> postmapM k (pure r) = lifts (k r)
-}
postmapM :: Monad m => (a -> m r) -> FoldM1 m x a -> FoldM1 m x r
postmapM phi (FoldM1 f) = FoldM1 $ Foldl.postmapM phi . f
{-# INLINABLE postmapM #-}

{-|
> instance Monad m => Semigroup (FromMaybe m a) where
>     mappend (FromMaybe f) (FromMaybe g) = FromMaybeM (f . Just . g)
-}
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . (Just $!) . g)
    {-# INLINE (<>) #-}

{-| A handler for the upstream input of a `Fold1`

    This is compatible with van Laarhoven optics as defined in the lens package.
    Any Lens, Fold1 or Traversal1 will type-check as a `Handler1`.
-}
type Handler1 a b =
    forall x. (b -> Const (Dual (FromMaybe x)) b) -> a -> Const (Dual (FromMaybe x)) a

{-| @(handles t folder)@ transforms the input of a `Fold1` using a Lens,
    Traversal1, or Fold1 optic:

> handles _1        :: Fold1 a r -> Fold1 (a, b) r
> handles traverse1 :: Traversable1 t => Fold1 a r -> Fold1 (t a) r
> handles folded1   :: Foldable1    t => Fold1 a r -> Fold1 (t a) r

>>> Foldl1.fold1 (handles traverse1 Foldl1.nonEmpty) $ (1 :| [2..4]) :| [ 5 :| [6,7], 8 :| [9,10] ]
1 :| [2,3,4,5,6,7,8,9,10]

>>> Foldl1.fold1 (handles _2 Foldl1.sconcat) $ (1,"Hello ") :| [(2,"World"),(3,"!")]
"Hello World!"

> handles id = id
>
> handles (f . g) = handles f . handles g

> handles t (pure r) = pure r
>
> handles t (f <*> x) = handles t f <*> handles t x
-}
handles :: forall a b r. Handler1 a b -> Fold1 b r -> Fold1 a r
handles k (Fold1_ begin step done) = Fold1_ begin' step' done
  where
    begin' = stepAfromMaybe Nothing
    step' x = stepAfromMaybe (Just $! x)
    stepAfromMaybe = flip (appFromMaybe . getDual . getConst . k (Const . Dual . FromMaybe . flip stepBfromMaybe))
    stepBfromMaybe = maybe begin step
{-# INLINABLE handles #-}

{- | @(foldOver f folder xs)@ folds all values from a Lens, Traversal1 or Fold1 optic with the given folder

>>> foldOver (_2 . both1) Foldl1.nonEmpty (1, (2, 3))
2 :| [3]

> Foldl1.foldOver f folder xs == Foldl1.fold1 folder (xs ^.. f)

> Foldl1.foldOver (folded1 . f) folder == Foldl1.fold1 (Foldl1.handles f folder)

> Foldl1.foldOver folded1 == Foldl1.fold1

-}
foldOver :: Handler1 s a -> Fold1 a b -> s -> b
foldOver l (Fold1_ begin step done) =
    done . stepSfromMaybe Nothing
  where
    stepSfromMaybe = flip (appFromMaybe . getDual . getConst . l (Const . Dual . FromMaybe . flip stepAfromMaybe))
    stepAfromMaybe = maybe begin step
{-# INLINABLE foldOver #-}

{-|
> instance Monad m => Semigroup (FromMaybeM m a) where
>     mappend (FromMaybeM f) (FromMaybeM g) = FromMaybeM (f . Just <=< g)
-}
newtype FromMaybeM m a = FromMaybeM { appFromMaybeM :: Maybe a -> m a }

instance Monad m => Semigroup (FromMaybeM m a) where
    FromMaybeM f <> FromMaybeM g = FromMaybeM (f . (Just $!) <=< g)
    {-# INLINE (<>) #-}

{-| A Handler for the upstream input of `FoldM`

    This is compatible with van Laarhoven optics as defined in the lens package.
    Any Lens, Fold1 or Traversal1 will type-check as a `HandlerM1`
-}
type HandlerM1 m a b =
    forall x . (b -> Const (Dual (FromMaybeM m x)) b) -> a -> Const (Dual (FromMaybeM m x)) a

{-| @(handlesM t folder)@ transforms the input of a `FoldM1` using a Lens,
    Traversal1 or Fold1 optic:

> handlesM _1        :: FoldM1 m a r -> FoldM1 (a, b) r
> handlesM traverse1 :: Traversable1 t => FoldM1 m a r -> FoldM1 m (t a) r
> handlesM folded1   :: Foldable1    t => FoldM1 m a r -> FoldM1 m (t a) r

    `handlesM` obeys these laws:

> handlesM id = id
>
> handlesM (f . g) = handlesM f . handlesM g

> handlesM t (pure r) = pure r
>
> handlesM t (f <*> x) = handlesM t f <*> handlesM t x
-}
handlesM :: Monad m => HandlerM1 m a b -> FoldM1 m b r -> FoldM1 m a r
handlesM k (FoldM1_ begin step done) = FoldM1_ begin' step' done
  where
    begin' = stepMaybeA Nothing
    step' b = stepMaybeA (Just $! b)
    stepMaybeA = flip (appFromMaybeM . getDual . getConst . k (Const . Dual . FromMaybeM . flip stepMaybeB))
    stepMaybeB = maybe begin step
{-# INLINABLE handlesM #-}

{- | @(foldOverM f folder xs)@ folds all values from a Lens, Traversal1 or
     Fold1 optic monadically with the given folder

> Foldl1.foldOverM (folded1 . f) folder == Foldl1.foldM1 (handlesM f folder)

> Foldl1.foldOverM folded1 == Foldl1.foldM1

-}
foldOverM :: Monad m => HandlerM1 m s a -> FoldM1 m a b -> s -> m b
foldOverM l (FoldM1_ (begin :: a -> m x) (step :: x -> a -> m x) (done :: x -> m b)) s = do
    r <- stepMaybeS Nothing s
    done r
  where
    stepMaybeS = flip (appFromMaybeM . getDual . getConst . l (Const . Dual . FromMaybeM . flip stepMaybeA))
    stepMaybeA = maybe begin step
{-# INLINABLE foldOverM #-}

{-|
> handles folded1 :: Foldable1 t => Fold1 a r -> Fold1 (t a) r
-}
folded1
    :: (Contravariant f, Apply f, Foldable1 t)
    => (a -> f a) -> (t a -> f (t a))
folded1 k ts = contramap (\_ -> ()) (traverse1_ k ts)
{-# INLINABLE folded1 #-}

{-| Nest a fold in an Apply.
-}
nest :: Apply f => Fold1 a b -> Fold1 (f a) (f b)
nest (Fold1_ i s e) =
    Fold1_
        (fmap i)
        (liftF2 s)
        (fmap e)
{-# INLINABLE nest #-}
