{-| This module provides efficient and streaming left map-with-accumulator that
    you can combine using 'Applicative' style.

    Import this module qualified to avoid clashing with the Prelude:

>>> import qualified Control.Scanl as SL

    Use 'scan' to apply a 'Fold' to a list (or other 'Traversable' structures)
    from left to right, and 'scanr' to do so from right to left.

    Note that the `Scan` type does not supersede the `Fold` type nor does the
    `Fold` type supersede the `Scan` type.  Each type has a unique advantage.

    For example, `Scan`s can be chained end-to-end:

    > (>>>) :: Scan a b -> Scan b c -> Scan a c

    In other words, `Scan` is an instance of the `Category` typeclass.

    `Fold`s cannot be chained end-to-end

    Vice versa, `Fold`s can produce a result even when fed no input:

    > extract :: Fold a b -> b

    In other words, `Fold` is an instance of the `Comonad` typeclass.

    A `Scan`s cannot produce any output until provided with at least one
    input.
-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}

module Control.Scanl (
    -- * Scan Types
      Scan(..)
    , ScanM(..)

    -- * Scanning
    , scan
    , scanM
    , scanr

    , prescan
    , postscan

    -- * Utilities
    -- $utilities
    , purely
    , purely_
    , impurely
    , impurely_
    , generalize
    , simplify
    , hoists
    , arrM
    , premap
    , premapM
    ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Foldl (Fold(..))
import Control.Foldl.Internal (Pair(..))
import Control.Monad ((<=<))
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.Monoid hiding ((<>))
import Data.Profunctor
import Data.Semigroup (Semigroup(..))
import Data.Traversable
import Data.Tuple (swap)
import Prelude hiding ((.), id, scanr)

#if MIN_VERSION_base(4, 7, 0)
import Data.Coerce
#endif

asLazy :: StateT s m a -> Lazy.StateT s m a
asLazy = Lazy.StateT . runStateT

--import qualified Control.Foldl as L

{-| Efficient representation of a left map-with-accumulator that preserves the
    scan's step function and initial accumulator.

    This allows the 'Applicative' instance to assemble derived scans that
    traverse the container only once

    A \''Scan' a b\' processes elements of type __a__ replacing each with a
    value of type __b__.
-}
data Scan a b
  -- | @Scan @ @ step @ @ initial @
  = forall x. Scan (a -> State x b) x

instance Functor (Scan a) where
    fmap f (Scan step begin) = Scan (fmap f . step) begin
    {-# INLINE fmap #-}

instance Applicative (Scan a) where
    pure b    = Scan (\_ -> pure b) ()
    {-# INLINE pure #-}

    (Scan stepL beginL) <*> (Scan stepR beginR) =
        let step a (Pair xL xR) = (bL bR, (Pair xL' xR'))
              where (bL, xL') = runState (stepL a) xL
                    (bR, xR') = runState (stepR a) xR
            begin = Pair beginL beginR
        in  Scan (state . step) begin
    {-# INLINE (<*>) #-}

instance Profunctor Scan where
    lmap = premap
    rmap = fmap

instance Category Scan where
    id = Scan pure ()
    {-# INLINE id #-}
    (Scan s2 b2) . (Scan s1 b1) = Scan (state . step) (Pair b1 b2)
        where step a (Pair xL xR) = (c, Pair xL' xR')
                where (b, xL') = runState (s1 a) xL
                      (c, xR') = runState (s2 b) xR
    {-# INLINE (.) #-}

instance Arrow Scan where
    arr f = Scan (pure . f) ()
    {-# INLINE arr #-}
    first  (Scan step begin) = Scan
      (\(a,b) -> state $ \x -> first (,b) $ runState (step a) x)
      begin
    {-# INLINE first #-}
    second (Scan step begin) = Scan
      (\(b,a) -> state $ \x  -> first (b,) $ runState (step a) x)
      begin
    {-# INLINE second #-}

instance Semigroup b => Semigroup (Scan a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance Monoid b => Monoid (Scan a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = liftA2 mappend
    {-# INLINE mappend #-}

instance Num b => Num (Scan a b) where
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

instance Fractional b => Fractional (Scan a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance Floating b => Floating (Scan a b) where
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

{-| Like 'Scan', but monadic.

    A \''ScanM' m a b\' processes elements of type __a__ and
    results in a monadic value of type __m b__.
-}
data ScanM m a b =
  -- | @ScanM @ @ step @ @ initial @ @ extract@
  forall x . ScanM (a -> StateT x m b) (m x)

instance Functor m => Functor (ScanM m a) where
    fmap f (ScanM step begin) = ScanM (fmap f . step) begin
    {-# INLINE fmap #-}

instance Applicative m => Applicative (ScanM m a) where
    pure b    = ScanM (\_ -> StateT $ \() -> pure (b, ())) (pure ())
    {-# INLINE pure #-}

    (ScanM stepL beginL) <*> (ScanM stepR beginR) =
        let step a (Pair xL xR) =
              (\(bL, xL') (bR, xR') -> (bL bR, (Pair xL' xR')))
              <$> runStateT (stepL a) xL
              <*> runStateT (stepR a) xR
            begin = Pair <$> beginL <*> beginR
        in  ScanM (StateT . step) begin
    {-# INLINE (<*>) #-}

instance Functor m => Profunctor (ScanM m) where
    rmap = fmap
    lmap f (ScanM step begin) = ScanM (step . f) begin

instance Monad m => Category (ScanM m) where
    id = ScanM pure (pure ())
    {-# INLINE id #-}
    (ScanM s2 b2) . (ScanM s1 b1) = ScanM (StateT . step) (Pair <$> b1 <*> b2)
        where step a (Pair xL xR) = do
                (b, xL') <- runStateT (s1 a) xL
                (c, xR') <- runStateT (s2 b) xR
                pure (c, Pair xL' xR')
    {-# INLINE (.) #-}

instance Monad m => Arrow (ScanM m) where
    arr f = ScanM (lift . pure . f) (pure ())
    {-# INLINE arr #-}
    first  (ScanM step begin) = ScanM
      (\(a,b) -> StateT $ \x -> first (,b) <$> runStateT (step a) x)
      begin
    {-# INLINE first #-}
    second (ScanM step begin) = ScanM
      (\(b,a) -> StateT $ \x  -> first (b,) <$> runStateT (step a) x)
      begin
    {-# INLINE second #-}

instance (Monad m, Semigroup b) => Semigroup (ScanM m a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance (Monad m, Monoid b) => Monoid (ScanM m a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = liftA2 mappend
    {-# INLINE mappend #-}

instance (Monad m, Num b) => Num (ScanM m a b) where
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

instance (Monad m, Fractional b) => Fractional (ScanM m a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance (Monad m, Floating b) => Floating (ScanM m a b) where
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

-- | Apply a strict left 'Scan' to a 'Traversable' container
scan :: Traversable t => Scan a b -> t a -> t b
-- To make it possible to consume the generated structure lazily, we must
-- 'traverse' with lazy 'StateT'.
scan (Scan step begin) as = fst $ Lazy.runState (traverse (asLazy . step) as) begin
{-# INLINE scan #-}

-- | Like 'scan' but start scanning from the right
scanr :: Traversable t => Scan a b -> t a -> t b
scanr (Scan step begin) as =
  fst (runReverseState (traverse (ReverseState #. runState . step) as) begin)
{-# INLINE scanr #-}

-- | Like 'scan' but monadic
scanM :: (Traversable t, Monad m) => ScanM m a b -> t a -> m (t b)
-- To make it possible to consume the generated structure lazily, we must
-- 'traverse' with lazy 'StateT'.
scanM (ScanM step begin) as = fmap fst $ Lazy.runStateT (traverse (asLazy . step) as) =<< begin
{-# INLINE scanM #-}

{-| Convert a `Fold` into a prescan

    \"Prescan\" means that the last element of the scan is not included
-}
prescan :: Fold a b -> Scan a b
prescan (Fold step begin done) = Scan (state . step') begin
  where
    step' a x = (b, x')
      where
        x' = step x a
        b  = done x
{-# INLINE prescan #-}

{-| Convert a `Fold` into a postscan

    \"Postscan\" means that the first element of the scan is not included
-}
postscan :: Fold a b -> Scan a b
postscan (Fold step begin done) = Scan (state . step') begin
  where
    step' a x = (b, x')
      where
        x' = step x a
        b  = done x'
{-# INLINE postscan #-}

arrM :: Monad m => (b -> m c) -> ScanM m b c
arrM f = ScanM (lift . f) (pure ())
{-# INLINE arrM #-}

{- $utilities
-}

-- | Upgrade a scan to accept the 'Scan' type
purely :: (forall x . (a -> State x b) -> x -> r) -> Scan a b -> r
purely f (Scan step begin) = f step begin
{-# INLINABLE purely #-}

-- | Upgrade a more traditional scan to accept the `Scan` type
purely_ :: (forall x . (x -> a -> (x, b)) -> x -> r) -> Scan a b -> r
purely_ f (Scan step begin) = f (\s a -> swap $ runState (step a) s) begin
{-# INLINABLE purely_ #-}

-- | Upgrade a monadic scan to accept the 'ScanM' type
impurely
    :: (forall x . (a -> StateT x m b) -> m x -> r)
    -> ScanM m a b
    -> r
impurely f (ScanM step begin) = f step begin
{-# INLINABLE impurely #-}

-- | Upgrade a more traditional monadic scan to accept the `ScanM` type
impurely_
    :: Monad m
    => (forall x . (x -> a -> m (x, b)) -> m x -> r)
    -> ScanM m a b
    -> r
impurely_ f (ScanM step begin) = f (\s a -> swap <$> runStateT (step a) s) begin

{-| Generalize a `Scan` to a `ScanM`

> generalize (pure r) = pure r
>
> generalize (f <*> x) = generalize f <*> generalize x
-}
generalize :: Monad m => Scan a b -> ScanM m a b
generalize (Scan step begin) = hoists
  (\(Identity c) -> pure c)
  (ScanM step (Identity begin))
{-# INLINABLE generalize #-}

{-| Simplify a pure `ScanM` to a `Scan`

> simplify (pure r) = pure r
>
> simplify (f <*> x) = simplify f <*> simplify x
-}
simplify :: ScanM Identity a b -> Scan a b
simplify (ScanM step (Identity begin)) = Scan step begin
{-# INLINABLE simplify #-}

{- | Shift a 'ScanM' from one monad to another with a morphism such as 'lift' or 'liftIO';
     the effect is the same as 'Control.Monad.Morph.hoist'.
-}
hoists :: (forall x . m x -> n x) -> ScanM m a b -> ScanM n a b
hoists phi (ScanM step begin ) = ScanM
  (\a -> StateT $ phi . runStateT (step a))
  (phi begin)
{-# INLINABLE hoists #-}

{-| @(premap f scaner)@ returns a new 'Scan' where f is applied at each step

> scan (premap f scaner) list = scan scaner (map f list)

> premap id = id
>
> premap (f . g) = premap g . premap f

> premap k (pure r) = pure r
>
> premap k (f <*> x) = premap k f <*> premap k x
-}
premap :: (a -> b) -> Scan b r -> Scan a r
premap f (Scan step begin) = Scan (step . f) begin
{-# INLINABLE premap #-}

{-| @(premapM f scaner)@ returns a new 'ScanM' where f is applied to each input
    element

> premapM return = id
>
> premapM (f <=< g) = premap g . premap f

> premapM k (pure r) = pure r
>
> premapM k (f <*> x) = premapM k f <*> premapM k x
-}
premapM :: Monad m => (a -> m b) -> ScanM m b r -> ScanM m a r
premapM f (ScanM step begin) = ScanM (step <=< lift . f) begin
{-# INLINABLE premapM #-}


-- Internal helpers (not exported)
newtype ReverseState s a = ReverseState
  { runReverseState :: s -> (a, s)
  }

instance Functor (ReverseState s) where
  fmap f (ReverseState m) =
    ReverseState $ \s ->
      let (v, s') = m s
      in (f v, s')
  {-# INLINE fmap #-}

instance Applicative (ReverseState s) where
  pure = ReverseState #. (,)
  {-# INLINE pure #-}

  mf <*> mx =
    ReverseState $ \s ->
      let (f, s2) = runReverseState mf s1
          (x, s1) = runReverseState mx s
      in (f x, s2)
  {-# INLINE (<*>) #-}

#if MIN_VERSION_base(4, 10, 0)
  -- 'liftA2' was moved to the 'Applicative' class in base 4.10.0.0
  liftA2 f mx my =
    ReverseState $ \s ->
      let (x, s2) = runReverseState mx s1
          (y, s1) = runReverseState my s
      in (f x y, s2)
  {-# INLINE liftA2 #-}
#endif


#if MIN_VERSION_base(4, 7, 0)
-- | This is same as normal function composition, except slightly more efficient. The same trick is used in base <http://hackage.haskell.org/package/base-4.11.1.0/docs/src/Data.Functor.Utils.html#%23.> and lens <http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Internal-Coerce.html#v:-35-..>
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
#else
(#.) :: (b -> c) -> (a -> b) -> (a -> c)
(#.) = (.)
#endif

infixr 9 #.
{-# INLINE (#.) #-}
