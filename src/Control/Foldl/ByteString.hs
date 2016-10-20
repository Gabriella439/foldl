-- | Folds for byte streams

module Control.Foldl.ByteString (
    -- * Folding
      fold
    , foldM

    -- * Folds
    , head
    , last
    , null
    , length
    , any
    , all
    , maximum
    , minimum
    , elem
    , notElem
    , find
    , index
    , elemIndex
    , findIndex
    , count

    -- * Re-exports
    -- $reexports
    , module Control.Foldl
    , module Data.ByteString
    , module Data.Word
    ) where

import Control.Foldl (Fold, FoldM)
import Control.Foldl.Internal (Maybe'(..), lazy, strict, Either'(..), hush)
import qualified Control.Foldl as L
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as Lazy
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Prelude hiding (
    head, last, null, length, any, all, maximum, minimum, elem, notElem )

-- | Apply a strict left 'Fold' to a lazy bytestring
fold :: Fold ByteString a -> Lazy.ByteString -> a
fold (L.Fold step begin done) as = done (Lazy.foldlChunks step begin as)
{-# INLINABLE fold #-}

-- | Apply a strict monadic left 'FoldM' to a lazy bytestring
foldM :: Monad m => FoldM m ByteString a -> Lazy.ByteString -> m a
foldM (L.FoldM step begin done) as = do
    x <- Lazy.foldlChunks step' begin as
    done x
  where
    step' mx bs = do
      x <- mx
      step x bs
{-# INLINABLE foldM #-}

{-| Get the first byte of a byte stream or return 'Nothing' if the stream is
    empty
-}
head :: Fold ByteString (Maybe Word8)
head = L.Fold step Nothing' lazy
  where
    step mw8 bs =
        if B.null bs
        then mw8
        else case mw8 of
            Just' _  -> mw8
            Nothing' -> Just' (BU.unsafeHead bs)
{-# INLINABLE head #-}

{-| Get the last byte of a byte stream or return 'Nothing' if the byte stream is
    empty
-}
last :: Fold ByteString (Maybe Word8)
last = L.Fold step Nothing' lazy
  where
    step mw8 bs =
        if B.null bs
        then mw8
        else Just' (B.last bs)
        -- TODO: Use `unsafeLast` when Debian Stable Haskell Platform has it
{-# INLINABLE last #-}

-- | Returns 'True' if the byte stream is empty, 'False' otherwise
null :: Fold ByteString Bool
null = L.Fold step True id
  where
    step isNull bs = isNull && B.null bs
{-# INLINABLE null #-}

-- | Return the length of the byte stream in bytes
length :: Num n => Fold ByteString n
length = L.Fold (\n bs -> n + fromIntegral (B.length bs)) 0 id
{-# INLINABLE length #-}

{-| @(all predicate)@ returns 'True' if all bytes satisfy the predicate, 'False'
    otherwise
-}
all :: (Word8 -> Bool) -> Fold ByteString Bool
all predicate = L.Fold (\b bs -> b && B.all predicate bs) True id
{-# INLINABLE all #-}

{-| @(any predicate)@ returns 'True' if any byte satisfies the predicate,
    'False' otherwise
-}
any :: (Word8 -> Bool) -> Fold ByteString Bool
any predicate = L.Fold (\b bs -> b || B.any predicate bs) False id
{-# INLINABLE any #-}

-- | Computes the maximum byte
maximum :: Fold ByteString (Maybe Word8)
maximum = L.Fold step Nothing' lazy
  where
    step mw8 bs =
        if B.null bs
        then mw8
        else Just' (case mw8 of
            Nothing' -> B.maximum bs
            Just' w8 -> max w8 (B.maximum bs) )
{-# INLINABLE maximum #-}

-- | Computes the minimum byte
minimum :: Fold ByteString (Maybe Word8)
minimum = L.Fold step Nothing' lazy
  where
    step mw8 bs =
        if B.null bs
        then mw8
        else Just' (case mw8 of
            Nothing' -> B.minimum bs
            Just' w8 -> min w8 (B.minimum bs) )
{-# INLINABLE minimum #-}

{-| @(elem w8)@ returns 'True' if the byte stream has a byte equal to @w8@,
    'False' otherwise
-}
elem :: Word8 -> Fold ByteString Bool
elem w8 = any (w8 ==)
{-# INLINABLE elem #-}

{-| @(notElem w8)@ returns 'False' if the byte stream has a byte equal to @w8@,
    'True' otherwise
-}
notElem :: Word8 -> Fold ByteString Bool
notElem w8 = all (w8 /=)
{-# INLINABLE notElem #-}

{-| @(find predicate)@ returns the first byte that satisfies the predicate or
    'Nothing' if no byte satisfies the predicate
-}
find :: (Word8 -> Bool) -> Fold ByteString (Maybe Word8)
find predicate = L.Fold step Nothing' lazy
  where
    step mw8 bs = case mw8 of
        Nothing' -> strict (B.find predicate bs)
        Just' _  -> mw8
{-# INLINABLE find #-}

{-| @(index n)@ returns the @n@th byte of the byte stream, or 'Nothing' if the
    stream has an insufficient number of bytes
-}
index :: Integral n => n -> Fold ByteString (Maybe Word8)
index i = L.Fold step (Left' (fromIntegral i)) hush
  where
    step x bs = case x of
        Left' remainder ->
            let len = B.length bs
            in  if remainder < len
                then Right' (BU.unsafeIndex bs remainder)
                else Left'  (remainder - len)
        _               -> x
{-# INLINABLE index #-}

{-| @(elemIndex w8)@ returns the index of the first byte that equals @w8@, or
    'Nothing' if no byte matches
-}
elemIndex :: Num n => Word8 -> Fold ByteString (Maybe n)
elemIndex w8 = findIndex (w8 ==)
{-# INLINABLE elemIndex #-}

{-| @(findIndex predicate)@ returns the index of the first byte that satisfies
    the predicate, or 'Nothing' if no byte satisfies the predicate
-}
findIndex :: Num n => (Word8 -> Bool) -> Fold ByteString (Maybe n)
findIndex predicate = L.Fold step (Left' 0) hush
  where
    step x bs = case x of
        Left' m -> case B.findIndex predicate bs of
            Nothing -> Left'  (m + fromIntegral (B.length bs))
            Just n  -> Right' (m + fromIntegral n)
        _       -> x
{-# INLINABLE findIndex #-}

-- | @count w8@ returns the number of times @w8@ appears
count :: Num n => Word8 -> Fold ByteString n
count w8 = L.Fold step 0 id
  where
    step n bs = n + fromIntegral (B.count w8 bs)
{-# INLINABLE count #-}

{- $reexports

    "Control.Foldl" re-exports the 'Fold' type

    @Data.ByteString@ re-exports the 'ByteString' type

    @Data.Word@ re-exports the 'Word8' type
-}
