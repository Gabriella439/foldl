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
    , lazy

    -- * Re-exports
    -- $reexports
    , module Control.Foldl
    , module Data.ByteString
    , module Data.Word
    ) where

import Control.Foldl (Fold, FoldM)
import Control.Foldl.Internal (Maybe'(..), strict, Either'(..), hush)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Prelude hiding (
    head, last, null, length, any, all, maximum, minimum, elem, notElem )

import qualified Control.Foldl
import qualified Control.Foldl.Internal
import qualified Data.ByteString
import qualified Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy

-- | Apply a strict left 'Fold' to a lazy bytestring
fold :: Fold ByteString a -> Data.ByteString.Lazy.ByteString -> a
fold (Control.Foldl.Fold step begin done) as =
    done (Data.ByteString.Lazy.Internal.foldlChunks step begin as)
{-# INLINABLE fold #-}

-- | Apply a strict monadic left 'FoldM' to a lazy bytestring
foldM
    :: Monad m => FoldM m ByteString a -> Data.ByteString.Lazy.ByteString -> m a
foldM (Control.Foldl.FoldM step begin done) as = do
    x <- Data.ByteString.Lazy.Internal.foldlChunks step' begin as
    done x
  where
    step' mx bs = do
      x <- mx
      x `seq` step x bs
{-# INLINABLE foldM #-}

{-| Get the first byte of a byte stream or return 'Nothing' if the stream is
    empty
-}
head :: Fold ByteString (Maybe Word8)
head = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mw8 bs =
        if Data.ByteString.null bs
        then mw8
        else case mw8 of
            Just' _  -> mw8
            Nothing' -> Just' (Data.ByteString.Unsafe.unsafeHead bs)
{-# INLINABLE head #-}

{-| Get the last byte of a byte stream or return 'Nothing' if the byte stream is
    empty
-}
last :: Fold ByteString (Maybe Word8)
last = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mw8 bs =
        if Data.ByteString.null bs
        then mw8
        else Just' (Data.ByteString.last bs)
        -- TODO: Use `unsafeLast` when Debian Stable Haskell Platform has it
{-# INLINABLE last #-}

-- | Returns 'True' if the byte stream is empty, 'False' otherwise
null :: Fold ByteString Bool
null = Control.Foldl.Fold step True id
  where
    step isNull bs = isNull && Data.ByteString.null bs
{-# INLINABLE null #-}

-- | Return the length of the byte stream in bytes
length :: Num n => Fold ByteString n
length = Control.Foldl.Fold step 0 id
  where
    step n bs = n + fromIntegral (Data.ByteString.length bs)
{-# INLINABLE length #-}

{-| @(all predicate)@ returns 'True' if all bytes satisfy the predicate, 'False'
    otherwise
-}
all :: (Word8 -> Bool) -> Fold ByteString Bool
all predicate =
    Control.Foldl.Fold (\b bs -> b && Data.ByteString.all predicate bs) True id
{-# INLINABLE all #-}

{-| @(any predicate)@ returns 'True' if any byte satisfies the predicate,
    'False' otherwise
-}
any :: (Word8 -> Bool) -> Fold ByteString Bool
any predicate =
    Control.Foldl.Fold (\b bs -> b || Data.ByteString.any predicate bs) False id
{-# INLINABLE any #-}

-- | Computes the maximum byte
maximum :: Fold ByteString (Maybe Word8)
maximum = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mw8 bs =
        if Data.ByteString.null bs
        then mw8
        else Just' (case mw8 of
            Nothing' -> Data.ByteString.maximum bs
            Just' w8 -> max w8 (Data.ByteString.maximum bs) )
{-# INLINABLE maximum #-}

-- | Computes the minimum byte
minimum :: Fold ByteString (Maybe Word8)
minimum = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mw8 bs =
        if Data.ByteString.null bs
        then mw8
        else Just' (case mw8 of
            Nothing' -> Data.ByteString.minimum bs
            Just' w8 -> min w8 (Data.ByteString.minimum bs) )
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
find predicate = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mw8 bs = case mw8 of
        Nothing' -> strict (Data.ByteString.find predicate bs)
        Just' _  -> mw8
{-# INLINABLE find #-}

{-| @(index n)@ returns the @n@th byte of the byte stream, or 'Nothing' if the
    stream has an insufficient number of bytes
-}
index :: Integral n => n -> Fold ByteString (Maybe Word8)
index i = Control.Foldl.Fold step (Left' (fromIntegral i)) hush
  where
    step x bs = case x of
        Left' remainder ->
            let len = Data.ByteString.length bs
            in  if remainder < len
                then Right' (Data.ByteString.Unsafe.unsafeIndex bs remainder)
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
findIndex predicate = Control.Foldl.Fold step (Left' 0) hush
  where
    step x bs = case x of
        Left' m -> case Data.ByteString.findIndex predicate bs of
            Nothing -> Left'  (m + fromIntegral (Data.ByteString.length bs))
            Just n  -> Right' (m + fromIntegral n)
        _       -> x
{-# INLINABLE findIndex #-}

-- | @count w8@ returns the number of times @w8@ appears
count :: Num n => Word8 -> Fold ByteString n
count w8 = Control.Foldl.Fold step 0 id
  where
    step n bs = n + fromIntegral (Data.ByteString.count w8 bs)
{-# INLINABLE count #-}

-- | Combine all the strict `ByteString` chunks to build a lazy `ByteString`
lazy :: Fold ByteString Data.ByteString.Lazy.ByteString
lazy = fmap Data.ByteString.Lazy.fromChunks Control.Foldl.list
{-# INLINABLE lazy #-}

-- | 

{- $reexports

    "Control.Foldl" re-exports the 'Fold' type

    @Data.ByteString@ re-exports the 'ByteString' type

    @Data.Word@ re-exports the 'Word8' type
-}
