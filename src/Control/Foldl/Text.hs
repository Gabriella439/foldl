-- | Folds for byte streams

module Control.Foldl.Text (
    -- * Folds
      head
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

    -- * Re-exports
    -- $reexports
    , module Control.Foldl
    , module Data.Text
    ) where

import Control.Foldl (Fold)
import Control.Foldl.Internal (Maybe'(..), lazy, strict, Either'(..), hush)
import qualified Control.Foldl as L
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (
    head, last, null, length, any, all, maximum, minimum, elem, notElem )

{-| Get the first character of a text stream or return 'Nothing' if the stream
    is empty
-}
head :: Fold Text (Maybe Char)
head = L.Fold step Nothing' lazy
  where
    step mc txt =
        if T.null txt
        then mc
        else case mc of
            Just' _  -> mc
            Nothing' -> Just' (T.head txt)
{-# INLINABLE head #-}

{-| Get the last character of a text stream or return 'Nothing' if the text
    stream is empty
-}
last :: Fold Text (Maybe Char)
last = L.Fold step Nothing' lazy
  where
    step mc txt =
        if T.null txt
        then mc
        else Just' (T.last txt)
        -- TODO: Use `unsafeLast` when Debian Stable Haskell Platform has it
{-# INLINABLE last #-}

-- | Returns 'True' if the text stream is empty, 'False' otherwise
null :: Fold Text Bool
null = L.Fold step True id
  where
    step isNull txt = isNull && T.null txt 
{-# INLINABLE null #-}

-- | Return the length of the text stream in characters
length :: (Num n) => Fold Text n
length = L.Fold (\n txt -> n + fromIntegral (T.length txt)) 0 id
{-# INLINABLE length #-}

{-| @(all predicate)@ returns 'True' if all characters satisfy the predicate,
    'False' otherwise
-}
all :: (Char -> Bool) -> Fold Text Bool
all predicate = L.Fold (\b txt -> b && T.all predicate txt) True id
{-# INLINABLE all #-}

{-| @(any predicate)@ returns 'True' if any character satisfies the predicate,
    'False' otherwise
-}
any :: (Char -> Bool) -> Fold Text Bool
any predicate = L.Fold (\b txt -> b || T.any predicate txt) False id
{-# INLINABLE any #-}

-- | Computes the maximum character
maximum :: Fold Text (Maybe Char)
maximum = L.Fold step Nothing' lazy
  where
    step mc txt =
        if T.null txt
        then mc
        else Just' (case mc of
            Nothing' -> T.maximum txt
            Just' c -> max c (T.maximum txt) )
{-# INLINABLE maximum #-}

-- | Computes the minimum character
minimum :: Fold Text (Maybe Char)
minimum = L.Fold step Nothing' lazy
  where
    step mc txt =
        if T.null txt
        then mc
        else Just' (case mc of
            Nothing' -> T.minimum txt
            Just' c -> min c (T.minimum txt) )
{-# INLINABLE minimum #-}

{-| @(elem c)@ returns 'True' if the text stream has a character equal to @c@,
    'False' otherwise
-}
elem :: Char -> Fold Text Bool
elem c = any (c ==)
{-# INLINABLE elem #-}

{-| @(notElem c)@ returns 'False' if the text stream has a character equal to
    @c@, 'True' otherwise
-}
notElem :: Char -> Fold Text Bool
notElem c = all (c /=)
{-# INLINABLE notElem #-}

{-| @(find predicate)@ returns the first character that satisfies the predicate
    or 'Nothing' if no character satisfies the predicate
-}
find :: (Char -> Bool) -> Fold Text (Maybe Char)
find predicate = L.Fold step Nothing' lazy
  where
    step mc txt = case mc of
        Nothing' -> strict (T.find predicate txt)
        Just' _  -> mc
{-# INLINABLE find #-}

{-| @(index n)@ returns the @n@th character of the text stream, or 'Nothing' if
    the stream has an insufficient number of characters
-}
index :: (Integral n) => n -> Fold Text (Maybe Char)
index i = L.Fold step (Left' (fromIntegral i)) hush
  where
    step x txt = case x of
        Left' remainder ->
            let len = T.length txt
            in  if remainder < len
                then Right' (T.index txt remainder)
                else Left'  (remainder - len)
        _               -> x
{-# INLINABLE index #-}

{-| @(elemIndex c)@ returns the index of the first character that equals @c@,
    or 'Nothing' if no character matches
-}
elemIndex :: (Num n) => Char -> Fold Text (Maybe n)
elemIndex c = findIndex (c ==)
{-# INLINABLE elemIndex #-}

{-| @(findIndex predicate)@ returns the index of the first character that
    satisfies the predicate, or 'Nothing' if no character satisfies the
    predicate
-}
findIndex :: (Num n) => (Char -> Bool) -> Fold Text (Maybe n)
findIndex predicate = L.Fold step (Left' 0) hush
  where
    step x txt = case x of
        Left' m -> case T.findIndex predicate txt of
            Nothing -> Left'  (m + fromIntegral (T.length txt))
            Just n  -> Right' (m + fromIntegral n)
        _       -> x
{-# INLINABLE findIndex #-}

{- $reexports
    "Control.Foldl" re-exports the 'Fold' type

    @Data.Text@ re-exports the 'Text' type
-}
