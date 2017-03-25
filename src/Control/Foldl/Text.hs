-- | Folds for text streams

module Control.Foldl.Text (
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
    , module Data.Text
    ) where

import Control.Foldl (Fold, FoldM)
import Control.Foldl.Internal (Maybe'(..), strict, Either'(..), hush)
import Data.Text (Text)
import Prelude hiding (
    head, last, null, length, any, all, maximum, minimum, elem, notElem )

import qualified Control.Foldl
import qualified Control.Foldl.Internal
import qualified Data.Text
import qualified Data.Text.Lazy

-- | Apply a strict left 'Fold' to lazy text
fold :: Fold Text a -> Data.Text.Lazy.Text -> a
fold (Control.Foldl.Fold step begin done) as =
    done (Data.Text.Lazy.foldlChunks step begin as)
{-# INLINABLE fold #-}

-- | Apply a strict monadic left 'FoldM' to lazy text
foldM :: Monad m => FoldM m Text a -> Data.Text.Lazy.Text -> m a
foldM (Control.Foldl.FoldM step begin done) as = do
    x <- Data.Text.Lazy.foldlChunks step' begin as
    done x
  where
    step' mx bs = do
      x <- mx
      x `seq` step x bs
{-# INLINABLE foldM #-}

{-| Get the first character of a text stream or return 'Nothing' if the stream
    is empty
-}
head :: Fold Text (Maybe Char)
head = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mc txt =
        if Data.Text.null txt
        then mc
        else case mc of
            Just' _  -> mc
            Nothing' -> Just' (Data.Text.head txt)
{-# INLINABLE head #-}

{-| Get the last character of a text stream or return 'Nothing' if the text
    stream is empty
-}
last :: Fold Text (Maybe Char)
last = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mc txt =
        if Data.Text.null txt
        then mc
        else Just' (Data.Text.last txt)
        -- TODO: Use `unsafeLast` when Debian Stable Haskell Platform has it
{-# INLINABLE last #-}

-- | Returns 'True' if the text stream is empty, 'False' otherwise
null :: Fold Text Bool
null = Control.Foldl.Fold step True id
  where
    step isNull txt = isNull && Data.Text.null txt 
{-# INLINABLE null #-}

-- | Return the length of the text stream in characters
length :: Num n => Fold Text n
length =
    Control.Foldl.Fold (\n txt -> n + fromIntegral (Data.Text.length txt)) 0 id
{-# INLINABLE length #-}

{-| @(all predicate)@ returns 'True' if all characters satisfy the predicate,
    'False' otherwise
-}
all :: (Char -> Bool) -> Fold Text Bool
all predicate =
    Control.Foldl.Fold (\b txt -> b && Data.Text.all predicate txt) True id
{-# INLINABLE all #-}

{-| @(any predicate)@ returns 'True' if any character satisfies the predicate,
    'False' otherwise
-}
any :: (Char -> Bool) -> Fold Text Bool
any predicate =
    Control.Foldl.Fold (\b txt -> b || Data.Text.any predicate txt) False id
{-# INLINABLE any #-}

-- | Computes the maximum character
maximum :: Fold Text (Maybe Char)
maximum = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mc txt =
        if Data.Text.null txt
        then mc
        else Just' (case mc of
            Nothing' -> Data.Text.maximum txt
            Just' c -> max c (Data.Text.maximum txt) )
{-# INLINABLE maximum #-}

-- | Computes the minimum character
minimum :: Fold Text (Maybe Char)
minimum = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mc txt =
        if Data.Text.null txt
        then mc
        else Just' (case mc of
            Nothing' -> Data.Text.minimum txt
            Just' c -> min c (Data.Text.minimum txt) )
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
find predicate = Control.Foldl.Fold step Nothing' Control.Foldl.Internal.lazy
  where
    step mc txt = case mc of
        Nothing' -> strict (Data.Text.find predicate txt)
        Just' _  -> mc
{-# INLINABLE find #-}

{-| @(index n)@ returns the @n@th character of the text stream, or 'Nothing' if
    the stream has an insufficient number of characters
-}
index :: Integral n => n -> Fold Text (Maybe Char)
index i = Control.Foldl.Fold step (Left' (fromIntegral i)) hush
  where
    step x txt = case x of
        Left' remainder ->
            let len = Data.Text.length txt
            in  if remainder < len
                then Right' (Data.Text.index txt remainder)
                else Left'  (remainder - len)
        _               -> x
{-# INLINABLE index #-}

{-| @(elemIndex c)@ returns the index of the first character that equals @c@,
    or 'Nothing' if no character matches
-}
elemIndex :: Num n => Char -> Fold Text (Maybe n)
elemIndex c = findIndex (c ==)
{-# INLINABLE elemIndex #-}

{-| @(findIndex predicate)@ returns the index of the first character that
    satisfies the predicate, or 'Nothing' if no character satisfies the
    predicate
-}
findIndex :: Num n => (Char -> Bool) -> Fold Text (Maybe n)
findIndex predicate = Control.Foldl.Fold step (Left' 0) hush
  where
    step x txt = case x of
        Left' m -> case Data.Text.findIndex predicate txt of
            Nothing -> Left'  (m + fromIntegral (Data.Text.length txt))
            Just n  -> Right' (m + fromIntegral n)
        _       -> x
{-# INLINABLE findIndex #-}

-- | @(count c)@ returns the number of times @c@ appears
count :: Num n => Char -> Fold Text n
count c = Control.Foldl.Fold step 0 id
  where
    step n txt = n + fromIntegral (Data.Text.count (Data.Text.singleton c) txt)
{-# INLINABLE count #-}

-- | Combine all the strict `Text` chunks to build a lazy `Text`
lazy :: Fold Text Data.Text.Lazy.Text
lazy = fmap Data.Text.Lazy.fromChunks Control.Foldl.list
{-# INLINABLE lazy #-}

{- $reexports
    "Control.Foldl" re-exports the 'Fold' type

    @Data.Text@ re-exports the 'Text' type
-}
