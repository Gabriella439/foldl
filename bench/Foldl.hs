{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Foldl hiding (map)
import qualified Control.Foldl.NonEmpty as Foldl1
import Criterion.Main
import qualified Data.List
import Prelude hiding (length, sum)
import qualified Prelude
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant (Contravariant(..))
import Data.Profunctor (Profunctor(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = defaultMain
  [ env (return [1..10000 :: Int]) $ \ns ->
      bgroup "[1..10000 :: Int]"
        [ bgroup "sum" $ map ($ ns)
            [ bench "fold sum" .
                whnf (fold sum)
            , bench "foldM (generalize sum)" .
                whnfIO . foldM (generalize sum)
            , bench "Prelude.sum" .
                whnf Prelude.sum
            , bench "Data.List.foldl' (+) 0" .
                whnf (Data.List.foldl' (+) 0)
            ]
        , bgroup "filtered" $ map ($ ns)
            [ bench "fold (handles (filtered even) list)" .
                nf (fold (handles (filtered even) list))
            , bench "foldM (handlesM (filtered even) (generalize list))" .
                nfIO . foldM (handlesM (filtered even) (generalize list))
            , bench "filter even" .
                nf (filter even)
            ]
        , bgroup "length" $ map ($ ns)
            [ bench "fold length" .
                whnf (fold length)
            , bench "foldM (generalize length)" .
                whnfIO . foldM (generalize length)
            , bench "Prelude.length" .
                whnf Prelude.length
            ]
        , bgroup "sumAndLength" $ map ($ ns)
            [ bench "naive sumAndLength" .
                nf sumAndLength
            , bench "foldl' sumAndLength" .
                nf sumAndLength'
            , bench "strict pair sumAndLength" .
                nf sumAndLength_Pair
            , bench "foldl sumAndLength" .
                nf sumAndLength_foldl
            ]
        ]
  , env (return $ 1 :| [2..10000 :: Int]) $ \ns ->
      bgroup "1 :| [2..10000 :: Int]"
        [ bgroup "handles" $ map ($ ns)
            [ bench "fold (handles (to succ) list)" .
                nf (fold (handles (to succ) list))
            , bench "foldM (handlesM (to succ) (generalize list))" .
                nfIO . foldM (handlesM (to succ) (generalize list))
            , bench "NonEmpty.map succ" .
                nf (NonEmpty.map succ)
            , bench "Foldl1.fold1 (Foldl1.handles (to succ) (Foldl1.fromFold list))" .
                nf (Foldl1.fold1 (Foldl1.handles (to succ) (Foldl1.fromFold list)))
            ]
        ]
  ]


-- local definition to avoid importing Control.Lens.Getter.to
to :: (Profunctor p, Contravariant f) => (s -> a) -> p a (f a) -> p s (f s)
to k = dimap k (contramap k)
{-# INLINE to #-}

sumAndLength :: Num a => [a] -> (a, Int)
sumAndLength xs = (Prelude.sum xs, Prelude.length xs)

sumAndLength' :: Num a => [a] -> (a, Int)
sumAndLength' xs = Foldable.foldl' step (0, 0) xs
  where
    step (x, y) n = (x + n, y + 1)

data Pair a b = Pair !a !b

sumAndLength_Pair :: Num a => [a] -> (a, Int)
sumAndLength_Pair xs = done (Foldable.foldl' step (Pair 0 0) xs)
  where
    step (Pair x y) n = Pair (x + n) (y + 1)

    done (Pair x y) = (x, y)

sumAndLength_foldl :: Num a => [a] -> (a, Int)
sumAndLength_foldl = fold ((,) <$> sum <*> length)
