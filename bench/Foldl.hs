{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Foldl hiding (map)
import Criterion.Main
import qualified Data.List
import Prelude hiding (length, sum)
import qualified Prelude
import qualified Data.Foldable as Foldable

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
  ]


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
