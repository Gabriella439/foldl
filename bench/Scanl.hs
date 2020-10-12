-- Copyright (c) 2020 Google LLC

-- | Benchmarks for the 'Control.Scanl' module.
--
-- These benchmarks can also be used to detect space leaks via the "limited
-- stack size" method. For example, to check all of the pure left scan
-- benchmarks via 'stack':
--
-- % stack bench :Scanl \
--   --benchmark-arguments='"[1..10000 :: Int]/sum of scan/" +RTS -K1K'
module Main (main) where

import Control.Category ((.))
import qualified Control.Foldl as Foldl
import Control.Scanl
import Criterion.Main
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity(..))
import Prelude hiding ((.), scanr, sum)

-- A sum function guaranteed not to leak space on strict data types.
sum :: (Foldable t, Num a) => t a -> a
sum = foldl' (+) 0

scanSum :: Scan Int Int
scanSum = postscan Foldl.sum

scanMSum :: Monad m => ScanM m Int Int
scanMSum = generalize scanSum

scanProduct :: Scan Int Int
scanProduct = postscan Foldl.product

scanMProduct :: Monad m => ScanM m Int Int
scanMProduct = generalize scanProduct

main :: IO ()
main = defaultMain
  [ env (return [1..10000 :: Int]) $ \ns ->
      bgroup "[1..10000 :: Int]"
        [ bgroup "sum of scan" $ map ($ ns)
            [ bench "1" .
                whnf (sum . scan (1 :: Scan Int Int))
            , bench "scanSum" .
                whnf (sum . scan scanSum)
            , bench "scanProduct" .
                whnf (sum . scan scanProduct)
            , bench "fmap (+1) scanSum" .
                whnf (sum . scan (fmap (+1) scanSum))
            , bench "scanProduct / scanSum" .
                whnf (sum . scan (scanProduct + scanSum))
            , bench "scanProduct . scanSum" .
                whnf (sum . scan (scanProduct . scanSum))
            ]
        , bgroup "sum of scanM @Identity" $ map ($ ns)
            [ bench "1" .
                whnf (runIdentity . fmap sum . scanM (1 :: ScanM Identity Int Int))
            , bench "scanMSum" .
                whnf (runIdentity . fmap sum . scanM scanMSum)
            , bench "scanMProduct" .
                whnf (runIdentity . fmap sum . scanM scanMProduct)
            , bench "fmap (+1) scanMSum" .
                whnf (runIdentity . fmap sum . scanM (fmap (+1) scanMSum))
            , bench "scanMProduct / scanMSum" .
                whnf (runIdentity . fmap sum . scanM (scanMProduct + scanMSum))
            , bench "scanMProduct . scanMSum)" .
                whnf (runIdentity . fmap sum . scanM (scanMProduct . scanMSum))
            ]
        -- These right scans cannot be processed in constant space, so the
        -- "limited stack size" space leak test will always fail.
        , bgroup "sum of scanr" $ map ($ ns)
            [ bench "1" .
                whnf (sum . scanr (1 :: Scan Int Int))
            , bench "scanSum" .
                whnf (sum . scanr scanSum)
            , bench "scanProduct" .
                whnf (sum . scanr scanProduct)
            , bench "fmap (+1) scanSum" .
                whnf (sum . scanr (fmap (+1) scanSum))
            , bench "scanProduct / scanSum" .
                whnf (sum . scanr (scanProduct + scanSum))
            , bench "scanProduct . scanSum" .
                whnf (sum . scanr (scanProduct . scanSum))
            ]
        ]
  ]
