module Main (main) where

import Control.Foldl
import Criterion.Main
import qualified Data.List
import Prelude hiding (length, sum)
import qualified Prelude

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
        ]
  ]
