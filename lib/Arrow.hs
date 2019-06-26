{-# LANGUAGE Arrows #-}

module Arrow where

import           Control.Arrow
import           Control.Monad.Identity
import           Foundation

f1, f2 :: Int -> Int
f1 = (*) 2
f2 = (+) 3

{- HLINT ignore f -}
f :: Int -> (Int, Int)
f = \x ->
    let y = 2 * x
        z1 = y + 3
        z2 = y - 5
    in (z1, z2)

{- HLINT ignore fM -}
fM :: Int -> Identity (Int, Int)
fM = \x -> do
    y <- return $ 2 * x
    z1 <- return $ y + 3
    z2 <- return $ y - 5
    return (z1, z2)

fA :: Int -> (Int, Int)
fA = proc x -> do
    y <- (2 *) -< x
    z1 <- (+ 3) -< y
    z2 <- (5 -) -< y
    returnA -< (z1, z2)
