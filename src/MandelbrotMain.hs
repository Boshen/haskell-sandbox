module MandelbrotMain(main) where

import           Data.Array.Repa.IO.DevIL (runIL, writeImage)
import           Data.Maybe
import           System.Environment
import           Text.Read

import           Mandelbrot

main :: IO ()
main = do
    args <- getArgs
    case readDimension args of
        Nothing -> putStrLn "Usage: ./mandelbrot width height"
        Just d -> do
            -- putStrLn $ printMandelbrot d
            runIL $ writeImage "test.jpg" (getMandelbrotImage d)

readDimension :: [String] -> Maybe (Int, Int)
readDimension args
    | length args /= 2 = Nothing
    | otherwise = do
        width <- readMaybe (args !! 0)
        height <- readMaybe (args !! 1)
        return (width, height)
