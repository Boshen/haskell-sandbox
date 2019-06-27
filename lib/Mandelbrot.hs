module Mandelbrot where

import           Data.Array.Repa          ((:.) (..), Array, DIM2, DIM3, Z (..))
import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D
import qualified Data.Colour.RGBSpace     as Colour
import           Data.Colour.RGBSpace.HSL (hsl)
import           Data.Complex
import           Data.Word                (Word8)

type Dimension = (Int, Int)

maxIter = 255
startx = -2.5
endx = 3.5
starty = -1
endy = 2

getMandelbrotImage :: Dimension -> D.Image
getMandelbrotImage d = (D.RGB . R.computeS . R.delay . reshape d) (pixels d)

reshape :: Dimension -> [Word8] -> Array R.D DIM3 Word8
reshape (w, h) arr =
    R.reshape
    (Z :. h :. w :. 3)
    (R.fromListUnboxed (Z :. (3::Int) :. w :. h) arr)

pixels :: Dimension -> [Word8]
pixels d@(w, h) = [ (j, i) | j <- [0..h-1], i <- [0..w-1] ] >>= pixel d

pixel :: Dimension -> Dimension -> [Word8]
pixel (w, h) (j, i) = [Colour.channelRed rgb, Colour.channelGreen rgb, Colour.channelBlue rgb]
    where
        x = startx + endx  / fromIntegral w * fromIntegral i
        y = starty + endy / fromIntegral h * fromIntegral j
        p = (x :+ y)
        (z, n) = mandelbrot p
        mu = fromIntegral n + 1.0 - (log (log (magnitude z))) / (log 2)
        hue = (min 360) . (max 0) $ (0.95 + 20 * mu)
        rgb = if n >= maxIter
            then Colour.RGB 255 255 255
            else (truncate . (*255)) <$> hsl hue 1 0.5

mandelbrot :: Complex Double -> (Complex Double, Int)
mandelbrot = iterRec 0 (0 :+ 0)

iterRec :: Int -> Complex Double -> Complex Double -> (Complex Double, Int)
iterRec n z c
    | magnitude z > 2 = (z, n)
    | n > maxIter = (z, n)
    | otherwise = iterRec (n + 1) (z * z + c) c
