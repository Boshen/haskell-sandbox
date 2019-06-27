{-# LANGUAGE DeriveFunctor #-}

module ImageProcessing where

import           Control.Comonad
import           Data.List            (concatMap, (!!))
import qualified Data.List            as L
import qualified Data.Vector          as V
import           Foundation
import           Foundation.Numerical (roundNearest)

data Matrix a = Matrix { width :: Int, height :: Int, dat :: [[a]] } deriving (Show, Eq)
type Image  = Matrix Int
type Kernel = Matrix Double

data Pixel a = Pixel !a !a !a deriving (Show, Eq, Functor)

data BoxedImage a = BoxedImage
    { biWidth  :: !Int
    , biHeight :: !Int
    , biData   :: !(V.Vector a)
    } deriving (Show, Functor)

data FocusedImage a = FocusedImage
    { piBoxedImage :: !(BoxedImage a)
    , piX          :: !Int
    , piY          :: !Int
    } deriving (Show, Functor)

instance Additive a => Semigroup (Pixel a) where
    (Pixel r1 g1 b1) <> (Pixel r2 g2 b2) = Pixel (r1 + r2) (g1 + g2) (b1 + b2)

instance Additive a => Monoid (Pixel a) where
    mempty = Pixel azero azero azero
    (Pixel r1 g1 b1) `mappend` (Pixel r2 g2 b2) = Pixel (r1 + r2) (g1 + g2) (b1 + b2)

instance Comonad FocusedImage where
    extract (FocusedImage (BoxedImage w _ bi) x y) = bi V.! (y * w + x)
    extend f (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage
        (BoxedImage w h $ V.generate (w * h) $ \i ->
            let (y', x') = i `divMod` w
            in f (FocusedImage bi x' y'))
        x y

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

neighbour :: Int -> Int -> FocusedImage a -> FocusedImage a
neighbour dx dy (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage bi x' y'
  where
    x' = clamp 0 (w - 1) (x + dx)
    y' = clamp 0 (h - 1) (y + dy)

toFocusedImage :: Image -> FocusedImage (Pixel Int)
toFocusedImage (Matrix _ h m) = FocusedImage (BoxedImage h h (V.fromList . pixels . mconcat $ m)) 0 0
    where
        pixels []         = []
        pixels (r:g:b:xs) = Pixel r g b : pixels xs
        pixels _          = error "error"

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks r xs = take (CountOf r) xs : chunks r (drop (CountOf r) xs)

toImage :: Int -> Int -> FocusedImage (Pixel Int) -> Image
toImage w h (FocusedImage (BoxedImage _ _ bi) _ _) = (Matrix w h . chunks w) (concatMap (\(Pixel r g b) -> [r, g, b]) (V.toList bi))

reduceImage :: Kernel -> FocusedImage (Pixel Int) -> Pixel Int
reduceImage (Matrix w h k) fi = roundNearest . clamp 0 255 <$> L.foldl' mappend mempty ls
    where
        ls :: [Pixel Double]
        ls = do
            x <- [0..(w - 1)]
            y <- [0..(h - 1)]
            let weight = (k !! y) !! x
            return $ (fmap ((*weight) . fromIntegral) . extract) (neighbour (x - w `div` 2) (y - h `div` 2) fi)

processImage :: Image -> Kernel -> Image
processImage image@(Matrix w h _) k = toImage w h $ extend (reduceImage k) (toFocusedImage image)
