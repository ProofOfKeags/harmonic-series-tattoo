{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Arrow
import Graphics.Image hiding (zipWith)

newtype Curve = Curve {unCurve :: Double -> Double}
newtype ColorFn = ColorFn {unColorFn :: (Int, Int) -> Pixel X Bit}
instance Semigroup ColorFn where
  (ColorFn f0) <> (ColorFn f1) = ColorFn $ fromBool . uncurry (||) . (isOn *** isOn) . (f0 &&& f1)
instance Monoid ColorFn where
  mempty = ColorFn $ const off
color :: Curve -> ColorFn
color (Curve c) = ColorFn $ fromBool . (< 1) . abs . uncurry (-) . second c . (fromIntegral *** fromIntegral)
colorAll :: [Curve] -> ColorFn
colorAll = foldMap color
tattoo :: Image RPU X Bit
tattoo =
  let width = 4000
      height = 2000
      halfHeight = height / 2
      photoNegate = Graphics.Image.map (fmap (bool2bit . not . bit2bool))
   in photoNegate . makeImage (height, width) . unColorFn . colorAll $
        translateY halfHeight . scaleY (halfHeight * 0.5) . scaleX (width / pi) <$> take 7 harmonics
harmonics :: [Curve]
harmonics = foldr (zipWith ($)) sines [alternated, yScaled, xScaled]
 where
  sines = (repeat . Curve) (negate . sin)
  (yScaled, xScaled) = unzip $ (scaleX &&& scaleY) . (1 /) <$> [1 ..]
  alternated = scaleY <$> cycle [1, -1]
translateY :: Double -> Curve -> Curve
translateY n (Curve f) = Curve $ (+ n) . f
translateX :: Double -> Curve -> Curve
translateX n (Curve f) = Curve $ f . (+ n)
scaleX :: Double -> Curve -> Curve
scaleX n (Curve f) = Curve $ f . (/ n)
scaleY :: Double -> Curve -> Curve
scaleY n (Curve f) = Curve $ (n *) . f
main :: IO ()
main = writeImage "tattoo.png" $ toImageRGB tattoo
