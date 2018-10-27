-- Adaptation of 
-- Module      : Graphics.Image.Processing.Filter
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
-- to clash constructs
-- http://hackage.haskell.org/package/hip-1.5.3.0/docs/src/Graphics-Image-Interface.html#Border
-- http://hackage.haskell.org/package/hip-1.5.3.0/docs/src/Graphics-Image-Processing-Filter.html
-- http://hackage.haskell.org/package/hip-1.5.3.0/docs/src/Graphics-Image-Interface-Vector-Storable.html#VS
-- https://hackage.haskell.org/package/hip-1.5.3.0/src/src/Graphics/Image/Interface/Vector/Generic.hs
module GAUSSIAN where
import Clash.Prelude

data VGImage v p =
  VGImage {-# UNPACK #-}!Int
          {-# UNPACK #-}!Int

makeImageVG :: VG.Vector v p => (Int, Int) -> ((Int, Int) -> p) -> VGImage v p
makeImageVG sz f =
  let (m, n) = checkDimsVG "makeImageVGM" sz in
    VGImage m n $ VG.generate (m * n) (f . toIx n)
{-# INLINE makeImageVG #-}

foldlVG :: VG.Vector v p =>
           (a -> p -> a) -> a -> VGImage v p -> a
foldlVG f px0 (VGImage _ _ v) = VG.foldl' f px0 v
{-# INLINE foldlVG #-}

Ifold f px0 (VSImage img) = foldlVG f px0 img






-- | Filter that can be applied to an image using `applyFilter`.
--
-- @since 1.5.3
data Filter arr cs e = Filter
  { applyFilter :: Image arr cs e -> Image arr cs e -- ^ Apply a filter to an image
  }

-- | Used to specify direction for some filters.
data Direction
  = Vertical
  | Horizontal

-- | Approach to be used near the borders during various transformations.
-- Whenever a function needs information not only about a pixel of interest, but
-- also about it's neighbours, it will go out of bounds around the image edges,
-- hence is this set of approaches that can be used in such situtation.
data Border px =
  Fill px    -- ^ Fill in a constant pixel.
              --
              -- @
              --            outside |  Image  | outside
              -- ('Fill' 0) : 0 0 0 0 | 1 2 3 4 | 0 0 0 0
              -- @
              --
  | Wrap      -- ^ Wrap around from the opposite border of the image.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Wrap' :     1 2 3 4 | 1 2 3 4 | 1 2 3 4
              -- @
              --
  | Edge      -- ^ Replicate the pixel at the edge.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Edge' :     1 1 1 1 | 1 2 3 4 | 4 4 4 4
              -- @
              --
  | Reflect   -- ^ Mirror like reflection.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Reflect' :  4 3 2 1 | 1 2 3 4 | 4 3 2 1
              -- @
              --
  | Continue  -- ^ Also mirror like reflection, but without repeating the edge pixel.
              --
              -- @
              --            outside |  Image  | outside
              -- 'Continue' : 1 4 3 2 | 1 2 3 4 | 3 2 1 4
              -- @
              --
  deriving Show



-- | Create a Gaussian Filter.
--
-- @since 1.5.3
gaussianLowPass :: (Vec 256 arr cs e, Vec 256 arr a e, Floating e, Fractional e) =>
                   Int -- ^ Radius
                -> e -- ^ Sigma
                -> Border (Pixel cs e) -- ^ Border resolution technique.
                -> Filter arr cs e
gaussianLowPass r sigma border =
  Filter (correlate border gV' . correlate border gV)
  where
    gV = compute $ (gauss / scalar weight)
    gV' = compute $ transpose gV
    gauss = makeImage (1, n) getPx
    weight = Ifold (+) 0 gauss
    n = 2 * r + 1
    sigma2sq = 2 * sigma ^ (2 :: Int)
    getPx (_, j) = promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)





-- | Create a Gaussian Blur filter. Radius will be derived from standard
-- deviation: @ceiling (2*sigma)@ and `Edge` border resolution will be
-- utilized. If custom radius and/or border resolution is desired,
-- `gaussianLowPass` can be used instead.
--
-- @since 1.5.3
gaussianBlur :: (Vec 256 arr cs e, Vec 256 arr a e, Floating e, RealFrac e) => e -> Filter arr cs e
gaussianBlur sigma = gaussianLowPass (ceiling (2*sigma)) sigma Edge

