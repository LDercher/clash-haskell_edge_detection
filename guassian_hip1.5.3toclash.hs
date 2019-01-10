--- synopsis: implementation of guassian blur function from Haskell Graphics.GD Haskell module in clash (Haskell->VHDL)
--- author: Luke Dercher
--- citation: Haskell Graphics.HIP version 1.5.3 library 
--- last modified: 12/04/2018

{-# LANGUAGE FlexibleInstances, ViewPatterns, UndecidableInstances, BangPatterns,
      UndecidableSuperClasses, MultiParamTypeClasses, RankNTypes #-}


module GAUSSIAN where
import Clash.Prelude
import           Data.Typeable                     (Typeable, showsTypeRep,
                                                    typeRep)

--https://hackage.haskell.org/package/hip-1.0.1/candidate/docs/src/Graphics-Image-Interface.html#Elevator


                                                    
import Prelude hiding (and, map, zipWith, sum, product)
-- #if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
import Data.Maybe
import Data.Foldable
-- #endif
import GHC.Exts (Constraint)
import Data.Typeable
import GHC.Float
import Data.Word
import Control.Applicative
import qualified Data.Complex  as C



-- | Very efficient loop
loop :: t -> (t -> Bool) -> (t -> t) -> a -> (t -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> acc
      True  -> go (increment step) (f step acc)
{-# INLINE loop #-}

-- | Correlate an image with a kernel. Border resolution technique is required.
correlate :: (Array arr X e, Array arr cs e)
          => Border (Pixel cs e) -> Image arr X e -> Image arr cs e -> Image arr cs e
correlate !border !kernel !img =
  makeImageWindowed
    sz
    (kM2, kN2)
    (m - kM2 * 2, n - kN2 * 2)
    (getStencil (unsafeIndex imgM))
    (getStencil (borderIndex border imgM))
  where
    !imgM = toManifest img
    !sz@(m, n) = dims img
    !kernelM = toManifest kernel
    !(kM, kN) = dims kernel
    !(kM2, kN2) = (kM `div` 2, kN `div` 2)
    getStencil getImgPx !(i, j) =
      loop 0 (< kM) (+ 1) 0 $ \ !iK !acc0 ->
        let !iD = i + iK - kM2 in
          loop 0 (< kN) (+ 1) acc0 $ \ !jK !acc1 ->
            let !jD = j + jK - kN2 in
              acc1 + liftPx (* getX (unsafeIndex kernelM (iK, jK))) (getImgPx (iD, jD))
    {-# INLINE getStencil #-}
{-# INLINE correlate #-}

data X = X deriving (Eq, Enum, Bounded, Show, Typeable)

newtype instance Pixel X e = PixelX { getX :: e } deriving (Ord, Eq)

-- | Convert to integral streaching it's value up to a maximum value.
stretch :: forall a b. (RealFrac a, Floating a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)
{-# INLINE stretch #-}


-- | Clamp a value to @[0, 1]@ range.
clamp01 :: (Ord a, Floating a) => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}




{-------------------------------------------------------------COLORSPACE-----------------------------------------------------------------}

data family Pixel cs e :: *


class (Eq cs, Enum cs, Show cs, Bounded cs, Typeable cs,
      Eq (Pixel cs e), -- VU.Unbox (Components cs e),
      Num e, Typeable e)
      => ColorSpace cs e where

  -- type Components cs e


  -- | Construt a Pixel by replicating the same value across all of the components.
  promote :: e -> Pixel cs e

  -- | Map a function over all Pixel's componenets.
  liftPx :: (e -> e) -> Pixel cs e -> Pixel cs e

  -- | Zip two Pixels with a function.
  liftPx2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e


  -- | Right fold over all Pixel's components.
  foldrPx :: (e -> b -> b) -> b -> Pixel cs e -> b
  foldrPx f !z0 !xs = foldlPx f' id xs z0
      where f' k x !z = k $! f x z

  -- | Left strict fold over all Pixel's components.
  foldlPx :: (b -> e -> b) -> b -> Pixel cs e -> b
  foldlPx f !z0 !xs = foldrPx f' id xs z0
      where f' x k !z = k $! f z x



-- | Base array like representation for an image.
class (Typeable arr, ColorSpace cs e, SuperClass arr cs e) =>
      BaseArray arr cs e where

  -- | Required array specific constraints for an array element.
  type SuperClass arr cs e :: Constraint

  -- | Underlying image representation.
  data Image arr cs e

  -- | Get dimensions of an image.
  --
  -- >>> frog <- readImageRGB VU "images/frog.jpg"
  -- >>> frog
  -- <Image VectorUnboxed RGB (Double): 200x320>
  -- >>> dims frog
  -- (200,320)
  --
  dims :: Image arr cs e -> (Int, Int)

class (MArray (Manifest arr) cs e, BaseArray arr cs e) => Array arr cs e where

  type Manifest arr :: *

  type Vector arr :: * -> *

  -- | Create an Image by supplying it's dimensions and a pixel generating
  -- function.
  makeImage :: (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
            -> ((Int, Int) -> Pixel cs e)
               -- ^ A function that takes (@i@-th row, and @j@-th column) as an
               -- argument and returns a pixel for that location.
            -> Image arr cs e 

  makeImageWindowed :: (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
                    -> (Int, Int) -- ^ Starting index
                    -> (Int, Int) -- ^ Size of the window
                    -> ((Int, Int) -> Pixel cs e)
                       -- ^ Function that generates inner pixels.
                    -> ((Int, Int) -> Pixel cs e)
                       -- ^ Function that generates border pixels
                    -> Image arr cs e 

  -- | Create a scalar image, required for various operations on images with
  -- a scalar.
  scalar :: Pixel cs e -> Image arr cs e

  -- | Map a function over a an image.
  map :: Array arr cs' e' =>
         (Pixel cs' e' -> Pixel cs e)
         -- ^ A function that takes a pixel of a source image and returns a pixel
         -- for the result image a the same location.
      -> Image arr cs' e' -- ^ Source image.
      -> Image arr cs e   -- ^ Result image. 

  -- | Zip two images with a function
  zipWith :: (Array arr cs1 e1, Array arr cs2 e2) =>
             (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
          -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e 

  -- | Transpose an image
  transpose :: Image arr cs e -> Image arr cs e

  -- | Undirected reduction of an image.
  fold :: (Pixel cs e -> Pixel cs e -> Pixel cs e) -- ^ An associative folding function.
       -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
       -> Image arr cs e -- ^ Source image.
       -> Pixel cs e

  -- | `Array` class does not enforce an image to be represented as concrete
  -- array of pixels in memory, but if at any time it is desired for the image
  -- to be brought to a computed state, this function can be used.
  compute :: Image arr cs e -> Image arr cs e

  -- | Each array has a sibling `Manifest` array representation, which
  toManifest :: Image arr cs e -> Image (Manifest arr) cs e

-- | Array representation that is actually has real data stored in memory, hence
-- allowing for image indexing, forcing pixels into computed state etc.
class BaseArray arr cs e => MArray arr cs e  where
  data MImage s arr cs e

  -- | Get a pixel at @(i, j)@ location without any bounds checks.
  unsafeIndex :: Image arr cs e -> (Int, Int) -> Pixel cs e

  -- | Make sure that an image is fully evaluated.
  -- deepSeqImage :: Image arr cs e -> a -> a


-- | Approach to be used near the borders during various transformations.
-- Whenever a function needs information not only about a pixel of interest, but
-- also about it's neighbours, it will go out of bounds around the image edges,
-- hence is this set of approaches that can be used in such situtation.
data Border px =
  Fill !px    -- ^ Fill in a constant pixel.
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


-- | Border handling function. If @(i, j)@ location is within bounds, then supplied
-- lookup function will be used, otherwise it will be handled according to a
-- supplied border strategy.
handleBorderIndex :: Border px -- ^ Border handling strategy.
                   -> (Int, Int) -- ^ Image dimensions
                   -> ((Int, Int) -> px) -- ^ Image's indexing function.
                   -> (Int, Int) -- ^ @(i, j)@ location of a pixel lookup.
                   -> px
handleBorderIndex ~border !(m, n) getPx !(i, j) =
  if north || east || south || west
  then case border of
    Fill px  -> px
    Wrap     -> getPx (i `mod` m, j `mod` n)
    Edge     -> getPx (if north then 0 else if south then m - 1 else i,
                       if west then 0 else if east then n - 1 else j)
    Reflect  -> getPx (if north then (abs i - 1) `mod` m else
                         if south then (-i - 1) `mod` m else i,
                       if west then (abs j - 1) `mod` n else
                         if east then (-j - 1) `mod` n else j)
    Continue -> getPx (if north then abs i `mod` m else
                         if south then (-i - 2) `mod` m else i,
                       if west then abs j `mod` n else
                         if east then (-j - 2) `mod` n else j)
  else getPx (i, j)
  where
    !north = i < 0
    !south = i >= m
    !west  = j < 0
    !east  = j >= n
{-# INLINE handleBorderIndex #-}


-- | Image indexing function that uses a special border resolutions strategy for
-- out of bounds pixels.
borderIndex :: MArray arr cs e =>
               Border (Pixel cs e) -> Image arr cs e -> (Int, Int) -> Pixel cs e
borderIndex ~atBorder !img = handleBorderIndex atBorder (dims img) (unsafeIndex img)
{-# INLINE borderIndex #-}

instance ColorSpace cs e => Num (Pixel cs e) where
  (+)         = liftPx2 (+)
  {-# INLINE (+) #-}
  (-)         = liftPx2 (-)
  {-# INLINE (-) #-}
  (*)         = liftPx2 (*)
  {-# INLINE (*) #-}
  abs         = liftPx abs
  {-# INLINE abs #-}
  signum      = liftPx signum
  {-# INLINE signum #-}
  fromInteger = promote . fromInteger
  {-# INLINE fromInteger #-}

-- instance ColorSpace cs e => Num  (cs (Vec 256 (Signed 8)))

instance (ColorSpace cs e, Fractional e) => Fractional (Pixel cs e) where
  (/)          = liftPx2 (/) 
  {-# INLINE (/) #-}
  recip        = liftPx recip
  {-# INLINE recip #-}
  fromRational = promote . fromRational
  {-# INLINE fromRational #-}


instance Array arr cs e => Num (Image arr cs e) where
  (+)         = GAUSSIAN.zipWith (+)
  {-# INLINE (+) #-}
  (-)         = GAUSSIAN.zipWith (-)
  {-# INLINE (-) #-}
  (*)         = GAUSSIAN.zipWith (*)
  {-# INLINE (*) #-}
  abs         = GAUSSIAN.map abs
  {-# INLINE abs #-}
  signum      = GAUSSIAN.map signum
  {-# INLINE signum #-}
  fromInteger = scalar . fromInteger
  {-# INLINE fromInteger #-}

instance (Fractional (Pixel cs e), Array arr cs e) =>
         Fractional (Image arr cs e) where
  (/)          = GAUSSIAN.zipWith (/)
  {-# INLINE (/) #-}
  fromRational = scalar . fromRational
  {-# INLINE fromRational #-}



instance BaseArray arr cs e =>
         Show (Image arr cs e) where
  show (dims -> (m, n)) =
    "<Image " Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy arr)) " " Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy cs)) " (" Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy e)) "): " Prelude.++
     show m Prelude.++ "x" Prelude.++ show n Prelude.++ ">"

{------------------------------------------------------INSTANCES-------------------------------------------------------------------}

data RGB = Red | Green | Blue
 deriving (Eq,Enum,Show,Bounded)

data instance Pixel RGB (Unsigned 8) = PixelRGB (Unsigned 8) (Unsigned 8) (Unsigned 8)
  deriving Eq

instance ColorSpace RGB (Unsigned 8) where
  promote x = PixelRGB x x x

  liftPx f (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)

  liftPx2 f (PixelRGB r1 g1 b1) (PixelRGB r2 g2 b2) = PixelRGB (f r1 r2) (f g1 g2) (f b1 b2)

  foldrPx f z (PixelRGB r g b) = f r (f g (f b z))
  foldlPx f z (PixelRGB r g b) = f (f (f z r) g) b

instance KnownNat i => BaseArray (Vec i (Pixel RGB (Unsigned 8))) RGB (Unsigned 8) where

  type SuperClass (Vec i (Pixel RGB (Unsigned 8))) RGB (Unsigned 8) = () --apply constraints as needed


instance BaseArray (Vec i (Pixel RGB (Unsigned 8))) RGB (Unsigned 8) => MArray (Vec i (Pixel RGB (Unsigned 8))) RGB (Unsigned 8) where 
  --unsafeIndex img (x,y) = 


{------------------------------------------------------GAUSSIAN FUNCTION-----------------------------------------------------------}

-- | Filter that can be applied to an image using `applyFilter`.
--
-- @since 1.5.3
data Filter arr cs e = Filter
  { applyFilter :: Image arr cs e -> Image arr cs e -- ^ Apply a filter to an image
  }


-- | Create a Gaussian Filter.
--
-- @since 1.5.3
gaussianLowPass :: (Array arr cs e, Array arr X e, Floating e, Fractional e) =>
                   Int -- ^ Radius
                -> e -- ^ Sigma
                -> Border (Pixel cs e) -- ^ Border resolution technique.
                -> Filter arr cs e
gaussianLowPass !r !sigma border =
  Filter (correlate border gV' . correlate border gV)
  where
    !gV = compute $ (gauss / scalar weight)
    !gV' = compute $ GAUSSIAN.transpose gV
    !gauss = makeImage (1, n) getPx
    !weight = GAUSSIAN.fold (+) 0 gauss
    !n = 2 * r + 1
    !sigma2sq = 2 * sigma ^ (2 :: Int)
    getPx (_, j) = promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)
    {-# INLINE getPx #-}
{-# INLINE gaussianLowPass #-}



-- | Create a Gaussian Blur filter. Radius will be derived from standard
-- deviation: @ceiling (2*sigma)@ and `Edge` border resolution will be
-- utilized. If custom radius and/or border resolution is desired,
-- `gaussianLowPass` can be used instead.
--
-- @since 1.5.3
topEntity :: (Array arr cs e, Array arr X e, Floating e, RealFrac e) =>
                e -- ^ Sigma
             -> Filter arr cs e
topEntity !sigma = gaussianLowPass (ceiling (2*sigma)) sigma Edge
--{-# INLINE gaussianBlur #-}
