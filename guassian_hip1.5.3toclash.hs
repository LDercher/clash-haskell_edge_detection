module GAUSSIAN where
import Clash.Prelude
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Unboxed               as VU
--import           Graphics.Image.Interface          as I
import           Data.Typeable                     (Typeable, showsTypeRep,
                                                    typeRep)

--https://hackage.haskell.org/package/hip-1.0.1/candidate/docs/src/Graphics-Image-Interface.html#Elevator
                                                    
import Prelude hiding (and, map, zipWith, sum, product)
-- #if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
import Data.Maybe
import Data.Foldable --(Foldable(foldMap))
-- #endif
import GHC.Exts (Constraint)
import Data.Typeable --(Typeable, showsTypeRep, typeOf, Proxy)
import Control.DeepSeq --(NFData(rnf))
import Data.Word
import           Control.Monad.ST
import Control.Applicative
import Control.Monad.Primitive (PrimMonad (..))
import qualified Data.Complex  as C
--import           Graphics.Image.Interface              as I
--import           Graphics.Image.Processing.Convolution
--import           Graphics.Image.ColorSpace               (X)

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

class (Eq e, Num e, Typeable e, VU.Unbox e) => Elevator e where

  -- | Values are scaled to @[0, 255]@ range.
  toWord8 :: e -> Word8

  -- | Values are scaled to @[0, 65535]@ range.
  toWord16 :: e -> Word16

  -- | Values are scaled to @[0, 4294967295]@ range.
  toWord32 :: e -> Word32

  -- | Values are scaled to @[0, 18446744073709551615]@ range.
  toWord64 :: e -> Word64

  -- | Values are scaled to @[0.0, 1.0]@ range.
  toFloat :: e -> Float

  -- | Values are scaled to @[0.0, 1.0]@ range.
  toDouble :: e -> Double

  -- | Values are scaled from @[0.0, 1.0]@ range.
  fromDouble :: Double -> e


-- | Lower the precision
dropDown :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
dropDown !e = fromIntegral $ fromIntegral e `div` ((maxBound :: a) `div`
                                                   fromIntegral (maxBound :: b))
{-# INLINE dropDown #-}

-- | Increase the precision
raiseUp :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
raiseUp !e = fromIntegral e * ((maxBound :: b) `div` fromIntegral (maxBound :: a))
{-# INLINE raiseUp #-}

-- | Convert to fractional with value less than or equal to 1.
squashTo1 :: forall a b. (Fractional b, Integral a, Bounded a) => a -> b
squashTo1 !e = fromIntegral e / fromIntegral (maxBound :: a)
{-# INLINE squashTo1 #-}

-- | Convert to integral streaching it's value up to a maximum value.
stretch :: forall a b. (RealFrac a, Floating a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)
{-# INLINE stretch #-}


-- | Clamp a value to @[0, 1]@ range.
clamp01 :: (Ord a, Floating a) => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}


-- | Values between @[0, 255]]@
instance Elevator Word8 where
  toWord8 = id
  {-# INLINE toWord8 #-}
  toWord16 = raiseUp
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord8
  {-# INLINE fromDouble #-}


-- | Values between @[0, 65535]]@
instance Elevator Word16 where
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = id
  {-# INLINE toWord16 #-}
  toWord32 = raiseUp
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord16
  {-# INLINE fromDouble #-}


-- | Values between @[0, 4294967295]@
instance Elevator Word32 where
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = id
  {-# INLINE toWord32 #-}
  toWord64 = raiseUp
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord32
  {-# INLINE fromDouble #-}


-- | Values between @[0, 18446744073709551615]@
instance Elevator Word64 where
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = dropDown
  {-# INLINE toWord32 #-}
  toWord64 = id
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = toWord64
  {-# INLINE fromDouble #-}

-- | Values between @[0, 18446744073709551615]@ on 64bit
instance Elevator Word where
  toWord8 = dropDown
  {-# INLINE toWord8 #-}
  toWord16 = dropDown
  {-# INLINE toWord16 #-}
  toWord32 = dropDown
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral
  {-# INLINE toWord64 #-}
  toFloat = squashTo1
  {-# INLINE toFloat #-}
  toDouble = squashTo1
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}
{--
 | Values between @[0, 127]@
instance Elevator Int8 where
  toWord8 = fromIntegral . (max 0)
  {-# INLINE toWord8 #-}
 -- toWord16 = raiseUp . (max 0)
 -- {-# INLINE toWord16 #-}
 -- toWord32 = raiseUp . (max 0)
 -- {-# INLINE toWord32 #-}
 -- toWord64 = raiseUp . (max 0)
 -- {-# INLINE toWord64 #-}
 -- toFloat = squashTo1 . (max 0)
 -- {-# INLINE toFloat #-}
 -- toDouble = squashTo1 . (max 0)
 -- {-# INLINE toDouble #-}
 -- fromDouble = stretch . clamp01
 -- {-# INLINE fromDouble #-}


-- | Values between @[0, 32767]@
--instance Elevator Int16 where
--  toWord8 = dropDown . (max 0)
--  {-# INLINE toWord8 #-}
-- toWord16 = fromIntegral . (max 0)
--{-# INLINE toWord16 #-}
--  toWord32 = raiseUp . (max 0)
--  {-# INLINE toWord32 #-}
--  toWord64 = raiseUp . (max 0)
--  {-# INLINE toWord64 #-}
--  toFloat = squashTo1 . (max 0)
--  {-# INLINE toFloat #-}
--  toDouble = squashTo1 . (max 0)
--  {-# INLINE toDouble #-}
--  fromDouble = stretch . clamp01
--  {-# INLINE fromDouble #-}


-- | Values between @[0, 2147483647]@-
--instance Elevator Int32 where
--  toWord8 = dropDown . (max 0)
--  {-# INLINE toWord8 #-}
--  toWord16 = dropDown . (max 0)
--  {-# INLINE toWord16 #-}
--  toWord32 = fromIntegral . (max 0)
--  {-# INLINE toWord32 #-}
--  toWord64 = raiseUp . (max 0)
--  {-# INLINE toWord64 #-}
--  toFloat = squashTo1 . (max 0)
--  {-# INLINE toFloat #-}
--  toDouble = squashTo1 . (max 0)
--  {-# INLINE toDouble #-}
-- fromDouble = stretch . clamp01
--  {-# INLINE fromDouble #-}


-- | Values between @[0, 9223372036854775807]@
--instance Elevator Int64 where
--  toWord8 = dropDown . (max 0)
--  {-# INLINE toWord8 #-}
--  toWord16 = dropDown . (max 0)
--  {-# INLINE toWord16 #-}
--  toWord32 = dropDown . (max 0)
--  {-# INLINE toWord32 #-}
--  toWord64 = fromIntegral . (max 0)
--  {-# INLINE toWord64 #-}
--  toFloat = squashTo1 . (max 0)
--  {-# INLINE toFloat #-}
--  toDouble = squashTo1 . (max 0)
--  {-# INLINE toDouble #-}
--  fromDouble = stretch . clamp01
--  {-# INLINE fromDouble #-}
 --}

-- | Values between @[0, 9223372036854775807]@ on 64bit
instance Elevator Int where
  toWord8 = dropDown . (max 0)
  {-# INLINE toWord8 #-}
  toWord16 = dropDown . (max 0)
  {-# INLINE toWord16 #-}
  toWord32 = dropDown . (max 0)
  {-# INLINE toWord32 #-}
  toWord64 = fromIntegral . (max 0)
  {-# INLINE toWord64 #-}
  toFloat = squashTo1 . (max 0)
  {-# INLINE toFloat #-}
  toDouble = squashTo1 . (max 0)
  {-# INLINE toDouble #-}
  fromDouble = stretch . clamp01
  {-# INLINE fromDouble #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Float where
  toWord8 = stretch . clamp01
  {-# INLINE toWord8 #-}
  toWord16 = stretch . clamp01
  {-# INLINE toWord16 #-}
  toWord32 = stretch . clamp01
  {-# INLINE toWord32 #-}
  toWord64 = stretch . clamp01
  {-# INLINE toWord64 #-}
  toFloat = id
  {-# INLINE toFloat #-}
  toDouble = float2Double
  {-# INLINE toDouble #-}
  fromDouble = toFloat
  {-# INLINE fromDouble #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Double where
  toWord8 = stretch . clamp01
  {-# INLINE toWord8 #-}
  toWord16 = stretch . clamp01
  {-# INLINE toWord16 #-}
  toWord32 = stretch . clamp01
  {-# INLINE toWord32 #-}
  toWord64 = stretch . clamp01
  {-# INLINE toWord64 #-}
  toFloat = double2Float
  {-# INLINE toFloat #-}
  toDouble = id
  {-# INLINE toDouble #-}
  fromDouble = id
  {-# INLINE fromDouble #-}


-- | Discards imaginary part and changes precision of real part.
instance (Num e, Elevator e, RealFloat e) => Elevator (C.Complex e) where
  toWord8 = toWord8 . C.realPart
  {-# INLINE toWord8 #-}
  toWord16 = toWord16 . C.realPart
  {-# INLINE toWord16 #-}
  toWord32 = toWord32 . C.realPart
  {-# INLINE toWord32 #-}
  toWord64 = toWord64 . C.realPart
  {-# INLINE toWord64 #-}
  toFloat = toFloat . C.realPart
  {-# INLINE toFloat #-}
  toDouble = toDouble . C.realPart
  {-# INLINE toDouble #-}
  fromDouble = (C.:+ 0) . fromDouble
  {-# INLINE fromDouble #-}

{-------------------------------------------------------------COLORSPACE-----------------------------------------------------------------}

data family Pixel cs e :: *


class (Eq cs, Enum cs, Show cs, Bounded cs, Typeable cs,
      Eq (Pixel cs e), VU.Unbox (Components cs e), Elevator e)
      => ColorSpace cs e where

  type Components cs e

  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toComponents :: Pixel cs e -> Components cs e

  -- | Convert from an elemnt representation back to a Pixel.
  fromComponents :: Components cs e -> Pixel cs e

  -- | Construt a Pixel by replicating the same value across all of the components.
  promote :: e -> Pixel cs e

  -- | Retrieve Pixel's component value
  getPxC :: Pixel cs e -> cs -> e

  -- | Set Pixel's component value
  setPxC :: Pixel cs e -> cs -> e -> Pixel cs e

  -- | Map a channel aware function over all Pixel's components.
  mapPxC :: (cs -> e -> e) -> Pixel cs e -> Pixel cs e

  -- | Map a function over all Pixel's componenets.
  liftPx :: (e -> e) -> Pixel cs e -> Pixel cs e

  -- | Zip two Pixels with a function.
  liftPx2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e

  -- | Left fold on two pixels a the same time.
  foldlPx2 :: (b -> e -> e -> b) -> b -> Pixel cs e -> Pixel cs e -> b

  -- | Right fold over all Pixel's components.
  foldrPx :: (e -> b -> b) -> b -> Pixel cs e -> b
  foldrPx f !z0 !xs = foldlPx f' id xs z0
      where f' k x !z = k $! f x z

  -- | Left strict fold over all Pixel's components.
  foldlPx :: (b -> e -> b) -> b -> Pixel cs e -> b
  foldlPx f !z0 !xs = foldrPx f' id xs z0
      where f' x k !z = k $! f z x

  foldl1Px :: (e -> e -> e) -> Pixel cs e -> e
  foldl1Px f !xs = fromMaybe (error "foldl1Px: empty Pixel")
                  (foldlPx mf Nothing xs)
      where
        mf m !y = Just (case m of
                           Nothing -> y
                           Just x  -> f x y)
  toListPx :: Pixel cs e -> [e]
  toListPx !px = foldr' f [] (enumFrom (toEnum 0))
    where f !cs !ls = getPxC px cs:ls



-- | A color space that supports transparency.
class (ColorSpace (Opaque cs) e, ColorSpace cs e) => AlphaSpace cs e where
  -- | A corresponding opaque version of this color space.
  type Opaque cs

  -- | Get an alpha channel of a transparant pixel.
  getAlpha :: Pixel cs e -> e

  -- | Add an alpha channel to an opaque pixel.
  --
  -- @ addAlpha 0 (PixelHSI 1 2 3) == PixelHSIA 1 2 3 0 @
  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e

  -- | Convert a transparent pixel to an opaque one by dropping the alpha
  -- channel.
  --
  -- @ dropAlpha (PixelRGBA 1 2 3 4) == PixelRGB 1 2 3 @
  --
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e


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

class (VG.Vector (Vector arr) (Pixel cs e),
       MArray (Manifest arr) cs e, BaseArray arr cs e) => Array arr cs e where

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

  -- | Retrieves a pixel at @(0, 0)@ index. Useful together with `fold`, when
  -- arbitrary initial pixel is needed.
  index00 :: Image arr cs e -> Pixel cs e

  -- | Map a function over a an image.
  map :: Array arr cs' e' =>
         (Pixel cs' e' -> Pixel cs e)
         -- ^ A function that takes a pixel of a source image and returns a pixel
         -- for the result image a the same location.
      -> Image arr cs' e' -- ^ Source image.
      -> Image arr cs e   -- ^ Result image.

  -- | Map an index aware function over each pixel in an image.
  imap :: Array arr cs' e' =>
          ((Int, Int) -> Pixel cs' e' -> Pixel cs e)
        -- ^ A function that takes an index @(i, j)@, a pixel at that location
        -- and returns a new pixel at the same location for the result image.
       -> Image arr cs' e' -- ^ Source image.
       -> Image arr cs e   -- ^ Result image.

  -- | Zip two images with a function
  zipWith :: (Array arr cs1 e1, Array arr cs2 e2) =>
             (Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
          -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e

  -- | Zip two images with an index aware function
  izipWith :: (Array arr cs1 e1, Array arr cs2 e2) =>
              ((Int, Int) -> Pixel cs1 e1 -> Pixel cs2 e2 -> Pixel cs e)
           -> Image arr cs1 e1 -> Image arr cs2 e2 -> Image arr cs e

  -- | Traverse an image
  traverse :: Array arr cs' e' =>
              Image arr cs' e' -- ^ Source image.
           -> ((Int, Int) -> (Int, Int))
           -- ^ Function that takes dimensions of a source image and returns
           -- dimensions of a new image.
           -> (((Int, Int) -> Pixel cs' e') ->
               (Int, Int) -> Pixel cs e)
           -- ^ Function that receives a pixel getter (a source image index
           -- function), a location @(i, j)@ in a new image and returns a pixel
           -- for that location.
           -> Image arr cs e

  -- | Traverse two images.
  traverse2 :: (Array arr cs1 e1, Array arr cs2 e2) =>
               Image arr cs1 e1 -- ^ First source image.
            -> Image arr cs2 e2 -- ^ Second source image.
            -> ((Int, Int) -> (Int, Int) -> (Int, Int))
            -- ^ Function that produces dimensions for the new image.
            -> (((Int, Int) -> Pixel cs1 e1) ->
                ((Int, Int) -> Pixel cs2 e2) ->
                (Int, Int) -> Pixel cs e)
            -- ^ Function that produces pixels for the new image.
            -> Image arr cs e

  -- | Transpose an image
  transpose :: Image arr cs e -> Image arr cs e

  -- | Backwards permutation of an image.
  backpermute :: (Int, Int) -- ^ Dimensions of a result image.
              -> ((Int, Int) -> (Int, Int))
                 -- ^ Function that maps an index of a source image to an index
                 -- of a result image.
              -> Image arr cs e -- ^ Source image.
              -> Image arr cs e -- ^ Result image.

  -- | Construct an image from a nested rectangular shaped list of pixels.
  -- Length of an outer list will constitute @m@ rows, while the length of inner lists -
  -- @n@ columns. All of the inner lists must be the same length and greater than @0@.
  --
  -- >>> fromLists [[PixelY (fromIntegral (i*j) / 60000) | j <- [1..300]] | i <- [1..200]]
  -- <Image VectorUnboxed Y (Double): 200x300>
  --
  -- <<images/grad_fromLists.png>>
  --
  fromLists :: [[Pixel cs e]]
            -> Image arr cs e

  -- | Perform matrix multiplication on two images. Inner dimensions must agree.
  (|*|) :: Image arr cs e -> Image arr cs e -> Image arr cs e

  -- | Undirected reduction of an image.
  fold :: (Pixel cs e -> Pixel cs e -> Pixel cs e) -- ^ An associative folding function.
       -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
       -> Image arr cs e -- ^ Source image.
       -> Pixel cs e

  -- | Undirected reduction of an image with an index aware function.
  foldIx :: (Pixel cs e -> (Int, Int) -> Pixel cs e -> Pixel cs e)
            -- ^ Function that takes an accumulator, index, a pixel at that
            -- index and returns a new accumulator pixel.
         -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
         -> Image arr cs e -- ^ Source image.
         -> Pixel cs e

  -- | Pixelwise equality function of two images. Images are
  -- considered distinct if either images' dimensions or at least one pair of
  -- corresponding pixels are not the same. Used in defining an in instance for
  -- the 'Eq' typeclass.
  eq :: Image arr cs e -> Image arr cs e -> Bool

  -- | `Array` class does not enforce an image to be represented as concrete
  -- array of pixels in memory, but if at any time it is desired for the image
  -- to be brought to a computed state, this function can be used.
  compute :: Image arr cs e -> Image arr cs e

  -- | Each array has a sibling `Manifest` array representation, which
  toManifest :: Image arr cs e -> Image (Manifest arr) cs e

  -- | Convert an image to a flattened 'Vector'. For all current representations
  -- it is a __O(1)__ opeartion.
  --
  -- >>> toVector $ makeImage (3, 2) (\(i, j) -> PixelY $ fromIntegral (i+j))
  -- fromList [<Luma:(0.0)>,<Luma:(1.0)>,<Luma:(1.0)>,<Luma:(2.0)>,<Luma:(2.0)>,<Luma:(3.0)>]
  --
  toVector :: Image arr cs e -> Vector arr (Pixel cs e)

  -- | Construct a two dimensional image with @m@ rows and @n@ columns from a
  --  flat 'Vector' of length @k@. For all current representations it is a
  --  __O(1)__ opeartion. Make sure that @m * n = k@.
  --
  -- >>> fromVector (200, 300) $ generate 60000 (\i -> PixelY $ fromIntegral i / 60000)
  -- <Image Vector Luma: 200x300>
  --
  -- <<images/grad_fromVector.png>>
  --
  fromVector :: (Int, Int) -> Vector arr (Pixel cs e) -> Image arr cs e


-- | Array representation that is actually has real data stored in memory, hence
-- allowing for image indexing, forcing pixels into computed state etc.
class BaseArray arr cs e => MArray arr cs e  where
  data MImage s arr cs e

  -- | Get a pixel at @(i, j)@ location without any bounds checks.
  unsafeIndex :: Image arr cs e -> (Int, Int) -> Pixel cs e

  -- | Make sure that an image is fully evaluated.
  deepSeqImage :: Image arr cs e -> a -> a

  -- | Fold an image from the left in a row major order.
  foldl :: (a -> Pixel cs e -> a) -> a -> Image arr cs e -> a

  -- | Fold an image from the right in a row major order.
  foldr :: (Pixel cs e -> a -> a) -> a -> Image arr cs e -> a

  -- | Create an Image by supplying it's dimensions and a monadic pixel
  -- generating action.
  makeImageM :: (Functor m, Monad m) =>
                (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
             -> ((Int, Int) -> m (Pixel cs e))
                -- ^ A function that takes (@i@-th row, and @j@-th column) as an
                -- argument and generates a pixel for that location.
             -> m (Image arr cs e)

  -- | Monading mapping over an image.
  mapM :: (MArray arr cs' e', Functor m, Monad m) =>
          (Pixel cs' e' -> m (Pixel cs e)) -> Image arr cs' e' -> m (Image arr cs e)

  -- | Monading mapping over an image. Result is discarded.
  mapM_ :: (Functor m, Monad m) => (Pixel cs e -> m b) -> Image arr cs e -> m ()

  -- | Monadic folding.
  foldM :: (Functor m, Monad m) => (a -> Pixel cs e -> m a) -> a -> Image arr cs e -> m a

  -- | Monadic folding. Result is discarded.
  foldM_ :: (Functor m, Monad m) => (a -> Pixel cs e -> m a) -> a -> Image arr cs e -> m ()

  -- | Get dimensions of a mutable image.
  mdims :: MImage s arr cs e -> (Int, Int)

  -- | Yield a mutable copy of an image.
  thaw :: (Functor m, PrimMonad m) =>
          Image arr cs e -> m (MImage (PrimState m) arr cs e)

  -- | Yield an immutable copy of an image.
  freeze :: (Functor m, PrimMonad m) =>
            MImage (PrimState m) arr cs e -> m (Image arr cs e)

  -- | Create a mutable image with given dimensions. Pixels are likely uninitialized.
  new :: (Functor m, PrimMonad m) =>
         (Int, Int) -> m (MImage (PrimState m) arr cs e)

  -- | Yield the pixel at a given location.
  read :: (Functor m, PrimMonad m) =>
          MImage (PrimState m) arr cs e -> (Int, Int) -> m (Pixel cs e)

  -- | Set a pixel at a given location.
  write :: (Functor m, PrimMonad m) =>
           MImage (PrimState m) arr cs e -> (Int, Int) -> Pixel cs e -> m ()

  -- | Swap pixels at given locations.
  swap :: (Functor m, PrimMonad m) =>
          MImage (PrimState m) arr cs e -> (Int, Int) -> (Int, Int) -> m ()


-- | Run a stateful monadic computation that generates an image.
createImage
  :: MArray arr cs e
  => (forall s. ST s (MImage s arr cs e)) -> Image arr cs e
createImage create = runST (create >>= freeze)


-- | Exchange the underlying array representation of an image.
exchange :: (Array arr' cs e, Array arr cs e) =>
            arr -- ^ New representation of an image.
         -> Image arr' cs e -- ^ Source image.
         -> Image arr cs e
exchange _ img@(dims -> (1, 1)) = scalar $ index00 img
exchange _ img = fromVector (dims img) $ VG.convert $ toVector img
{-# INLINE exchange #-}


--{-# RULES
--"exchange/id" forall arr. exchange arr = id
-- #-}


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


-- | Get a pixel at @i@-th and @j@-th location.
--
-- >>> let grad_gray = makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
-- >>> index grad_gray (20, 30) == PixelY ((20*30) / (200*200))
-- True
--
index :: MArray arr cs e => Image arr cs e -> (Int, Int) -> Pixel cs e
index !img !ix = borderIndex (error $ show img Prelude.++ " - Index out of bounds: " Prelude.++ show ix) img ix
{-# INLINE index #-}


-- | Image indexing function that returns a default pixel if index is out of bounds.
defaultIndex :: MArray arr cs e =>
                Pixel cs e -> Image arr cs e -> (Int, Int) -> Pixel cs e
defaultIndex !px !img = handleBorderIndex (Fill px) (dims img) (index img)
{-# INLINE defaultIndex #-}


-- | Image indexing function that uses a special border resolutions strategy for
-- out of bounds pixels.
borderIndex :: MArray arr cs e =>
               Border (Pixel cs e) -> Image arr cs e -> (Int, Int) -> Pixel cs e
borderIndex ~atBorder !img = handleBorderIndex atBorder (dims img) (unsafeIndex img)
{-# INLINE borderIndex #-}


-- | Image indexing function that returns @'Nothing'@ if index is out of bounds,
-- @'Just' px@ otherwise.
maybeIndex :: MArray arr cs e =>
              Image arr cs e -> (Int, Int) -> Maybe (Pixel cs e)
maybeIndex !img@(dims -> (m, n)) !(i, j) =
  if i >= 0 && j >= 0 && i < m && j < n then Just $ unsafeIndex img (i, j) else Nothing
{-# INLINE maybeIndex #-}


-- | 2D to a flat vector index conversion.
--
-- __Note__: There is an implicit assumption that @j < n@
fromIx :: Int -- ^ @n@ columns
       -> (Int, Int) -- ^ @(i, j)@ row, column index
       -> Int -- ^ Flat vector index
fromIx !n !(i, j) = n * i + j
{-# INLINE fromIx #-}


-- | Flat vector to 2D index conversion.
toIx :: Int -- ^ @n@ columns
     -> Int -- ^ Flat vector index
     -> (Int, Int) -- ^ @(i, j)@ row, column index
toIx !n !k = divMod k n
{-# INLINE toIx #-}

checkDims :: String -> (Int, Int) -> (Int, Int)
checkDims err !sz@(m, n)
  | m <= 0 || n <= 0 =
    error $
    show err Prelude.++ ": dimensions are expected to be positive: " Prelude.++ show sz
  | otherwise = sz
{-# INLINE checkDims #-}


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


instance (ColorSpace cs e, Fractional e) => Fractional (Pixel cs e) where
  (/)          = liftPx2 (/)
  {-# INLINE (/) #-}
  recip        = liftPx recip
  {-# INLINE recip #-}
  fromRational = promote . fromRational
  {-# INLINE fromRational #-}


instance (ColorSpace cs e, Floating e) => Floating (Pixel cs e) where
  pi      = promote pi
  {-# INLINE pi #-}
  exp     = liftPx exp
  {-# INLINE exp #-}
  log     = liftPx log
  {-# INLINE log #-}
  sin     = liftPx sin
  {-# INLINE sin #-}
  cos     = liftPx cos
  {-# INLINE cos #-}
  asin    = liftPx asin
  {-# INLINE asin #-}
  atan    = liftPx atan
  {-# INLINE atan #-}
  acos    = liftPx acos
  {-# INLINE acos #-}
  sinh    = liftPx sinh
  {-# INLINE sinh #-}
  cosh    = liftPx cosh
  {-# INLINE cosh #-}
  asinh   = liftPx asinh
  {-# INLINE asinh #-}
  atanh   = liftPx atanh
  {-# INLINE atanh #-}
  acosh   = liftPx acosh
  {-# INLINE acosh #-}

instance (ColorSpace cs e, Bounded e) => Bounded (Pixel cs e) where
  maxBound = promote maxBound
  {-# INLINE maxBound #-}
  minBound = promote minBound
  {-# INLINE minBound #-}

instance (Foldable (Pixel cs), NFData e) => NFData (Pixel cs e) where

  rnf = foldr' deepseq ()
  {-# INLINE rnf #-}

instance Array arr cs e => Eq (Image arr cs e) where
  (==) = eq
  {-# INLINE (==) #-}

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


instance (Floating (Pixel cs e), Array arr cs e) =>
         Floating (Image arr cs e) where
  pi    = scalar pi
  {-# INLINE pi #-}
  exp   = GAUSSIAN.map exp
  {-# INLINE exp #-}
  log   = GAUSSIAN.map log
  {-# INLINE log #-}
  sin   = GAUSSIAN.map sin
  {-# INLINE sin #-}
  cos   = GAUSSIAN.map cos
  {-# INLINE cos #-}
  asin  = GAUSSIAN.map asin
  {-# INLINE asin #-}
  atan  = GAUSSIAN.map atan
  {-# INLINE atan #-}
  acos  = GAUSSIAN.map acos
  {-# INLINE acos #-}
  sinh  = GAUSSIAN.map sinh
  {-# INLINE sinh #-}
  cosh  = GAUSSIAN.map cosh
  {-# INLINE cosh #-}
  asinh = GAUSSIAN.map asinh
  {-# INLINE asinh #-}
  atanh = GAUSSIAN.map atanh
  {-# INLINE atanh #-}
  acosh = GAUSSIAN.map acosh
  {-# INLINE acosh #-}


instance MArray arr cs e => NFData (Image arr cs e) where
  rnf img = img `deepSeqImage` ()
  {-# INLINE rnf #-}


instance BaseArray arr cs e =>
         Show (Image arr cs e) where
  show (dims -> (m, n)) =
    "<Image " Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy arr)) " " Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy cs)) " (" Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy e)) "): " Prelude.++
     show m Prelude.++ "x" Prelude.++ show n Prelude.++ ">"


instance MArray arr cs e =>
         Show (MImage st arr cs e) where
  show (mdims -> (m, n)) =
    "<MutableImage " Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy arr)) " " Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy cs)) " (" Prelude.++
    showsTypeRep (typeRep (Proxy :: Proxy e)) "): " Prelude.++
     show m Prelude.++ "x" Prelude.++ show n Prelude.++ ">"

{------------------------------------------------------GUASSIAN FUNCTION-----------------------------------------------------------}

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
gaussianBlur :: (Array arr cs e, Array arr X e, Floating e, RealFrac e) =>
                e -- ^ Sigma
             -> Filter arr cs e
gaussianBlur !sigma = gaussianLowPass (ceiling (2*sigma)) sigma Edge
{-# INLINE gaussianBlur #-}


