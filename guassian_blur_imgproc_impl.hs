{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE ViewPatterns            #-}
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
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Unboxed               as VU
--import           Graphics.Image.Interface          as I
import           Data.Typeable                     (Typeable, showsTypeRep,
                                                    typeRep)

--https://hackage.haskell.org/package/hip-1.0.1/candidate/docs/src/Graphics-Image-Interface.html#Elevator
                                                    
import Prelude hiding (and, map, zipWith, sum, product)
-- #if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
import Data.Foldable (Foldable(foldMap))
-- #endif
import GHC.Exts (Constraint)
import Data.Typeable (Typeable, showsTypeRep, typeOf)
import Control.DeepSeq (NFData(rnf))
import Data.Word
import Control.Applicative
import Control.Monad.Primitive (PrimMonad (..))
import qualified Data.Colour as C



-- | This class has all included color spaces installed into it and is also
-- intended for implementing any other possible custom color spaces. Every
-- instance of this class automatically installs an associated 'Pixel' into
-- 'Num', 'Fractional', 'Floating', 'Functor', 'Applicative' and 'Foldable',
-- which in turn make it possible to be used by the rest of the library.
{- class (Eq cs, Enum cs, Show cs, Typeable cs) => ColorSpace cs where
  
  -- | Representation of a pixel, such that it can be an element of any
  -- Array. Which is usally a tuple of channels or a channel itself for single
  -- channel color spaces.
  type PixelElt cs e

  -- | A concrete Pixel representation for a particular color space.
  data Pixel cs e

  -- | Construt a pixel by replicating a same value among all of the channels.
  fromChannel :: e -> Pixel cs e

  -- | Convert a Pixel to a representation suitable for storage as an unboxed
  -- element, usually a tuple of channels.
  toElt :: Pixel cs e -> PixelElt cs e

  -- | Convert from an elemnt representation back to a Pixel.
  fromElt :: PixelElt cs e -> Pixel cs e

  -- | Retrieve Pixel's channel value
  getPxCh :: Pixel cs e -> cs -> e
  
  -- | Map a channel aware function over all Pixel's channels.
  chOp :: (cs -> e' -> e) -> Pixel cs e' -> Pixel cs e 

  -- | Map a function over all Pixel's channels.
  pxOp :: (e' -> e) -> Pixel cs e' -> Pixel cs e

  -- | Function application to a Pixel.
  chApp :: Pixel cs (e' -> e) -> Pixel cs e' -> Pixel cs e

  -- | A pixel eqiuvalent of 'foldMap'.
  pxFoldMap :: Monoid m => (e -> m) -> Pixel cs e -> m

  -- | Get a pure colour representation of a channel.
  csColour :: cs -> C.AlphaColour Double -}
  

-- | A color space that supports transparency.
class (ColorSpace (Opaque cs), ColorSpace cs) => Alpha cs where
  -- | An corresponding opaque version of this color space.
  type Opaque cs

  -- | Get an alpha channel of a transparant pixel. 
  getAlpha :: Pixel cs e -> e

  -- | Add an alpha channel of an opaque pixel.
  --
  -- @ addAlpha 0 (PixelHSI 1 2 3) == PixelHSIA 1 2 3 0 @
  addAlpha :: e -> Pixel (Opaque cs) e -> Pixel cs e
  
  -- | Convert a transparent pixel to an opaque one by dropping the alpha
  -- channel.
  --
  -- @ dropAlpha (PixelRGBA 1 2 3 4) == PixelRGB 1 2 3 @
  --
  dropAlpha :: Pixel cs e -> Pixel (Opaque cs) e

  -- | Get a corresponding opaque channel type.
  opaque :: cs -> Opaque cs


-- | A class with a set of convenient functions that allow for changing precision of
-- channels within pixels, while scaling the values to keep them in an appropriate range.
--
-- >>> let rgb = PixelRGB 0.0 0.5 1.0 :: Pixel RGB Double
-- >>> toWord8 rgb
-- <RGB:(0|128|255)> 
--
{- class Elevator e where

  toWord8 :: ColorSpace cs => Pixel cs e -> Pixel cs Word8

  toWord16 :: ColorSpace cs => Pixel cs e -> Pixel cs Word16

  toWord32 :: ColorSpace cs => Pixel cs e -> Pixel cs Word32

  toWord64 :: ColorSpace cs => Pixel cs e -> Pixel cs Word64

  toFloat :: ColorSpace cs => Pixel cs e -> Pixel cs Float

  toDouble :: ColorSpace cs => Pixel cs e -> Pixel cs Double

  fromDouble :: ColorSpace cs => Pixel cs Double -> Pixel cs e -}


-- | Base array like representation for an image.
class (Show arr, ColorSpace cs, Num (Pixel cs e),
       Functor (Pixel cs), Applicative (Pixel cs), Foldable (Pixel cs),
       Num e, Typeable e, Elt arr cs e) =>
      Array arr cs e where

  -- | Required array specific constraints for an array element.
  type Elt arr cs e :: Constraint
  type Elt arr cs e = ()
  
  -- | Underlying image representation.
  data Image arr cs e

  -- | Create an Image by supplying it's dimensions and a pixel generating
  -- function.
  makeImage :: (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
          -> ((Int, Int) -> Pixel cs e)
          -- ^ A function that takes (@i@-th row, and @j@-th column) as an
          -- argument and returns a pixel for that location.
          -> Image arr cs e

  -- | Create a singleton image, required for various operations on images with
  -- a scalar.
  singleton :: Pixel cs e -> Image arr cs e

  -- | Get dimensions of an image.
  --
  -- >>> frog <- readImageRGB "images/frog.jpg"
  -- >>> frog
  -- <Image VectorUnboxed RGB (Double): 200x320>
  -- >>> dims frog
  -- (200,320)
  --
  dims :: Image arr cs e -> (Int, Int)

  -- | Map a function over a an image.
  map :: Array arr cs' e' =>
         (Pixel cs' e' -> Pixel cs e)
         -- ^ A function that takes a pixel of a source image and returns a pixel
         -- for the result image a the same location.
      -> Image arr cs' e' -- ^ Source image.
      -> Image arr cs  e  -- ^ Result image.

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
  fromLists :: [[Pixel cs e]]
            -> Image arr cs e


-- | Array representation that is actually has real data stored in memory, hence
-- allowing for image indexing, forcing pixels into computed state etc.
class Array arr cs e => ManifestArray arr cs e where

  -- | Get a pixel at @i@-th and @j@-th location.
  --
  -- >>> let grad_gray = makeImage (200, 200) (\(i, j) -> PixelY $ fromIntegral (i*j)) / (200*200)
  -- >>> index grad_gray (20, 30) == PixelY ((20*30) / (200*200))
  -- True
  --
  index :: Image arr cs e -> (Int, Int) -> Pixel cs e
  
  -- | Make sure that an image is fully evaluated.
  deepSeqImage :: Image arr cs e -> a -> a

  -- | Perform matrix multiplication on two images. Inner dimensions must agree.
  (|*|) :: Image arr cs e -> Image arr cs e -> Image arr cs e

  -- | Undirected reduction of an image.
  fold :: (Pixel cs e -> Pixel cs e -> Pixel cs e) -- ^ An associative folding function.
       -> Pixel cs e -- ^ Initial element, that is neutral with respect to the folding function.
       -> Image arr cs e -- ^ Source image.
       -> Pixel cs e

  -- | Pixelwise equality function of two images. Images are
  -- considered distinct if either images' dimensions or at least one pair of
  -- corresponding pixels are not the same. Used in defining an in instance for
  -- the 'Eq' typeclass.
  eq :: Eq (Pixel cs e) => Image arr cs e -> Image arr cs e -> Bool


-- | Array representation that allows computation, which depends on some specific
-- order, consequently making it possible to be computed only sequentially.
class ManifestArray arr cs e => SequentialArray arr cs e where

  -- | Fold an image from the left in a row major order.
  foldl :: (a -> Pixel cs e -> a) -> a -> Image arr cs e -> a

  -- | Fold an image from the right in a row major order.
  foldr :: (Pixel cs e -> a -> a) -> a -> Image arr cs e -> a

  -- | Monading mapping over an image.
  mapM :: (SequentialArray arr cs' e', Functor m, Monad m) =>
          (Pixel cs' e' -> m (Pixel cs e)) -> Image arr cs' e' -> m (Image arr cs e)

  -- | Monading mapping over an image. Result is discarded.
  mapM_ :: (Functor m, Monad m) => (Pixel cs e -> m b) -> Image arr cs e -> m ()

  foldM :: (Functor m, Monad m) => (a -> Pixel cs e -> m a) -> a -> Image arr cs e -> m a

  foldM_ :: (Functor m, Monad m) => (a -> Pixel cs e -> m a) -> a -> Image arr cs e -> m ()


-- | Array representation that supports mutation.
class ManifestArray arr cs e => MutableArray arr cs e where
  data MImage st arr cs e

  -- | Get dimensions of a mutable image.
  mdims :: MImage st arr cs e -> (Int, Int)

  -- | Yield a mutable copy of an image.
  thaw :: (Functor m, PrimMonad m) => Image arr cs e -> m (MImage (PrimState m) arr cs e)

  -- | Yield an immutable copy of an image.
  freeze :: (Functor m, PrimMonad m) => MImage (PrimState m) arr cs e -> m (Image arr cs e)

  -- | Create a mutable image with given dimensions. Pixels are uninitialized.
  new :: (Functor m, PrimMonad m) => (Int, Int) -> m (MImage (PrimState m) arr cs e)

  -- | Yield the pixel at a given location.
  read :: (Functor m, PrimMonad m) => MImage (PrimState m) arr cs e -> (Int, Int) -> m (Pixel cs e)

  -- | Set a pixel at a given location.
  write :: (Functor m, PrimMonad m) =>
           MImage (PrimState m) arr cs e -> (Int, Int) -> Pixel cs e -> m ()

  -- | Swap pixels at given locations.
  swap :: (Functor m, PrimMonad m) =>
          MImage (PrimState m) arr cs e -> (Int, Int) -> (Int, Int) -> m ()


-- | Allows for changing an underlying image representation.
class Exchangable arr' arr where

  -- | Exchange the underlying array representation of an image.
  exchange :: (Array arr' cs e, Array arr cs e) =>
              arr -- ^ New representation of an image.
           -> Image arr' cs e -- ^ Source image.
           -> Image arr cs e


-- | Changing to the same array representation as before is disabled and `changeTo`
-- will behave simply as an identitity function.
instance Exchangable arr arr where

  exchange _ = id
  {-# INLINE exchange #-}



-- | Approach to be used near the border during transformations, which, besides a pixel
-- of interest, also use it's neighbors, consequently going out of bounds at the
-- edges of an image.
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

-- | Border handling function. If @(i, j)@ location is within bounds, then supplied
-- lookup function will be used, otherwise it will be handled according to a
-- supplied border strategy.
borderIndex :: Border (Pixel cs e) -- ^ Border handling strategy.
            -> (Int, Int) -- ^ Image dimensions
            -> ((Int, Int) -> Pixel cs e) -- ^ Image's indexing function.
            -> (Int, Int) -- ^ @(i, j)@ location of a pixel lookup.
            -> Pixel cs e
borderIndex border (m, n) getPx (i, j) =  --removed !(m,n) !getPx !(i,j)
  if i >= 0 && j >= 0 && i < m && j < n then getPx (i, j) else getPxB border where
    getPxB (Fill px) = px
    getPxB Wrap      = getPx (i `mod` m, j `mod` n)
    getPxB Edge      = getPx (if i < 0 then 0 else if i >= m then m - 1 else i,
                              if j < 0 then 0 else if j >= n then n - 1 else j)
    getPxB Reflect   = getPx (if i < 0 then (abs i - 1) `mod` m else
                                if i >= m then (m - (i - m + 1)) `mod` m else i,
                              if j < 0 then (abs j - 1) `mod` n else
                                if j >= n then (n - (j - n + 1)) `mod` n else j)
    getPxB Continue  = getPx (if i < 0 then abs i `mod` m else
                                if i >= m then m - (i - m + 2) `mod` m else i,
                              if j < 0 then abs j `mod` n else
                                if j >= n then n - (j - n + 2) `mod` n else j)
    {-# INLINE getPxB #-}
{-# INLINE borderIndex #-}


-- | Image indexing function that returns a default pixel if index is out of bounds.
defaultIndex :: ManifestArray arr cs e =>
                Pixel cs e -> Image arr cs e -> (Int, Int) -> Pixel cs e
defaultIndex px img = borderIndex (Fill px) (dims img) (index img) -- removed !px !img
{-# INLINE defaultIndex #-}


-- | Image indexing function that returns 'Nothing' if index is out of bounds,
-- 'Just' pixel otherwise.
maybeIndex :: ManifestArray arr cs e =>
              Image arr cs e -> (Int, Int) -> Maybe (Pixel cs e)
maybeIndex img@(dims -> (m, n)) (i, j) =
  if i >= 0 && j >= 0 && i < m && j < n then Just $ index img (i, j) else Nothing
{-# INLINE maybeIndex #-}



instance ColorSpace cs => Functor (Pixel cs) where

  fmap = pxOp
  {-# INLINE fmap #-}
  
instance ColorSpace cs => Applicative (Pixel cs) where

  pure = fromChannel
  {-# INLINE pure #-}

  (<*>) = chApp
  {-# INLINE (<*>) #-}


instance ColorSpace cs => Foldable (Pixel cs) where

  foldMap = pxFoldMap
  {-# INLINE foldMap #-}


instance (ColorSpace cs, Num e) => Num (Pixel cs e) where
  (+)         = liftA2 (+)
  {-# INLINE (+) #-}
  
  (-)         = liftA2 (-)
  {-# INLINE (-) #-}
  
  (*)         = liftA2 (*)
  {-# INLINE (*) #-}
  
  abs         = liftA abs
  {-# INLINE abs #-}
  
  signum      = liftA signum
  {-# INLINE signum #-}
  
  fromInteger = pure . fromInteger
  -- {-# INLINE fromInteger#-}
  

instance (ColorSpace cs, Fractional e) => Fractional (Pixel cs e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  
  recip        = liftA recip
  {-# INLINE recip #-}

  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance (ColorSpace cs, Floating e) => Floating (Pixel cs e) where
  pi      = fromChannel pi
  {-# INLINE pi #-}

  exp     = liftA exp
  {-# INLINE exp #-}

  log     = liftA log
  {-# INLINE log #-}
  
  sin     = liftA sin
  {-# INLINE sin #-}
  
  cos     = liftA cos
  {-# INLINE cos #-}
  
  asin    = liftA asin
  {-# INLINE asin #-}
  
  atan    = liftA atan
  {-# INLINE atan #-}
  
  acos    = liftA acos
  {-# INLINE acos #-}
  
  sinh    = liftA sinh
  {-# INLINE sinh #-}
  
  cosh    = liftA cosh
  {-# INLINE cosh #-}
  
  asinh   = liftA asinh
  {-# INLINE asinh #-}
  
  atanh   = liftA atanh
  {-# INLINE atanh #-}
  
  acosh   = liftA acosh
  {-# INLINE acosh #-}


instance (ManifestArray arr cs e, Eq (Pixel cs e)) => Eq (Image arr cs e) where
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
  
  fromInteger = GAUSSIAN.singleton . fromInteger
  --  {-# INLINE fromInteger#-}


instance (Fractional (Pixel cs e), Fractional e, Array arr cs e) =>
         Fractional (Image arr cs e) where
  (/)          = GAUSSIAN.zipWith (/)
  {-# INLINE (/) #-}
  
  fromRational = GAUSSIAN.singleton . fromRational 
  {-# INLINE fromRational #-}


instance (Floating (Pixel cs e), Floating e, Array arr cs e) =>
         Floating (Image arr cs e) where
  pi    = GAUSSIAN.singleton pi
  {-# INLINE pi #-}
  
  exp   = GAUSSIAN.map exp
  {-# INLINE exp #-}
  
  log   = GAUSSIAN.map log
 -- {-# INLINE log#-}
  
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


instance ManifestArray arr cs e => NFData (Image arr cs e) where
  rnf img = img `deepSeqImage` ()
  {-# INLINE rnf #-}



instance Array arr cs e => Show (Image arr cs e) where
  show ((dims -> (m, n)) :: Image arr cs e) =
    "<Image "Prelude.++show (Prelude.undefined :: arr)Prelude.++" "Prelude.++
    ((showsTypeRep (typeOf (Prelude.undefined :: cs))) " (") Prelude.++
    ((showsTypeRep (typeOf (Prelude.undefined :: e))) "): " Prelude.++ show m Prelude.++"x" Prelude.++ show n Prelude.++ ">")


instance MutableArray arr cs e => Show (MImage st arr cs e) where
  show ((mdims -> (m, n)) :: MImage st arr cs e) =
    "<MutableImage "Prelude.++show (Prelude.undefined :: arr)Prelude.++" "Prelude.++
    ((showsTypeRep (typeOf (Prelude.undefined :: cs))) " (")Prelude.++
    ((showsTypeRep (typeOf (Prelude.undefined :: e))) "): "Prelude.++show m Prelude.++"x"Prelude.++show n Prelude.++">")
{-# LANGUAGE ViewPatterns, CPP #-} 

---https://hackage.haskell.org/package/hip-1.0.1/candidate/docs/src/Graphics-Image-Interface.html#ColorSpace

-- http://hackage.haskell.org/package/hip-1.5.3.0/docs/src/Graphics-Image-Interface.html#promote

-- | A Pixel family with a color space and a precision of elements.
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


--http://hackage.haskell.org/package/hip-1.5.3.0/docs/src/Graphics-Image-Interface.html#Border
  
  

data VGImage v p =
  VGImage {-# UNPACK #-}!Int
          {-# UNPACK #-}!Int

makeImageVG ::  {- VG.Vector v p => -} (Int, Int) -> ((Int, Int) -> p) -> VGImage v p
makeImageVG sz f =
  let (m, n) = checkDimsVG "makeImageVGM" sz in
    VGImage m n $ VG.generate (m * n) (f . toIx n)
{-# INLINE makeImageVG #-}



 -- http://hackage.haskell.org/package/hip-1.5.3.0/docs/src/Graphics-Image-Processing-Filter.html

data Filter arr cs e = Filter{ applyFilter :: Image arr cs e -> Image arr cs e }

-- | Create a Gaussian Filter.
--
-- @since 1.5.3
gaussianLowPass :: (Floating e, RealFrac e) =>
                   Int -- ^ Radius
                -> e -- ^ Sigma
                -> Border (Pixel cs e) -- ^ Border resolution technique.
                -> Filter arr cs e
gaussianLowPass r sigma border =
  Filter (correlate border gV' . correlate border gV)
  where
    gV = compute $ (gauss / scalar weight)
    gV' = compute $ GAUSSIAN.transpose gV
    gauss = makeImage (1, n) getPx
    weight = ifold (+) 0 gauss
    n = 2 * r + 1
    sigma2sq = 2 * sigma ^ (2 :: Int)
    getPx (_, j) = promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)
    {-# INLINE getPx #-}
{-# INLINE gaussianLowPass #-}




-- | Create a Gaussian Blur filter. Radius will be derived from standard
-- deviation: @ceiling (2*sigma)@ and `Edge` border resolution will be
-- utilized. If custom radius and/or border resolution is desired,
-- `gaussianLowPass` can be used instead.
--
-- @since 1.5.3
gaussianBlur :: (Floating e, RealFrac e) => e -> Filter arr cs e
gaussianBlur sigma = gaussianLowPass (ceiling (2*sigma)) sigma Edge

