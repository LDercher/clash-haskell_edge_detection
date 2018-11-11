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
--import           Graphics.Image.Interface              as I
--import           Graphics.Image.Processing.Convolution
--import           Graphics.Image.ColorSpace               (X)

data X = X deriving (Eq, Enum, Bounded, Show, Typeable)


newtype instance Pixel X e = PixelX { getX :: e } deriving (Ord, Eq)


instance Show e => Show (Pixel X e) where
  show (PixelX g) = "<X:("++show g++")>"


instance Elevator e => ColorSpace X e where
  type Components X e = e

  promote = PixelX
  {-# INLINE promote #-}
  fromComponents = PixelX
  {-# INLINE fromComponents #-}
  toComponents (PixelX g) = g
  {-# INLINE toComponents #-}
  getPxC (PixelX g) X = g
  {-# INLINE getPxC #-}
  setPxC (PixelX _) X g = PixelX g
  {-# INLINE setPxC #-}
  mapPxC f (PixelX g) = PixelX (f X g)
  {-# INLINE mapPxC #-}
  liftPx = fmap
  {-# INLINE liftPx #-}
  liftPx2 = liftA2
  {-# INLINE liftPx2 #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}
  foldlPx2 f !z (PixelX g1) (PixelX g2) = f z g1 g2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel X) where
  fmap f (PixelX g) = PixelX (f g)
  {-# INLINE fmap #-}


instance Applicative (Pixel X) where
  pure = PixelX
  {-# INLINE pure #-}
  (PixelX fg) <*> (PixelX g) = PixelX (fg g)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel X) where
  foldr f !z (PixelX g) = f g z
  {-# INLINE foldr #-}


instance Monad (Pixel X) where

  return = PixelX
  {-# INLINE return #-}

  (>>=) (PixelX g) f = f g
  {-# INLINE (>>=) #-}


instance Storable e => Storable (Pixel X e) where

  sizeOf _ = sizeOf (undefined :: e)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: e)
  {-# INLINE alignment #-}
  peek !p = do
    q <- return $ castPtr p
    g <- peek q
    return (PixelX g)
  {-# INLINE peek #-}
  poke !p (PixelX g) = do
    q <- return $ castPtr p
    poke q g
  {-# INLINE poke #-}


-- | Separate a Pixel into a list of components with 'X' pixels containing every
-- component from the pixel.
--
-- >>> toPixelsX (PixelRGB 4 5 6)
-- [<X:(4)>,<X:(5)>,<X:(6)>]
--
toPixelsX :: ColorSpace cs e => Pixel cs e -> [Pixel X e]
toPixelsX = foldrPx ((:) . PixelX) []

-- | Combine a list of `X` pixels into a Pixel with a specified channel
-- order. Not the most efficient way to construct a pixel, but might prove
-- useful to someone.
--
-- >>> fromPixelsX [(RedRGB, 3), (BlueRGB, 5), (GreenRGB, 4)]
-- <RGB:(3.0|4.0|5.0)>
-- >>> fromPixelsX $ zip (enumFrom RedRGB) (toPixelsX $ PixelRGB 4 5 6)
-- <RGB:(4.0|5.0|6.0)>
--
fromPixelsX :: ColorSpace cs e => [(cs, Pixel X e)] -> Pixel cs e
fromPixelsX = foldl' f (promote 0) where
  f !px (c, PixelX x) = setPxC px c x



-- | Apply a left fold to each of the pixels in the image.
squashWith :: (Array arr cs e, Array arr X b) =>
              (b -> e -> b) -> b -> Image arr cs e -> Image arr X b
squashWith f !a = I.map (PixelX . foldlPx f a) where
{-# INLINE squashWith #-}


-- | Combination of zipWith and simultanious left fold on two pixels at the same time.
squashWith2 :: (Array arr cs e, Array arr X b) =>
               (b -> e -> e -> b) -> b -> Image arr cs e -> Image arr cs e -> Image arr X b
squashWith2 f !a = I.zipWith (PixelX .:! foldlPx2 f a) where
{-# INLINE squashWith2 #-}


-- | Separate an image into a list of images with 'X' pixels containing every
-- channel from the source image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> let [frog_red, frog_green, frog_blue] = toImagesX frog
-- >>> writeImage "images/frog_red.png" $ toImageY frog_red
-- >>> writeImage "images/frog_green.jpg" $ toImageY frog_green
-- >>> writeImage "images/frog_blue.jpg" $ toImageY frog_blue
--
-- <<images/frog_red.jpg>> <<images/frog_green.jpg>> <<images/frog_blue.jpg>>
--
toImagesX :: (Array arr cs e, Array arr X e) => Image arr cs e -> [Image arr X e]
toImagesX !img = P.map getCh (enumFrom minBound) where
  getCh !ch = I.map (PixelX . (`getPxC` ch)) img
  {-# INLINE getCh #-}
{-# INLINE toImagesX #-}


-- | Combine a list of images with 'X' pixels into an image of any color
-- space, by supplying an order of color space channels.
--
-- For example here is a frog with swapped 'BlueRGB' and 'GreenRGB' channels.
--
-- >>> writeImage "images/frog_rbg.jpg" $ fromImagesX [(RedRGB, frog_red), (BlueRGB, frog_green), (GreenRGB, frog_blue)]
--
-- <<images/frog.jpg>> <<images/frog_rbg.jpg>>
--
-- It is worth noting though, despite that separating image channels can be
-- sometimes pretty useful, exactly the same effect as in example above can be
-- achieved in a much simpler and a more efficient way:
--
-- @ `I.map` (\\(PixelRGB r g b) -> PixelRGB r b g) frog @
--
fromImagesX :: (Array arr X e, Array arr cs e) =>
               [(cs, Image arr X e)] -> Image arr cs e
fromImagesX = fromXs 0 where
  updateCh !ch !px (PixelX e) = setPxC px ch e
  {-# INLINE updateCh #-}
  fromXs img []          = img
  fromXs img ((c, i):xs) = fromXs (I.zipWith (updateCh c) img i) xs
  {-# INLINE fromXs #-}
{-# INLINE fromImagesX #-}

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
index !img !ix = borderIndex (error $ show img ++ " - Index out of bounds: " ++ show ix) img ix
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
    show err ++ ": dimensions are expected to be positive: " ++ show sz
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
  (+)         = zipWith (+)
  {-# INLINE (+) #-}
  (-)         = zipWith (-)
  {-# INLINE (-) #-}
  (*)         = zipWith (*)
  {-# INLINE (*) #-}
  abs         = map abs
  {-# INLINE abs #-}
  signum      = map signum
  {-# INLINE signum #-}
  fromInteger = scalar . fromInteger
  {-# INLINE fromInteger #-}

instance (Fractional (Pixel cs e), Array arr cs e) =>
         Fractional (Image arr cs e) where
  (/)          = zipWith (/)
  {-# INLINE (/) #-}
  fromRational = scalar . fromRational
  {-# INLINE fromRational #-}


instance (Floating (Pixel cs e), Array arr cs e) =>
         Floating (Image arr cs e) where
  pi    = scalar pi
  {-# INLINE pi #-}
  exp   = map exp
  {-# INLINE exp #-}
  log   = map log
  {-# INLINE log #-}
  sin   = map sin
  {-# INLINE sin #-}
  cos   = map cos
  {-# INLINE cos #-}
  asin  = map asin
  {-# INLINE asin #-}
  atan  = map atan
  {-# INLINE atan #-}
  acos  = map acos
  {-# INLINE acos #-}
  sinh  = map sinh
  {-# INLINE sinh #-}
  cosh  = map cosh
  {-# INLINE cosh #-}
  asinh = map asinh
  {-# INLINE asinh #-}
  atanh = map atanh
  {-# INLINE atanh #-}
  acosh = map acosh
  {-# INLINE acosh #-}


instance MArray arr cs e => NFData (Image arr cs e) where
  rnf img = img `deepSeqImage` ()
  {-# INLINE rnf #-}


instance BaseArray arr cs e =>
         Show (Image arr cs e) where
  show (dims -> (m, n)) =
    "<Image " ++
    showsTypeRep (typeRep (Proxy :: Proxy arr)) " " ++
    showsTypeRep (typeRep (Proxy :: Proxy cs)) " (" ++
    showsTypeRep (typeRep (Proxy :: Proxy e)) "): " ++
     show m ++ "x" ++ show n ++ ">"


instance MArray arr cs e =>
         Show (MImage st arr cs e) where
  show (mdims -> (m, n)) =
    "<MutableImage " ++
    showsTypeRep (typeRep (Proxy :: Proxy arr)) " " ++
    showsTypeRep (typeRep (Proxy :: Proxy cs)) " (" ++
    showsTypeRep (typeRep (Proxy :: Proxy e)) "): " ++
     show m ++ "x" ++ show n ++ ">"


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
    !gV' = compute $ transpose gV
    !gauss = makeImage (1, n) getPx
    !weight = I.fold (+) 0 gauss
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


