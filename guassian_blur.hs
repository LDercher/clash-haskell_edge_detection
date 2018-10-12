--- synopsis: implementation of guassian blur function from Haskell Graphics.GD Haskell module in clash (Haskell->VHDL)
--- author: Luke Dercher
--- citation: Haskell Graphics.GD library 


module GAUSSIAN where
import Clash.Prelude
import qualified Foreign                  as F
import Control.Monad (mapM_,foldM)
import           Control.Exception        (bracket)
import           Control.Monad            (liftM, unless)
import           Data.Bits
import qualified Data.ByteString.Internal as B
import           Foreign                  (Ptr,FunPtr,ForeignPtr)
import           Foreign                  (peekByteOff)
import qualified Foreign                  as F
import           Foreign.C                (CString)
import qualified Foreign.C                as C
import           Foreign.C.Types

data CFILE

foreign import ccall "stdio.h fopen" c_fopen
        :: CString -> CString -> IO (Ptr CFILE)
foreign import ccall "stdio.h fclose" c_fclose
        :: Ptr CFILE -> IO CInt
    

data GDImage

    -- JPEG format
    
foreign import ccall "gd.h gdImageCreateFromJpeg" gdImageCreateFromJpeg
        :: Ptr CFILE -> IO (Ptr GDImage)
    
foreign import ccall "gd.h gdImageCreateFromJpegPtr" gdImageCreateFromJpegPtr
        :: CInt -> Ptr a -> IO (Ptr GDImage)
    
foreign import ccall "gd.h gdImageJpeg" gdImageJpeg
        :: Ptr GDImage -> Ptr CFILE -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageJpegPtr" gdImageJpegPtr
        :: Ptr GDImage -> Ptr CInt -> CInt -> IO (Ptr a)
    
    -- PNG format
    
foreign import ccall "gd.h gdImageCreateFromPng" gdImageCreateFromPng
        :: Ptr CFILE -> IO (Ptr GDImage)
    
foreign import ccall "gd.h gdImageCreateFromPngPtr" gdImageCreateFromPngPtr
        :: CInt -> Ptr a -> IO (Ptr GDImage)
    
foreign import ccall "gd.h gdImagePng" gdImagePng
        :: Ptr GDImage -> Ptr CFILE -> IO ()
    
foreign import ccall "gd.h gdImagePngPtr" gdImagePngPtr
        :: Ptr GDImage -> Ptr CInt -> IO (Ptr a)
    
    -- GIF format
    
foreign import ccall "gd.h gdImageCreateFromGif" gdImageCreateFromGif
        :: Ptr CFILE -> IO (Ptr GDImage)
    
foreign import ccall "gd.h gdImageCreateFromGifPtr" gdImageCreateFromGifPtr
        :: CInt -> Ptr a -> IO (Ptr GDImage)
    
foreign import ccall "gd.h gdImageGif" gdImageGif
        :: Ptr GDImage -> Ptr CFILE -> IO ()
    
foreign import ccall "gd.h gdImageGifPtr" gdImageGifPtr
        :: Ptr GDImage -> Ptr CInt -> IO (Ptr a)
    
    -- Creating and destroying images
    
foreign import ccall "gd.h gdImageCreateTrueColor" gdImageCreateTrueColor 
        :: CInt -> CInt -> IO (Ptr GDImage)
    
foreign import ccall "gd.h gdImageDestroy" gdImageDestroy
        :: Ptr GDImage -> IO ()
    
foreign import ccall "gd-extras.h &gdImagePtrDestroyIfNotNull"
        ptr_gdImagePtrDestroyIfNotNull 
        :: FunPtr (Ptr (Ptr GDImage) -> IO ())
    
    
    -- Copying image parts
    
foreign import ccall "gd.h gdImageCopy" gdImageCopy
        :: Ptr GDImage -> Ptr GDImage 
        -> CInt -> CInt -> CInt -> CInt
        -> CInt -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageCopyResampled" gdImageCopyResampled
        :: Ptr GDImage -> Ptr GDImage 
        -> CInt -> CInt -> CInt -> CInt
        -> CInt -> CInt -> CInt -> CInt -> IO ()
    
foreign import ccall "gd-extras.h gdImageCopyRotated90" gdImageCopyRotated90
        :: Ptr GDImage -> Ptr GDImage 
        -> CInt -> CInt -> CInt -> CInt
        -> CInt -> CInt -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageGetPixel" gdImageGetPixel
        :: Ptr GDImage -> CInt -> CInt -> IO CInt
    
    -- Drawing functions
    
foreign import ccall "gd.h gdImageSetBrush" gdImageSetBrush
        :: Ptr GDImage -> Ptr GDImage -> IO ()
    
foreign import ccall "gd.h gdImageFilledRectangle" gdImageFilledRectangle
        :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageFilledEllipse" gdImageFilledEllipse
        :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageLine" gdImageLine
        :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageArc" gdImageArc
        :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt
        -> CInt -> CInt -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageSetAntiAliased" gdImageSetAntiAliased
        :: Ptr GDImage -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageSetPixel" gdImageSetPixel
        :: Ptr GDImage -> CInt -> CInt -> CInt -> IO ()
    
foreign import ccall "gd.h gdImageColorAllocate" gdImageColorAllocate
        :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> IO CInt
    
    -- Text functions
    
foreign import ccall "gd.h gdFTUseFontConfig" gdFTUseFontConfig
        :: CInt -> IO CInt
    
foreign import ccall "gd.h gdImageStringFT" gdImageStringFT
        :: Ptr GDImage -> Ptr CInt -> CInt -> CString -> CDouble -> CDouble ->
           CInt -> CInt -> CString -> IO CString
    
foreign import ccall "gd.h gdImageStringFTCircle" gdImageStringFTCircle
        :: Ptr GDImage -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CString
           -> CDouble -> CString -> CString -> CInt -> IO CString

--convolute ::    Image 
--                -> [[Float]]    -- ^ Convolution matrix
--                -> Float        -- ^ Divisor
--                -> Float        -- ^ Offset
--                -> IO ()
--convolute img matrix fdiv offset = do
--    (width,height) <- imageSize img
--    imgCpy <- copyImage img
--    mapM_ (\y -> 
--        mapM_ (\x -> convoluteImage img imgCpy matrix fdiv offset x y) [0..(width-1)]
--        ) [0..(height-1)] 
--    return () --}--

type RGBA = (Signed 8,Signed 8,Signed 8,Signed 8)

-- replace Signed 8s with unsigned 8 or whatever it needs to be
-- 


type Size = (Signed 8,Signed 8)

type Point = (Signed 8,Signed 8)

type Color = Signed 8




toRGBA :: Color -> (Int, Int, Int, Int) 
toRGBA c = (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
 where
   b = c `mod` byte
   g = shiftR c 8 `mod` byte
   r = shiftR c 16 `mod` byte
   a = shiftR c 24 `mod` byte
   byte = 2 ^ (8::Int)

   -- We use a second level of indirection to allow storing a null pointer
-- when the image has already been freed. This allows 'withImage' to 
-- free the @gdImage@ early.
newtype Image = Vec (Vec 256 (Signed 256))--(Vec 256 (Signed 8))-- Image (ForeignPtr (Ptr GDImage))
   
-- | Retrieves the color index or the color values of a particular pixel.
getPixel :: (Int,Int) -> Image -> IO Color
getPixel (x,y) i = withImagePtr i f
    where f p' = gdImageGetPixel p' (int x) (int y)

rgba :: Signed 8 -- ^ Red (0-255)
          -> Signed 8 -- ^ Green (0-255)
          -> Signed 8 -- ^ Blue (0-255)
          -> Signed 8 -- ^ Alpha (0-127), 0 is opaque, 127 is transparent
          -> Color
rgba r g b a = 
    (int a `F.shiftL` 24) .|.
    (int r `F.shiftL` 16) .|.
    (int g `F.shiftL` 8)  .|.
     int b

-- | Utility function for clamping a value between a minimum and maximum value
clamp :: (Ord a, Num a) =>  a       -- ^ Minimum
                            -> a    -- ^ Maximum
                            -> a    -- ^ Value to clamp
                            -> a
clamp minm maxm num 
	| num < minm = minm
	| num > maxm = maxm
	| otherwise = num

setPixel :: Point -> Color -> Vec n3 (Vec n2 (Signed 8)) -> IO ()
setPixel (x,y) c i =
    withImagePtr i $ \p ->
        gdImageSetPixel p (Signed 8 x) (Signed 8 y) c

convoluteImage :: Vec n3 (Vec n2 (Signed 8)) ->Vec n3 (Vec n2 (Signed 8)) -> Vec n3 (Vec n2 (Signed 8)) -> Signed 8 -> Signed 8 -> Signed 8 -> Signed 8 -> Signed 8
convoluteImage img imgCpy matrix fdiv offset x y = do
    (nr,ng,nb,na) <- foldM (\(or,og,ob,oa) j -> do
        let yy = min (max (y-(1+j)) 0) (max (y-1) 0)
        (pr,pg,pb,pa) <- foldM (\(ar,ag,ab,aa) k -> do
                        let xx = min (max (x-(1+k)) 0) (max (x-1) 0)
                        curr <- getPixel (xx,yy) imgCpy
                        let (r,g,b,a) = toRGBA curr
                        return (ar + fromIntegral r * ((matrix!!j)!!k)
                            ,ag + fromIntegral g * ((matrix!!j)!!k)
                            ,ab + fromIntegral b * ((matrix!!j)!!k)
                            ,fromIntegral a)
                        ) (or,og,ob,oa) [0.. 255] -- length (matrix!!j) - 1)] make matrix dimensions static size 255 for now
        return (pr,pg,pb,pa)
        ) ((0.0,0.0,0.0,0.0) :: (Signed 8,Signed 8,Signed 8,Signed 8)) [0.. 255]
    let
        new_r = clamp 0 255 . truncate $ (nr/fdiv)+offset
        new_g = clamp 0 255 . truncate $ (ng/fdiv)+offset
        new_b = clamp 0 255 . truncate $ (nb/fdiv)+offset
    setPixel (x,y) (rgba new_r new_g new_b (truncate na)) img