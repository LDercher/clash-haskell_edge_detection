module GAUSSIAN where
import Clash.Prelude
import qualified Foreign                  as F
import Control.Monad (mapM_,foldM)

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

type RGBA = (Bit,Bit,Bit,Bit)

--newtype Image = Image (ForeignPtr (Ptr GDImage))

-- replace Signed 8s with unsigned 8 or whatever it needs to be
-- 


type Size = (Signed 8,Signed 8)

type Point = (Bit,Bit)

type Color = Signed 8

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

setPixel :: Point -> Color -> [[Bit]] -> IO ()
setPixel (x,y) c i =
    withImagePtr i $ \p ->
        gdImageSetPixel p (Signed 8 x) (Signed 8 y) c

convoluteImage :: [[Bit]] -> [[Bit]] -> [[Bit]] -> Bit -> Bit -> Bit -> Bit -> Bit
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
                        ) (or,og,ob,oa) [0.. (length (matrix!!j) - 1)]
        return (pr,pg,pb,pa)
        ) ((0.0,0.0,0.0,0.0) :: (Bit,Bit,Bit,Bit)) [0.. (length matrix - 1)]
    let
        new_r = clamp 0 255 . truncate $ (nr/fdiv)+offset
        new_g = clamp 0 255 . truncate $ (ng/fdiv)+offset
        new_b = clamp 0 255 . truncate $ (nb/fdiv)+offset
    setPixel (x,y) (rgba new_r new_g new_b (truncate na)) img