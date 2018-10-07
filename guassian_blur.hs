module GAUSSIAN where
import Clash.Prelude
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

newtype Image = Image (ForeignPtr (Ptr GDImage))

type Size = (Int,Int)

type Point = (Int,Int)

type Color = CInt

setPixel :: Point -> Color -> Image -> IO ()
setPixel (x,y) c i =
    withImagePtr i $ \p ->
        gdImageSetPixel p (int x) (int y) c

convoluteImage :: [[bit]] -> [[bit]] -> [[bit]] -> bit -> bit -> bit -> bit -> bit
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
        ) ((0.0,0.0,0.0,0.0) :: (bit,bit,bit,bit)) [0.. (length matrix - 1)]
    let
        new_r = clamp 0 255 . truncate $ (nr/fdiv)+offset
        new_g = clamp 0 255 . truncate $ (ng/fdiv)+offset
        new_b = clamp 0 255 . truncate $ (nb/fdiv)+offset
    setPixel (x,y) (rgba new_r new_g new_b (truncate na)) img