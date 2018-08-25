-- from tutorial https://www.stackbuilders.com/tutorials/haskell/image-processing/

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R -- for Repa

data ImgFormat = Bmp | Jpg | Png | Tiff

main :: IO ()
main = do
  [ext, path] <- getArgs
  case fromExt ext of
    Nothing -> putStrLn "Sorry, I don't know such format!"
    Just fmt -> convertImg fmt path

convertImg
  :: ImgFormat         -- ^ Format of resulting image
  -> FilePath          -- ^ Where to get source image
  -> IO ()
convertImg fmt path = do
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right img ->
      (case fmt of -- select saving function
        Bmp  -> saveBmpImage
        Jpg  -> saveJpgImage 100
        Png  -> savePngImage
        Tiff -> saveTiffImage)
      (replaceExtension path (toExt fmt)) -- replace file extension
      img -- pass it 'DynamicImage' we've read

-- | Get file extension corresponding to known image format.
toExt :: ImgFormat -> String
toExt Bmp      = "bmp"
toExt Jpg      = "jpeg"
toExt Png      = "png"
toExt Tiff     = "tiff"

-- | Get image format corresponding to given extension or 'Nothing' if we
-- don't support that format.
fromExt :: String -> Maybe ImgFormat
fromExt "bmp"  = Just Bmp
fromExt "jpeg" = Just Jpg
fromExt "png"  = Just Png
fromExt "tiff" = Just Tiff
fromExt _      = Nothing