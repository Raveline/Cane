-- Utilities for surface manipulation
module Cane.Image where

import Data.List
import Foreign
import Control.Monad
import Graphics.UI.SDL as SDL hiding (Color)

import Glove.Types

colorToRGB :: Color -> (Word8, Word8, Word8)
colorToRGB (Color r g b _) = (fromIntegral r, fromIntegral g, fromIntegral b) 

toPixel :: Color -> Pixel
toPixel (Color r g b _) = Pixel $ foldl' accum 0 [r,g,b]
    where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

-- |Read the pixel at (x,y) on s.
-- If it is white, replace it on the surface s'
-- by the color of pixel p. If not, do nothing.
pixToR :: Surface -> Surface -> Pixel -> (Int, Int) -> IO ()
pixToR s s' p (x,y) = do pix <- getPixel s x y
                         when (isWhite pix) $ setPixel s' x y p

isWhite :: Pixel -> Bool
isWhite (Pixel 0x00ffffff) = True
isWhite _ = False

getPixel :: Surface -> Int -> Int -> IO Pixel
getPixel s x y = do pixels <- castPtr `liftM` surfaceGetPixels s
                    Pixel `liftM` peekElemOff pixels ((y * surfaceGetWidth s) + x) 

setPixel :: Surface -> Int -> Int -> Pixel -> IO () 
setPixel s x y (Pixel p) = 
    do pixels <- castPtr `liftM` surfaceGetPixels s
       pokeElemOff pixels ((y * surfaceGetWidth s) + x) p

prepareSurface' :: Width -> Height -> IO Surface
prepareSurface' w h = prepareSurface w h 0x00 0x00 0x00 0x00
                      >>= transparencyOn 0x00 0x00 0x00 

prepareSurface :: Width -> Height -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
prepareSurface w h r g b a = 
    createRGBSurface [SDL.SWSurface] w h 32 r g b a
    >>= displayFormat

transparencyOn :: Word8 -> Word8 -> Word8 -> Surface -> IO Surface
transparencyOn r g b s
    = (mapRGB . surfaceGetPixelFormat) s r g b
      >>= setColorKey s [SrcColorKey]
      >> return s
