module Cane.Screen
( raiseCane
, test)
where

import Prelude hiding (init)
import Data.Char
import Control.Monad
import Graphics.UI.SDL as SDL hiding (Color)
import Graphics.UI.SDL.Image
import qualified Data.Vector as V

import Glove.Types
import Cane.Image
import Cane.Types

test :: IO ()
test = let f = Font "terminal.png" True 8 8
           s = Screen False "Yolo" 80 60
       in raiseCane f s

-- |Temporary loop function. To refactor.
loop :: Cane -> IO ()
loop c = do escape <- parseEvents
            let scr = _screen c
            displayTile c (Tile 'A' white black True) (0,0)
            displayTile c (Tile (chr 66) blue white True) (1,0)
            displayTile c (Tile (chr 1) red green True) (0,1)
            SDL.flip scr
            unless escape $ loop c

-- |Given the context of our Cane, a tile and a position,
-- display it on the screen if this tile need to be rendered.
displayTile :: Cane -> Tile -> Position -> IO ()
displayTile _ (Tile _ _ _ False) _ = return ()
displayTile (Cane s g fw fh) (Tile c b f _) (x,y) = 
    do let char = g V.! ord c
       let xTile = x*fw
       let yTile = y*fw
       colorizeBack s b xTile yTile fw fh
       colorized <- colorizeFront char fw fh f
       void $ blitOn xTile yTile colorized s
       freeSurface colorized -- We don't need the colorized version anymore

 
-- |Draw a rectangle of the color bc on the background of the screen s
-- at the given position x,y.
colorizeBack :: Surface -> BackColor -> Int -> Int -> Width -> Height -> IO ()
colorizeBack s bc x y w h = color >>= fillRect s rect >> return ()
    where rect = Just Rect { rectX = x, rectY = y, rectW = w, rectH = h }
          color = let (r,g,b) = colorToRGB bc in
                  (mapRGB . surfaceGetPixelFormat) s r g b

-- |Make a surface (newS). Take every white pixel from the surface s,
-- and put them, colorized with the ForeColor f, on our new surface.
colorizeFront :: Surface -> Width -> Height -> ForeColor -> IO Surface
colorizeFront s w h f 
    = do newS <- prepareSurface' w h
         forM_ [(x,y)| x <- [0..w-1], y <- [0..h-1]] (pixToR s newS $ colToPix f)
         return newS
         -- This is a rather ugly hack. Since the background of our font image 
         -- is black, we set the transparency key on black.
         -- So this prevent displaying black characters. Hence this little ruse,
         -- that creates a "quasi-black". This should be modified so that the
         -- back-color is customizable, and this hack can be applied to any
         -- transparent color.
         where colToPix (Color 0 0 0 0) = toPixel (Color 0 0 1 0)
               colToPix f' = toPixel f'

-- |Short cut to the SDL.blitSurface method.
blitOn :: Int -> Int -> Surface -> Surface -> IO Bool
blitOn x y src dst = SDL.blitSurface src Nothing dst offset
    where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

-- |Parse Events temporary function, will obviously need to be improved.
parseEvents :: IO Bool
parseEvents = do
    e <- SDL.pollEvent
    case e of
        SDL.KeyUp (Keysym SDLK_ESCAPE _ _) -> return True
        _ -> return False

-- |Initialization function. Kind of temporary, changes will depend
-- on the modification of loop.
raiseCane :: Font -> Screen -> IO ()
raiseCane font@(Font _ _ fw fh) (Screen f s w h) 
    = do let res = (fw * w, fh * h)
         SDL.withInit [SDL.InitEverything] $ do
             c <- launch f res
             SDL.setCaption s []
             glyphs <- initFonts font
             let cane = Cane c glyphs fw fh
             loop cane
    where launch :: Bool -> Resolution -> IO Surface
          launch f' (w', h') = let surf = screenSurface f' in
                            SDL.setVideoMode w' h' 32 surf
          screenSurface :: Bool -> [SurfaceFlag]
          screenSurface True = [SDL.SWSurface, SDL.Fullscreen]
          screenSurface False = [SDL.SWSurface]

-- Load the Font image, cut it into pieces to build our
-- GlyphLibrary. This will allow us to store the surface of 
-- each glyph in memory.
initFonts :: Font -> IO GlyphLibrary
initFonts (Font p v w h) = 
    do fontImg <- loadImages p
       let fw = surfaceGetWidth fontImg `div` w
       let fh = surfaceGetHeight fontImg `div` h
       liftM V.fromList $ mapM (storeChar fw fh w h fontImg v) [0..fw*fh] 
    where loadImages :: FilePath -> IO Surface
          loadImages path = load path >>= displayFormat


storeChar :: Int                -- Number of char / columns
             -> Int             -- Number of lines of char
             -> Width           -- Width of a char
             -> Height          -- Height of a char
             -> Surface         -- The image with all the chars
             -> Vertical        -- Glyphs order
             -> Int             -- Index of current char
             -> IO Surface      -- ONE char stored on a surface
storeChar cols rows w h src v n = 
    do surf <- prepareSurface' w h
       _ <- SDL.blitSurface src (pickRect v) surf Nothing         
       return surf
    where pickRect False = hRect
          pickRect True = vRect
          xChar = (n `mod` rows) * w
          yChar = (n `div` cols) * h
          hRect = Just Rect { rectX = xChar
                            , rectY = yChar
                            , rectW = w
                            , rectH = h}
          vRect = Just Rect { rectX = yChar
                            , rectY = xChar
                            , rectW = w
                            , rectH = h }
