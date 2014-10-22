module Cane.Types
where

import Glove.Types
import Graphics.UI.SDL
import qualified Data.Vector as V

-- Are the glyphs from the font image sorted horizontally
-- (i.e., B left to A) or vertically (i.e., B below A)
type Vertical = Bool

data Font = Font { _path :: FilePath
                 , _vertical :: Vertical
                 , _charWidth :: Width
                 , _charHeight :: Height }

data Screen = Screen { _fullScreen :: Bool
                     , _title :: String
                     , _cWidth :: Width
                     , _cHeight :: Height }

type GlyphLibrary = V.Vector Surface

data Cane = Cane { _screen :: Surface       -- The main screen
                 , _glyphs :: GlyphLibrary  -- The char surfaces
                 , _tileWidth :: Width
                 , _tileHeight :: Height }

-- Screen resolution in pixel
type Resolution = (Width, Height)
