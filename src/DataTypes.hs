{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import qualified SDL
import qualified SDL.Font

data Intent
  = Idle
  | MouseMoved (Int, Int)
  | Quit
  | Skip
  | Press


data World = World
  { exiting :: Bool
  , mouseCoords   :: (Int, Int)
  , board :: [[Slot]]
  , textures :: [(SDL.Texture, SDL.TextureInfo)]
  , font :: SDL.Font.Font
  , allSlotPos :: [(Int, Int)]
  , whiteMarkerPos :: [(Int, Int)]
  , blackMarkerPos :: [(Int, Int)]
  , curColor :: Slot
  , whiteGroups :: [[(Int, Int)]]
  , blackGroups :: [[(Int, Int)]]
  }


data Slot
  = Empty
  | White
  | Black

