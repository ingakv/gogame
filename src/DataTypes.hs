{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import qualified SDL
import qualified SDL.Font

data Intent
  = Idle
  | MouseMoved (Int, Int)
  | Press
  | Quit
  | Skip
  | Clear


data World = World
  { exiting :: Bool
  , mouseCoords :: (Int, Int)
  , textures :: [(SDL.Texture, SDL.TextureInfo)]
  , curColor :: Slot
  , font :: SDL.Font.Font
  , board :: [[Slot]]
  , allSlotPos :: [(Int, Int)]
  , whiteMarkerPos :: [(Int, Int)]
  , blackMarkerPos :: [(Int, Int)]
  , whiteGroups :: [[(Int, Int)]]
  , blackGroups :: [[(Int, Int)]]
  , whiteFree :: [(Int, Int)]
  , blackFree :: [(Int, Int)]
  }


data Slot
  = Empty
  | White
  | Black

