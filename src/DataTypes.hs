{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import qualified SDL

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

