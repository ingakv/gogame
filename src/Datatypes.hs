{-# LANGUAGE OverloadedStrings #-}

module Datatypes where

import qualified SDL

data Intent
  = Idle
  | MouseMoved (Int, Int)
  | Quit
  | Press Slot
  | Hover Slot
  | Leave Slot


data World = World
  { exiting :: Bool
  , mouseCoords   :: (Int, Int)
  , slotMap :: [[Slot]]
  , textures :: [(SDL.Texture, SDL.TextureInfo)]
  , mPos :: [(Int, Int)]
  }


data Slot
  = Empty
  | White
  | Black

