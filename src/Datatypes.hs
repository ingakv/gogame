{-# LANGUAGE OverloadedStrings #-}

module Datatypes where

import qualified SDL


data Intent
  = Idle
  | MouseMoved (Int, Int)
  | Quit
  | Press Slot
  | Release Slot
  | Hover Slot
  | Leave Slot


data World = World
  { exiting :: Bool
  , slots   :: SlotMap
  , mouseCoords   :: (Int, Int)
  , slotMap :: [[Slot]]
  , textures :: [(SDL.Texture, SDL.TextureInfo)]
  , mPos :: [(Int, Int)]
  }


data Slot
  = Empty
  | White
  | Black


data SlotMap = SlotMap
  { topLeft    :: Slot
  }

