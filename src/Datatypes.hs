{-# LANGUAGE OverloadedStrings #-}

module Datatypes where

import qualified SDL

data Intent
  = Idle
  | MouseMoved (Int, Int)
  | Quit
  | Press


data World = World
  { exiting :: Bool
  , mouseCoords   :: (Int, Int)
  , board :: [[Slot]]
  , textures :: [(SDL.Texture, SDL.TextureInfo)]
  , mPos :: [(Int, Int)]
  , curColor :: Slot
  }


data Slot
  = Empty
  | White
  | Black

