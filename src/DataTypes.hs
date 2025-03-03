{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import qualified SDL
import qualified SDL.Font

data Intent
  = Idle               -- No action, the system is idle
  | MouseMoved (Int, Int) -- Mouse has moved to the given coordinates (Int, Int)
  | Press              -- User has pressed a button or triggered an action
  | Quit               -- User wants to quit the game
  | Skip               -- User wants to skip their turn
  | Clear              -- User wants to clear the board


data World = World
  { exiting :: Bool                 -- A flag indicating whether the game should exit (True or False)
  , mouseCoords :: (Int, Int)       -- The current position of the mouse pointer
  , textures :: [(SDL.Texture, SDL.TextureInfo)] -- List of textures (including texture data and information) used for rendering game elements
  , curColor :: Slot                -- The current player's color (White or Black)
  , font :: SDL.Font.Font           -- The font used for rendering text
  , board :: [[Slot]]               -- The game board represented as a 2D list of slots, each representing an empty, white, or black slot
  , prevBoard1 :: [[Slot]]
  , prevBoard2 :: [[Slot]]
  , allSlotPos :: [(Int, Int)]      -- List of all possible slot positions on the board
  , whiteStonePos :: [(Int, Int)]   -- List of positions where white stones are placed
  , blackStonePos :: [(Int, Int)]   -- List of positions where black stones are placed
  , whiteGroups :: [[(Int, Int)]]   -- List of groups of white stones (each group is a list of positions)
  , blackGroups :: [[(Int, Int)]]   -- List of groups of black stones (each group is a list of positions)
  , whiteFree :: [(Int, Int)]       -- List of free positions for white stones (those not occupied by other stones)
  , blackFree :: [(Int, Int)]       -- List of free positions for black stones
  }


data Slot
  = Empty  -- The slot is empty
  | White  -- The slot contains a white stone
  | Black  -- The slot contains a black stone
  deriving (Eq)
