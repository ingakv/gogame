{-# LANGUAGE OverloadedStrings #-}

module Lib (
  stonePos
, windowSize
, intersect'
, textColor
, initBoard
, initialWorld
, updateWorld
) where

import qualified SDL

import Data.Foldable          (foldl')
import SDL.Font
import Data.List.Split        (chunksOf)
import Data.List as DL        (intersect, elemIndex, length)

import DataTypes as DT
import GameLogic

-- Window size for the SDL application
windowSize :: (Int, Int)
windowSize = (1000, 750)

-- Text color used for rendering fonts
textColor :: SDL.Font.Color
textColor = SDL.V4 150 0 0 255

-- Create an empty game board based on the given size (x by y)
initBoard :: Int -> Int -> [Slot] -> [[Slot]]
initBoard x y li = do
  -- Splits the list into rows
  let finalList = chunksOf boardSize li
  if x > 0
  then do
      -- Recursively initialize rows
      initBoard (x-1) y (insertAt Empty 0 li)
  else if y > 1
    then do
      -- Move to the next column
      initBoard boardSize (y-1) li
    else
      -- Return the final initialized board
      finalList

-- Calculate the pixel position of a stone based on its coordinates on the board
stonePos :: Int -> Int -> (Int, Int)
stonePos x y = (65 + 35*x, 45 + 35*y)

-- Generate a list of all possible stone positions on the board
allStonePos :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
allStonePos x y li = do
  if x >= 0
  then do
      allStonePos (x-1) y (insertAt (stonePos x y) 0 li)
  else if y > 0
    then do
      allStonePos (boardSize-1) (y-1) li
    else
      li

-- Initialize the game world with default values
initialWorld :: [(SDL.Texture, SDL.TextureInfo)] -> Font -> [[Slot]] -> World
initialWorld tx f b = World
  { exiting = False               -- Whether the game is exiting
  , mouseCoords = (0,0)           -- Current mouse coordinates
  , textures = tx                 -- List of textures
  , font = f                      -- Font used for rendering
  , board = b                     -- Initial board setup
  , curColor = Black              -- Current player color
  , allSlotPos = allStonePos (boardSize-1) (boardSize-1) [] -- Precomputed slot positions
  , whiteStonePos = []            -- Positions of white stones
  , blackStonePos = []            -- Positions of black stones
  , whiteGroups = []              -- Groupings of white stones
  , blackGroups = []              -- Groupings of black stones
  , whiteFree = []                -- Free positions around white stones
  , blackFree = []                -- Free positions around black stones
  }

-- Update the world based on a list of SDL events
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w . fmap (payloadToIntent . SDL.eventPayload)


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q or Escape is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ ||
     SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeEscape then Quit else

  -- C button to clear the board
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeC then Clear else

  -- S button to skip a turn
  if (SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeS)
   && (SDL.keyboardEventKeyMotion e == SDL.Pressed)
   then Skip else Idle
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle


-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = (MouseMoved (fromIntegral x, fromIntegral y))
  where
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e

-- Handle mouse button events
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent _ = Press

-- Leave the world unchanged for Idle intents
idleWorld :: World -> World
idleWorld = id

-- Update the world with new hover coordinates
hoverWorld :: (Int, Int) -> World -> World
hoverWorld coords w = w { mouseCoords = coords }

-- Mark the world as exiting
quitWorld :: World -> World
quitWorld w = w { exiting = True }

-- Skip the current player's turn
skipTurn :: World -> World
skipTurn w = w { curColor = switchColor w }

-- Reset the board to its initial state
clearBoard :: World -> World
clearBoard w = w { board = initBoard boardSize boardSize [] }

-- Apply a given Intent to the World
applyIntent :: Intent -> World -> World
applyIntent Idle        = idleWorld
applyIntent Press       = pressWorld
applyIntent (MouseMoved coords)  = hoverWorld coords
applyIntent Quit        = quitWorld
applyIntent Skip        = skipTurn
applyIntent Clear       = clearBoard

-- Find the slot under the mouse cursor (if any)
intersect' :: World -> (Int,Int)
intersect' w = inter
  where
    a = fst $ mouseCoords w
    b = snd $ mouseCoords w

    -- Create two lists consisting of all of the coordinates within 20 pixels of the mouse
    lix = [(a-20) .. (a)]
    liy = [(b-20) .. (b)]

    -- Goes over all of the positions of the slots and sees if any of the slots overlap
    -- If they do, the selected slots coordinates will be returned
    inters = intersect [ (x,y) | x <- lix, y <- liy ] $ allSlotPos w

    inter =
      -- If it is hovering over a slot, return the position
      if (DL.length inters) > 0
      then inters !! 0
      else (-1,-1)

-- Swaps the active player
switchColor :: World -> Slot
switchColor w = newColor
  where
    newColor
      | isBlack $ curColor w = White
      | isWhite $ curColor w = Black
      | otherwise = Empty

-- Calculate the coordinates of a slot based on its index
getPlacement :: Int -> Int -> (Int, Int)
getPlacement x y
  | y < boardSize = (x,y)
  | otherwise = getPlacement (x+1) (y-boardSize)

-- Handle a mouse button press and update the world accordingly
pressWorld :: World -> World
pressWorld w = w3
  where
    w1 = updateStonePos s s w { board = newMap, curColor = newColor , whiteFree = [] , blackFree = [] } [] []
    w2 = checkFree w1 0 0
    w3 = updateGroups 0 0 w2 [] []

    s = boardSize-1

    -- Get the slot currently hovered by the mouse
    inters = intersect' w

    (newMap, newColor) =
      -- Checks if the mouse was hovering over a slot when it was pressed
      if (fst inters) >= 0
      then do
        -- If it was, extract the placement of the slot
        let index = getPlacement 0 $ fromJust $ elemIndex (inters) $ allSlotPos w

        -- Checks if the slot is already occupied
        if isEmpty ((board w !! snd index) !! fst index)
        then do
          -- Replace the slot with the new one and switch the active color
          (replaceBoard w index (curColor w) , switchColor w)

        else (board w, curColor w)

      else (board w, curColor w)


-- Updates the positions of the stones
updateStonePos :: Int -> Int -> World -> [(Int, Int)] -> [(Int, Int)] -> World
updateStonePos x y w wli bli = do
  if x > 0
    then do
        -- Inserts all the white stones into a list
        if isWhite (((board w) !! x) !! y)
        then do updateStonePos (x-1) y w (insertAt (x,y) 0 $ wli) bli

        else
          -- Repeats for all of the black stones
          if isBlack (((board w) !! x) !! y)
          then do updateStonePos (x-1) y w wli (insertAt (x,y) 0 $ bli)
          else updateStonePos (x-1) y w wli bli
  else
    if y > 0
    then do updateStonePos (boardSize-1) (y-1) w wli bli
    else w { whiteStonePos = wli, blackStonePos = bli }


