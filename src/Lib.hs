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


windowSize :: (Int, Int)
windowSize = (1000, 750)


textColor :: SDL.Font.Color
textColor = SDL.V4 150 0 0 255


-- Create an empty board based on a given size
initBoard :: Int -> Int -> [Slot] -> [[Slot]]
initBoard x y li = do
  let finalList = chunksOf boardSize li
  if x > 0
  then do
      initBoard (x-1) y (insertAt Empty 0 li)
  else if y > 1
    then do
      initBoard boardSize (y-1) li
    else
      finalList


stonePos :: Int -> Int -> (Int, Int)
stonePos x y = (65 + 35*x, 45 + 35*y)


-- An array of all stone positions
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


initialWorld :: [(SDL.Texture, SDL.TextureInfo)] -> Font -> [[Slot]] -> World
initialWorld tx f b = World
  { exiting = False
  , mouseCoords = (0,0)
  , textures = tx
  , font = f
  , board = b
  , curColor = Black
  , allSlotPos = allStonePos (boardSize-1) (boardSize-1) []
  , whiteStonePos = []
  , blackStonePos = []
  , whiteGroups = []
  , blackGroups = []
  , whiteFree = []
  , blackFree = []
  }



-- Given a list of events, update the world
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w . fmap (payloadToIntent . SDL.eventPayload)


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ ||
     SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeEscape then Quit else
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeC then Clear else
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
    -- observe clever use of pattern matching to get x and y from the event!
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent _ = Press



idleWorld :: World -> World
idleWorld = id

hoverWorld :: (Int, Int) -> World -> World
hoverWorld coords w = w { mouseCoords = coords }

quitWorld :: World -> World
quitWorld w = w { exiting = True }

skipTurn :: World -> World
skipTurn w = w { curColor = switchColor w }

clearBoard :: World -> World
clearBoard w = w { board = initBoard boardSize boardSize [] }



applyIntent :: Intent -> World -> World
applyIntent Idle        = idleWorld
applyIntent Press       = pressWorld
applyIntent (MouseMoved coords)  = hoverWorld coords
applyIntent Quit        = quitWorld
applyIntent Skip        = skipTurn
applyIntent Clear       = clearBoard



intersect' :: World -> (Int,Int)
intersect' w = inter
  where
    a = fst $ mouseCoords w
    b = snd $ mouseCoords w

    -- Creates two lists consisting of all of the coordinates within 20 pixels of the mouse
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



 -- Converts the index number of a slot into coordinates
getPlacement :: Int -> Int -> (Int, Int)
getPlacement x y
  | y < boardSize = (x,y)
  | otherwise = getPlacement (x+1) (y-boardSize)


-- Updates updateStonePos, updateGroups and checkFree every time the mouse button is pressed
pressWorld :: World -> World
pressWorld w = w3
  where
    w1 = updateStonePos s s w { board = newMap, curColor = newColor , whiteFree = [] , blackFree = [] } [] []
    w2 = checkFree w1 0 0
    w3 = updateGroups 0 0 w2 [] []


    s = boardSize-1


    -- Get the slot where the mouse is currently hovering over
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


