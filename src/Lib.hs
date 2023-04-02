{-# LANGUAGE OverloadedStrings #-}

module Lib (
  markerPos
, windowSize
, intersect'
, textColor
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


markerPos :: Int -> Int -> (Int, Int)
markerPos x y = (65 + 35*x, 45 + 35*y)


-- An array of all marker positions
allMarkerPos :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
allMarkerPos x y li = do
  if x >= 0
  then do
      allMarkerPos (x-1) y (insertAt (markerPos x y) 0 li)
  else if y > 0
    then do
      allMarkerPos (boardSize-1) (y-1) li
    else
      li



initialWorld :: [(SDL.Texture, SDL.TextureInfo)] -> Font -> World
initialWorld tx f = World
  { exiting = False
  , board = initBoard boardSize boardSize []
  , mouseCoords = (0,0)
  , textures = tx
  , font = f
  , allSlotPos = allMarkerPos (boardSize-1) (boardSize-1) []
  , whiteMarkerPos = []
  , blackMarkerPos = []
  , curColor = Black
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
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else
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



applyIntent :: Intent -> World -> World
applyIntent Idle        = idleWorld
applyIntent Press       = pressWorld
applyIntent (MouseMoved coords)  = hoverWorld coords
applyIntent Quit        = quitWorld
applyIntent Skip        = skipTurn



intersect' :: World -> (Int,Int)
intersect' w = inter
  where
    a = fst $ mouseCoords w
    b = snd $ mouseCoords w

    -- Creates two lists consisting of all of the coordinates within 20 points of the mouse
    lix = [(a-20) .. (a)]
    liy = [(b-20) .. (b)]

    -- Goes over all of the positions of the slots and sees if any of the slots overlap
    -- If they do, the selected slots coordinates will be returned
    inters = intersect [ (x,y) | x <- lix, y <- liy ] $ allSlotPos w

    inter =
      if (DL.length inters) > 0
      then inters !! 0
      else (-1,-1)



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


-- Updates updateMarkerPos and updateGroups for every press of the mouse
pressWorld :: World -> World
pressWorld w = w3
  where
    w1 = updateMarkerPos s s w { board = newMap, curColor = newColor } [] []
    w2 = updateGroups lw lb w1 [] []
    w3 = checkFree w2{whiteFree = [] , blackFree = []} (boardSize-1) (boardSize-1)

    -- Amount of markers (-1)
    lw = DL.length (whiteMarkerPos w)-1
    lb = DL.length (blackMarkerPos w)-1

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
          -- Replace the slot with the new one
          let newRow = replace (fst index) (curColor w) ((board w) !! (snd index))

          -- Switch the active color
          (replace (snd index) newRow (board w), switchColor w)

        else (board w, curColor w)

      else (board w, curColor w)



updateMarkerPos :: Int -> Int -> World -> [(Int, Int)] -> [(Int, Int)] -> World
updateMarkerPos x y w wli bli = do
  if x > 0
    then do
        if isWhite (((board w) !! x) !! y)
        then do updateMarkerPos (x-1) y w (insertAt (x,y) 0 $ wli) bli

        else
          if isBlack (((board w) !! x) !! y)
          then do updateMarkerPos (x-1) y w wli (insertAt (x,y) 0 $ bli)
          else updateMarkerPos (x-1) y w wli bli
  else
    if y > 0
    then do updateMarkerPos (boardSize-1) (y-1) w wli bli
    else w { whiteMarkerPos = wli, blackMarkerPos = bli }


