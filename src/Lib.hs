{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified SDL

import Data.Foldable          (foldl')
import SDL.Font
import Data.List.Split        (chunksOf)
import Data.List as DL        (intersect, elemIndex, length)

import DataTypes as DT
import GameLogic


windowSize :: (Int, Int)
windowSize = (1000, 750)

boardSize :: Int
boardSize = 19

textColor :: SDL.Font.Color
textColor = SDL.V4 150 0 0 255



-- Create an empty board based on a given size
initBoard :: Int -> Int -> Int -> [Slot] -> [[Slot]]
initBoard x y s li = do
  let finalList = chunksOf s li
  if x > 0
  then do
      initBoard (x-1) y s (insertAt Empty 0 li)
  else if y > 1
    then do
      initBoard s (y-1) s li
    else
      finalList


markerPos :: Int -> Int -> (Int, Int)
markerPos x y = (65 + 35*x, 45 + 35*y)


-- An array of all marker positions
allMarkerPos :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
allMarkerPos x y s li = do
  if x >= 0
  then do
      allMarkerPos (x-1) y s (insertAt (markerPos x y) 0 li)
  else if y > 0
    then do
      allMarkerPos s (y-1) s li
    else
      li



initialWorld :: [(SDL.Texture, SDL.TextureInfo)] -> Font -> World
initialWorld tx f = World
  { exiting = False
  , board = initBoard boardSize boardSize boardSize []
  , mouseCoords = (0,0)
  , textures = tx
  , allSlotPos = allMarkerPos (boardSize-1)  (boardSize-1) (boardSize-1) []
  , whiteMarkerPos = []
  , blackMarkerPos = []
  , curColor = Black
  , whiteGroups = []
  , blackGroups = []
  , font = f
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
pressWorld w = w2
  where
    w1 = updateMarkerPos s s w { board = newMap, curColor = newColor } [] []
    w2 = updateGroups lw lb w1 [] []

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




------------------ A few small useful functions -----------------------------


-- Checks the color of a given slot
isWhite :: Slot -> Bool
isWhite White = True
isWhite _ = False

isBlack :: Slot -> Bool
isBlack Black = True
isBlack _ = False

isEmpty :: Slot -> Bool
isEmpty Empty = True
isEmpty _ = False


-- Inserts a given character t times for every n characters provided in the string
insertEveryN :: Int ->  Int -> Char -> [Char] -> [Char]
insertEveryN 0 _ _ xs = xs
insertEveryN _ _ _ [] = []
insertEveryN n t y xs
 | DL.length xs < n = xs
 | t < 1 = xs
 | otherwise = take n xs ++ (concatMap (replicate t) [y]) ++ insertEveryN n t y (drop n xs)


-- Inserts an element at a given location
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as


-- Replacing an element in a list
replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list


fromJust :: Maybe Int -> Int
fromJust (Just x) = x
fromJust Nothing = -1


