{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified SDL
import qualified Common as C

import Data.Foldable          (foldl')

import SDL.Font
import Data.List.Split

import Datatypes


windowSize :: (Int, Int)
windowSize = (1000, 750)

boardSize :: Int
boardSize = 19

textColor :: SDL.Font.Color
textColor = SDL.V4 150 0 0 255



-- Create an empty board based on a given size
initBoard :: Int -> Int -> Int -> [Slot] -> [[Slot]]
initBoard x y size li = do
  let finalList = chunksOf size li
  if x > 0
  then do
      initBoard (x-1) y size (insertAt Empty 0 li)
  else if y > 1
    then do
      initBoard size (y-1) size li
    else
      finalList


markerPos :: Int -> Int -> (Int, Int)
markerPos x y = (65 + 35*x, 45 + 35*y)



-- An array of all marker positions
allMarkerPos :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
allMarkerPos x y size li = do
  if x >= 0
  then do
      allMarkerPos (x-1) y size (insertAt (markerPos x y) 0 li)
  else if y > 0
    then do
      allMarkerPos size (y-1) size li
    else
      li





initialWorld :: World
initialWorld = World
  { exiting = False
  , slotMap = initBoard boardSize boardSize boardSize []
  , mouseCoords = (0,0)
  , slots = initialSlots
  , textures = []
  , mPos = allMarkerPos (boardSize-1)  (boardSize-1) (boardSize-1) []
  }


initialSlots :: SlotMap
initialSlots = SlotMap
  { topLeft    = Empty }






-- Given a list of events, update the world
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w . fmap (payloadToIntent . SDL.eventPayload)


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else Idle
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
--payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle



-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = (MouseMoved (fromIntegral x, fromIntegral y))
  where
    q = selectTopLeft x y
    -- observe clever use of pattern matching to get x and y from the event!
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = t q
  where
    q = selectTopLeft x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
    t = if SDL.mouseButtonEventMotion e == SDL.Pressed
           then Press
           else Release


selectTopLeft :: (Num a, Ord a) => a -> a -> Slot
selectTopLeft x y
  | x >= 3200 && y <  2400 = Empty
  | otherwise            = undefined

applyIntent :: Intent -> World -> World
--applyIntent (Press q)   = pressWorld q
--applyIntent (Release q) = releaseWorld q
--applyIntent (Hover q)   = hoverWorld q
--applyIntent (Leave q)   = leaveWorld q
applyIntent (MouseMoved coords)  = hoverWorld coords
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld

updateSlotMap :: (Slot -> Slot) -> Slot -> SlotMap -> SlotMap
updateSlotMap f _    (SlotMap tr) = SlotMap (f tr)

{-
pressWorld :: Slot -> World -> World
pressWorld q w = w { slots = slots' }
  where slots' = updateSlotMap setDown id q (slots w)


releaseWorld :: Slot -> World -> World
releaseWorld q w = w { slots = slots' }
  where slots' = updateSlotMap setUp id q (slots w)

leaveWorld :: Slot -> World -> World
leaveWorld q w = w { slots = slots' }
  where slots' = updateSlotMap setOut setOver q (slots w)


-}

hoverWorld :: (Int, Int) -> World -> World
hoverWorld coords w = w { mouseCoords = coords }

{--
placeMarker :: Char -> Int -> [[String]] -> [[String]]
placeMarker column row board
  | isDigit column = board
  | otherwise = insertAt (insertAt ("X") (fromJust(elemIndex (toUpper column) ['A'..'Z'])) (delete ((board !! (fromJust(elemIndex (toUpper column) ['A'..'Z'])-1)) !! (row-1)) (board !! (row-1)))) (row-1) (delete (board !! (row-1)) ((board)))


-}


-- Checks whether a given slot is empty or not (occupied by a marker)
isEmpty :: Slot -> Bool
isEmpty Empty = True
isEmpty _ = False

isWhite :: Slot -> Bool
isWhite White = True
isWhite _ = False

isBlack :: Slot -> Bool
isBlack Black = True
isBlack _ = False


setEmpty :: Slot -> Slot
setEmpty _ = Empty
setOut :: Slot -> Slot
setOut _ = Empty

setDown :: Slot -> Slot
setDown _ = Empty

setUp :: Slot -> Slot
setUp _ = Empty


setOver :: Slot -> Slot
setOver _ = White





idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }


------------------ A few small useful functions -----------------------------

-- Inserts a given character t times for every n characters provided in the string
insertEveryN :: Int ->  Int -> Char -> [Char] -> [Char]
insertEveryN 0 _ _ xs = xs
insertEveryN _ _ _ [] = []
insertEveryN n t y xs
 | length xs < n = xs
 | t < 1 = xs
 | otherwise = take n xs ++ (concatMap (replicate t) [y]) ++ insertEveryN n t y (drop n xs)



insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

