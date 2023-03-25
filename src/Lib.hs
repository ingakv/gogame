{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified SDL

import Data.Foldable          (foldl')

import SDL.Font
import Data.List.Split
import Data.List        (intersect, elemIndex)

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




initialWorld :: [(SDL.Texture, SDL.TextureInfo)] -> World
initialWorld tx = World
  { exiting = False
  , slotMap = initBoard boardSize boardSize boardSize []
  , mouseCoords = (0,0)
  , textures = tx
  , mPos = allMarkerPos (boardSize-1)  (boardSize-1) (boardSize-1) []
  }





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
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle


-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = (MouseMoved (fromIntegral x, fromIntegral y))
  where
    -- observe clever use of pattern matching to get x and y from the event!
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = Press Black
  where
    q = selectSlot x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e



selectSlot :: (Num a, Ord a) => a -> a -> Slot
selectSlot x y
  | x > 0 && y <  0 = Black
  | otherwise            = undefined

applyIntent :: Intent -> World -> World
applyIntent (Press q)   = pressWorld 5 5
--applyIntent (Hover q)   = hoverWorld q
--applyIntent (Leave q)   = leaveWorld q
applyIntent (MouseMoved coords)  = hoverWorld coords
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld


{-

releaseWorld :: Slot -> World -> World
releaseWorld q w = w { slots = slotts }
  where slotts = updateSlotMap setWhite (slots w)

leaveWorld :: Slot -> World -> World
leaveWorld q w = w { slots = slotts }
  where slotts = updateSlotMap setOut setOver q (slots w)

-}


pressWorld :: Int -> Int -> World -> World
pressWorld x y w = w { slotMap = newMap }
  where
        replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

        getPlacement x y
          | y < boardSize = (x,y)
          | otherwise = getPlacement (x+1) (y-boardSize)

        a = fst $ mouseCoords w
        b = snd $ mouseCoords w

        lix = [(a-20) .. (a)]
        liy = [(b-20) .. (b)]

        inters = intersect [ (x,y) | x <- lix, y <- liy ] $ mPos w

        newMap =
          if (length inters) > 0
          then do
            let index = getPlacement 0 $ fromJust $ elemIndex (inters !! 0) $ mPos w

            let newRow = replace (fst index) White ((slotMap w) !! (snd index))
            replace (snd index) newRow (slotMap w)
          else (slotMap w)



hoverWorld :: (Int, Int) -> World -> World
hoverWorld coords w = w { mouseCoords = coords }


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

setWhite :: Slot -> Slot
setWhite _ = White

setBlack :: Slot -> Slot
setBlack _ = Black

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


fromJust :: Maybe Int -> Int
fromJust (Just x) = x



