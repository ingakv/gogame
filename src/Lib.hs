{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified SDL

import Data.Foldable          (foldl')
import SDL.Font
import Data.Ord               (comparing)
import Data.List.Split        (chunksOf)
import Data.List as DL        (intersect, elemIndex, insert, sort, sortBy, delete, union, nub, length, head, tail)

import DataTypes as DT


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
      if (length inters) > 0
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



-- Updates updateMarkerPos and updateGroups for every press
pressWorld :: World -> World
pressWorld w = w2
  where
    w1 = updateMarkerPos (boardSize-1) (boardSize-1) w { board = newMap, curColor = newColor } [] []
    w2 = runUG w1

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




fixList :: [[(Int, Int)]] -> [[(Int, Int)]]
fixList l = remDupSub li bigLi smallLi (DL.length smallLi -1)
  where
    li = completeSort l
    bigLi = DL.head li
    smallLi = DL.tail li



-- Removes sublists that consists solely on elements that can all be found in another list
remDupSub :: [[(Int, Int)]] -> [(Int, Int)] -> [[(Int, Int)]] -> Int -> [[(Int, Int)]]
remDupSub li bLi sLi x = do
  if x >= 0
  then do
    let newLi = if sort (union (sLi !! x) bLi) == (sort bLi) then (delete (sLi !! x) li) else li
    remDupSub newLi bLi sLi (x-1)
  else li

-- Function for an easier way to call the resetNbors function
runRN :: [(Int, Int)] -> [((Int, Int), Bool)]
runRN arr = resetNbors arr ((DL.length arr) -1) []

-- Converts the array of markers into an array ready to go through neighbor checking
resetNbors :: [(Int, Int)] -> Int -> [((Int, Int), Bool)] -> [((Int, Int), Bool)]
resetNbors m x li = do
  if x >= 0
  then do
      resetNbors m (x-1) (insertAt ((m !! x), False) 0 li)
  else li




checkLeft :: ((Int, Int), Bool) -> [(Int, Int)] -> [((Int, Int), Bool)]
checkLeft (m, visited) mPos = do
  if (elem left mPos) && (visited == False) then [left'] ++ (checkNbors left' mPos) else []
  where
      left = ((fst m-1), snd m)
      left' = (left, True)


checkRight :: ((Int, Int), Bool) -> [(Int, Int)] -> [((Int, Int), Bool)]
checkRight (m, visited) mPos = do
  if (elem right mPos) && (visited == False) then [right'] ++ (checkNbors right' mPos) else []
  where
      right = ((fst m+1), snd m)
      right' = (right, True)


checkUp :: ((Int, Int), Bool) -> [(Int, Int)] -> [((Int, Int), Bool)]
checkUp (m, visited) mPos = do
  if (elem up mPos) && (visited == False) then [up'] ++ (checkNbors up' mPos) else []
  where
      up = (fst m, (snd m-1))
      up' = (up, True)

checkDown :: ((Int, Int), Bool) -> [(Int, Int)] -> [((Int, Int), Bool)]
checkDown (m, visited) mPos = do
  if (elem down mPos) && (visited == False) then [down'] ++ (checkNbors down' mPos) else []
  where
      down = (fst m, (snd m+1))
      down' = (down, True)


checkNbors :: ((Int, Int), Bool) -> [(Int, Int)] -> [((Int, Int), Bool)]
checkNbors m mPos = (checkLeft m mPos) ++ (checkRight m mPos) ++ (checkUp m mPos) ++ (checkDown m mPos)


findGroups :: ((Int, Int), Bool) -> [(Int, Int)] -> [(Int, Int)]
findGroups (m,v) mPos = do
   let nbors = checkNbors (m,v) mPos
   if length nbors > 0 then insert m (getn nbors) else [m]



-- Function for running the next function more clearly
runUG :: World -> World
runUG w = w { whiteGroups = (fst newWorld), blackGroups = (snd newWorld) }
  where
    wm = runRN (whiteMarkerPos w)
    bm = runRN (blackMarkerPos w)
    lw = length (whiteMarkerPos w)-1
    lb = length (blackMarkerPos w)-1
    newWorld = updateGroups lw lb wm bm [] []


-- Updates the coherent groups on the board
updateGroups :: Int -> Int -> [((Int, Int), Bool)] -> [((Int, Int), Bool)] -> [[(Int, Int)]] -> [[(Int, Int)]] -> ([[(Int, Int)]] , [[(Int, Int)]])
updateGroups x y wm bm wli bli = do
  -- x is the amount of white markers currently on the board
  if x >= 0
  then do
    -- Find the potential group
    let group = findGroups (wm !! x) $ getn wm

    -- Insert them into the array
    let new = fixList $ insert group wli

    -- Loops through each white marker
    updateGroups (x-1) y wm bm new bli

  -- Repeat for the black markers
  else
    if y >= 0
    then do
      let group = findGroups (bm !! y) $ getn bm
      let new = fixList $ insert group bli
      updateGroups x (y-1) wm bm wli new

    -- When all markers on the board have been checked, return these two world variables, and remove duplicates
    else  ((nub wli), (nub bli))




-- Takes a list of lists and returns the list where all the sublists are sorted
-- and are ordered from biggest sublist to smallest
completeSort :: [[a]] -> [[a]]
completeSort li = sortBy (flip $ comparing length) li
--  | ((sort $ head li) == head li) = sortBy (flip $ comparing length) li
--  | otherwise = completeSort $ insertAt (sort $ head li) (length li -1) (tail li)




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
 | length xs < n = xs
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


-- Extracts the position from the tuple
getn :: [(a, b)] -> [a]
getn [] = []
getn ((pos,_):xs) = [pos] ++ getn xs

