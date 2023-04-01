{-# LANGUAGE OverloadedStrings #-}

module GameLogic (
  updateGroups
, boardSize
, isWhite
, isBlack
, isEmpty
, replace
, checkFree
) where

import Data.Ord               (comparing)
import Data.List as DL        (intersect, insert, sortBy, delete, union, nub, length, (\\))

import DataTypes


boardSize :: Int
boardSize = 19


-- Checks if a slot adjacent to a given slot is empty, and inserts it in the array if it is
checkFree :: World -> Int -> Int -> World
checkFree w x y = do
  if x >= 0
  then do
    let bs = ((board w) !! x) !! y
    let m = (x,y)
    let s = (boardSize-1)
    let b = [(u,v) | u <- [0..s], v <- [0..s]]

    -- The slots on the board that are empty
    let li = (b \\ (whiteMarkerPos w)) \\ (blackMarkerPos w)

    -- Check if the slot is occupied by a white marker
    if isWhite bs
    then do

      -- Uses the checkNbors function to find the empty slots around the given marker
      let new = union (checkNbors m li) (whiteFree w)

      checkFree w{whiteFree = new} (x-1) y

    -- Repeat for if the slot is occupied by a black marker
    else if isBlack bs
    then do
      let new = union (checkNbors m li) (blackFree w)
      checkFree w{blackFree = new} (x-1) y

    else checkFree w (x-1) y

  else if y > 0
    then do
      checkFree w (boardSize-1) (y-1)
    else w





-- Joins groups together and sorts them
fixList :: [[(Int, Int)]] -> [[(Int, Int)]]
fixList l = joinGroups x li
  where
    li = completeSort l
    x = (DL.length li -1)


-- Joins groups with common markers together
joinGroups :: Int -> [[(Int, Int)]] -> [[(Int, Int)]]
joinGroups x li = do
  if x > 0
  then do
    let s = (li !! x)
    let t = (li !! (x-1))
    let newLi = if DL.length (intersect s t) > 0 then insert (union s t) (delete s $ delete t li) else li

    joinGroups (x-1) newLi

  else li






-- Checks if a given marker has a neighbor of the same color
checkNbors :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkNbors m mPos = (checkLeft m mPos) ++ (checkRight m mPos) ++ (checkUp m mPos) ++ (checkDown m mPos)


checkLeft :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkLeft m mPos
  | (elem left mPos) = [left]
  | otherwise = []
  where
      left = ((fst m-1), snd m)

checkRight :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkRight m mPos
  | (elem right mPos) = [right]
  | otherwise = []
  where
      right = ((fst m+1), snd m)



checkUp :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkUp m mPos
  | (elem up mPos) = [up]
  | otherwise = []
  where
      up = (fst m, (snd m-1))

checkDown :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkDown m mPos
  | (elem down mPos) = [down]
  | otherwise = []
  where
      down = (fst m, (snd m+1))



findGroups :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findGroups m mPos = do
   let nbors = checkNbors m mPos
   if DL.length nbors > 0 then insert m nbors else [m]



-- Updates the coherent groups on the board
updateGroups :: Int -> Int -> World -> [[(Int, Int)]] -> [[(Int, Int)]] -> World
updateGroups x y w wli bli = do
  -- x is the amount of white markers currently on the board
  if x >= 0
  then do
    -- Find the potential group
    let group = findGroups ((whiteMarkerPos w) !! x) (whiteMarkerPos w)

    -- Insert them into the array
    let new = fixList $ insert group wli

    -- Loops through each white marker
    updateGroups (x-1) y w new bli

  -- Repeat for the black markers
  else
    if y >= 0
    then do
      let group = findGroups ((blackMarkerPos w) !! y) (blackMarkerPos w)
      let new = fixList $ insert group bli
      updateGroups x (y-1) w wli new

    -- When all markers on the board have been checked, return these two world variables, and remove duplicates
    else w { whiteGroups = (nub wli), blackGroups = (nub bli) }


-- Takes a list of lists and returns the list where all the sublists are sorted
-- and are ordered from biggest sublist to smallest
completeSort :: [[a]] -> [[a]]
completeSort li = sortBy (flip $ comparing DL.length) li
--  | ((sort $ head li) == head li) = sortBy (flip $ comparing DL.length) li
--  | otherwise = completeSort $ insertAt (sort $ head li) (DL.length li -1) (tail li)

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


-- Replacing an element in a list
replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list


