{-# LANGUAGE OverloadedStrings #-}

module GameLogic (
  updateGroups
) where

import Data.Ord               (comparing)
import Data.List as DL        (intersect, insert, sortBy, delete, union, nub, length)

import DataTypes


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



checkLeft :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkLeft m mPos
  | (elem left mPos) = [left]
  | otherwise = []
  where
      left = ((fst m-1), snd m)




-- Checks if a given marker has a neighbor of the same color
checkNbors :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkNbors m mPos = (checkLeft m mPos) ++ (checkRight m mPos) ++ (checkUp m mPos) ++ (checkDown m mPos)



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

