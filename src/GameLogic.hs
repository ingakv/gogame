{-# LANGUAGE OverloadedStrings #-}

module GameLogic (
  updateStones
, boardSize
, isWhite
, isBlack
, isEmpty
, replace
, replaceBoard
, insertAt
, fromJust
, insertEveryN
, slotToChar
, charToSlot
, isEqual
) where

import Data.Ord               (comparing)
import Data.List as DL        (intersect, insert, sortBy, delete, union, nub, length, (\\))

import DataTypes

-- Define the size of the board
boardSize :: Int
boardSize = 19


-- Updates the positions of the stones
updateStonePos :: World -> World
updateStonePos w = helperUpdateStonePos (boardSize - 1) (boardSize - 1) w [] []

helperUpdateStonePos :: Int -> Int -> World -> [(Int, Int)] -> [(Int, Int)] -> World
helperUpdateStonePos x y w wli bli = do
  if x >= 0
    then do
        -- Inserts all the white stones into a list
        if isWhite (((board w) !! x) !! y)
        then do helperUpdateStonePos (x-1) y w (insertAt (x,y) 0 $ wli) bli

        else
          -- Repeats for all of the black stones
          if isBlack (((board w) !! x) !! y)
          then do helperUpdateStonePos (x-1) y w wli (insertAt (x,y) 0 $ bli)
          else helperUpdateStonePos (x-1) y w wli bli
  else
    if y > 0
    then do helperUpdateStonePos (boardSize-1) (y-1) w wli bli
    else w { whiteStonePos = wli, blackStonePos = bli }



-- Checks if a slot adjacent to a given slot is empty, and inserts it in the array if it is
checkFree :: World -> World
checkFree w = helperCheckFree w 0 0

helperCheckFree :: World -> Int -> Int -> World
helperCheckFree w x y = do
  -- All slots / intersections on the board
  let b = [(u,v) | u <- [0..(boardSize-1)], v <- [0..(boardSize-1)]]

  -- The slots on the board that are empty
  let li = (b \\ (whiteStonePos w)) \\ (blackStonePos w)

  -- Check if the slot is occupied by a white stone
  if x < (length $ whiteGroups w)
  then do
    let bs = whiteGroups w !! x

    -- Find the empty slots around the given stone
    let freeGr = concat [checkNbors a li | a <- bs]

    -- Handle capture if the group's degree of freedom is 0
    let (newPos , newWorld) = if length freeGr == 0 then ((whiteStonePos w) \\ bs , deleteGroup w bs $ (length bs)-1) else (whiteStonePos w , w)

    -- Update the free slots and world state
    let new = union freeGr (whiteFree w)
    helperCheckFree newWorld{ whiteStonePos = newPos , whiteFree = new } (x+1) y


    -- Repeat for if the slot is occupied by a black stone
  else
    if y < (length $ blackGroups w)
    then do
      let bs = blackGroups w !! y

      let freeGr = concat [checkNbors a li | a <- bs]

      let (newPos , newWorld) = if length freeGr == 0 then ((blackStonePos w) \\ bs , deleteGroup w bs $ (length bs)-1) else (blackStonePos w , w)

      let new = union freeGr (blackFree w)

      helperCheckFree newWorld{ blackStonePos = newPos , blackFree = new } x (y+1)

    else w


-- Deletes and entire group of stones
deleteGroup :: World -> [(Int, Int)] -> Int -> World
deleteGroup w gr x
  | x < 0 = w
  | gr == [] = w
  | otherwise = deleteGroup w{board = newBoard} gr (x-1)
  where newBoard = replaceBoard w (snd (gr !! x) , fst (gr !! x)) Empty

-- Sort and merge groups of connected stones
fixList :: [[(Int, Int)]] -> [[(Int, Int)]]
fixList l = joinGroups x li
  where
    li = completeSort l
    x = (DL.length li -1)

-- Merges groups with common stones
joinGroups :: Int -> [[(Int, Int)]] -> [[(Int, Int)]]
joinGroups x li = do
  if x > 0
  then do
    let s = (li !! x)
    let t = (li !! (x-1))
    let newLi = if DL.length (intersect s t) > 0 then insert (union s t) (delete s $ delete t li) else li
    joinGroups (x-1) newLi

  else li

-- Checks adjacent slots for stones of the same color
checkNbors :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
checkNbors m mPos = (checkLeft m mPos) ++ (checkRight m mPos) ++ (checkUp m mPos) ++ (checkDown m mPos)

-- Helper functions to check specific directions
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


-- Finds connecting stones that are in a '+' shape
findGroups :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findGroups m mPos = do
   let nbors = checkNbors m mPos
   if DL.length nbors > 0 then insert m nbors else [m]

-- Updates groups of stones on the board
updateGroups :: World -> World
updateGroups w = helperUpdateGroups 0 0 w [] []

helperUpdateGroups :: Int -> Int -> World -> [[(Int, Int)]] -> [[(Int, Int)]] -> World
helperUpdateGroups x y w wli bli = do
  -- x is the amount of white stones currently on the board, y is the amount of black stones
  if x < DL.length (whiteStonePos w)
  then do
    -- Find the potential group
    let group = findGroups ((whiteStonePos w) !! x) (whiteStonePos w)

    -- Insert it into the array
    let new = fixList $ insert group wli

    -- Loops through each white stone
    helperUpdateGroups (x+1) y w new bli

  -- Repeat for the black stones
  else
    if y < DL.length (blackStonePos w)
    then do
      let group = findGroups ((blackStonePos w) !! y) (blackStonePos w)
      let new = fixList $ insert group bli
      helperUpdateGroups x (y+1) w wli new

    -- When all stones on the board have been checked, return these two world variables, and remove duplicates
    else w { whiteGroups = (nub wli), blackGroups = (nub bli) }

-- Sorts sublists by size in descending order
completeSort :: [[a]] -> [[a]]
completeSort li = sortBy (flip $ comparing DL.length) li

-- Updates the stones in the world
updateStones :: World -> World
updateStones w = updateGroups $ checkFree $ updateStonePos w

------------------ Utility functions -----------------------------

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

isEqual :: Slot -> Slot -> Bool
isEqual Empty Empty = True
isEqual White White = True
isEqual Black Black = True
isEqual _ _ = False

-- Replaces an element in a list
replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

-- Replaces a slot on the board
replaceBoard :: World -> (Int , Int) -> Slot -> [[Slot]]
replaceBoard w (x,y) s = replace y (replace x s (board w !! y)) $ board w

-- Inserts a given character t times for every n characters in a string
insertEveryN :: Int ->  Int -> Char -> [Char] -> [Char]
insertEveryN 0 _ _ xs = xs
insertEveryN _ _ _ [] = []
insertEveryN n t y xs
 | DL.length xs < n = xs
 | t < 1 = xs
 | otherwise = take n xs ++ (concatMap (replicate t) [y]) ++ insertEveryN n t y (drop n xs)

-- Inserts an element at a given index in a list
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

-- Converts from Just Int to Int
fromJust :: Maybe Int -> Int
fromJust (Just x) = x
fromJust Nothing = -1

-- Convert Slot to Char for writing to file
slotToChar :: Slot -> String
slotToChar Empty = "E"
slotToChar Black = "B"
slotToChar White = "W"

charToSlot :: Char -> Slot
charToSlot 'W' = White
charToSlot 'B' = Black
charToSlot _ = Empty
