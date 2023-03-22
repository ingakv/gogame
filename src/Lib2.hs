{-# LANGUAGE OverloadedStrings #-}

module Lib2
    ( boardSize
    , playGame
    , displayBoard
    ) where


import Data.List
import Data.List.Split
import Data.Char


-- Create the board based on a given size
createBoard :: Int -> Int -> Int -> [String] -> [[String]]
createBoard x y size li = do
  let finalList = chunksOf size li
  if x > 0
  then do
      createBoard (x-1) y size (insert ("-") li)
  else if y > 1
    then do
      createBoard size (y-1) size li
    else
      finalList


boardSize :: Int -> [[String]]
boardSize size =
  createBoard size size size []

-- Makes the board printable
formatBoard :: [[String]] -> String
formatBoard board = unlines $ map(\\"\t|") $ map (concatMap ("\t|   "++)) board


-- Returns the numbers on the board
boardNumbers :: Int -> [[String]] -> [[String]]
boardNumbers nr board = do
  let len = length (board !! 0)
  let row = insertAt (show (nr+1)) (length (board !! 0)+1) (insertAt (show (nr+1)) 0 (board !! 0))
  let newBoard = insertAt row len (tail board)

  if nr < len-1
  then do
      boardNumbers (nr+1) newBoard
  else newBoard



-- Display the board in the terminal
displayBoard :: [[String]] -> IO ()
displayBoard board = do
  let letters = takeWhile (/= (['A'..'Z'] !! (length (board !! 0)))) ['A'..'Z']
  let letterArr = (unwords $ map (" |  "++) (map (show) (letters)))

  putStrLn $ ""
  putStr $ "       " ++ letterArr ++ "  |\n"
  putStrLn $ " -----------------------------------------------------------------------------------------------------------------------------------------------------------------------"
  putStr $ formatBoard $ boardNumbers 0 board
  putStrLn $ " -----------------------------------------------------------------------------------------------------------------------------------------------------------------------"
  putStr $ "       " ++ letterArr ++ "  |\n"


fromJust :: Maybe Int -> Int
fromJust (Just x) = x

placeMarker :: Char -> Int -> [[String]] -> [[String]]
placeMarker column row board
  | isDigit column = board
  | otherwise = insertAt (insertAt ("X") (fromJust(elemIndex (toUpper column) ['A'..'Z'])) (delete ((board !! (fromJust(elemIndex (toUpper column) ['A'..'Z'])-1)) !! (row-1)) (board !! (row-1)))) (row-1) (delete (board !! (row-1)) ((board)))


playGame :: [[String]] -> IO ()
playGame board = do

  x <- getLine

  let newBoard = placeMarker (x !! 0) 5 board

  displayBoard newBoard

  playGame newBoard


insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as
