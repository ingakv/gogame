{-# LANGUAGE OverloadedStrings #-}

module Lib2
    ( boardSize
    , playGame
    , displayBoard
    ) where


import Data.List
import Data.List.Split
import Data.Char


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



playGame :: [[String]] -> IO ()
playGame board = do

  x <- getLine

  let newBoard = placeMarker (x !! 0) 5 board

  displayBoard newBoard

  playGame newBoard


