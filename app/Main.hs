{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common as C
import Lib (windowSize, initBoard)
import Draw (mainApp)
import GameLogic (boardSize)
import DataTypes (Slot (Empty, Black, White))

import System.IO
import Data.List.Split        (chunksOf)

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do


  handle <- openFile "game.sgf" ReadMode
  contents <- hGetContents handle

  let readBoard = chunksOf boardSize $ map charToSlot [c | c <- contents , elem c ['E' , 'W' , 'B']]

  -- Ensures that the board read from the sgf file is of the correct size
  let board = if (length $ concat readBoard) == (boardSize * boardSize) then readBoard else initBoard boardSize boardSize []

  C.setHintQuality
  C.withWindow "Assignment 1 - Go" windowSize $ mainApp $ board

  hClose handle


charToSlot :: Char -> Slot
charToSlot 'W' = White
charToSlot 'B' = Black
charToSlot _ = Empty
