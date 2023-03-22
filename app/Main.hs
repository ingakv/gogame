{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common as C
import qualified Lib

main :: IO ()

main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Assignment 1 - Go" Lib.windowSize $ Lib.mainApp


  -- Making the board --
  --let board = boardSize 9

  --playGame board
