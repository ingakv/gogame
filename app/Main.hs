{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common as C
import Lib
import Draw

main :: IO ()

main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Assignment 1 - Go" windowSize $ mainApp


  -- Making the board --

  --playGame board
