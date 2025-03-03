{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common as C
import Lib (windowSize, loadFromFile, saveFilePath, tempFilePath)
import Draw (mainApp)

import System.IO
import System.Directory (removeFile)

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do

   -- Opens the sgf file and reads its contents
    withFile saveFilePath ReadMode $ \handle -> do
        board <- loadFromFile handle
        C.withWindow "Assignment 1 - Go" windowSize $ mainApp $ board



    -- Opens the temp sgf file and reads its contents
    handle <- openFile tempFilePath ReadMode
    contents <- hGetContents handle
    let boardString = contents

    -- Now reopen the save file for writing
    withFile saveFilePath WriteMode $ \saveHandle -> do
        hPutStrLn saveHandle boardString

    hClose handle

    removeFile tempFilePath
