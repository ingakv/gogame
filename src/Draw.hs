{-# LANGUAGE OverloadedStrings #-}

module Draw (
  mainApp
) where


import DataTypes as DT
import qualified Lib


import qualified SDL
import qualified Common as C

import Data.Text        (Text, pack)
import Control.Monad.Loops    (iterateUntilM)

import SDL.Font




-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything Empty and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp w =
    C.withRenderer w $ \r -> do

      t1 <- C.loadTextureWithInfo r "./assets/background.png"
      t2 <- C.loadTextureWithInfo r "./assets/wood.png"
      t3 <- C.loadTextureWithInfo r "./assets/white_marker.png"
      t4 <- C.loadTextureWithInfo r "./assets/black_marker.png"
      t5 <- C.loadTextureWithInfo r "./assets/white_marker_hover.png"
      t6 <- C.loadTextureWithInfo r "./assets/black_marker_hover.png"

      let t = [t1,t2,t3,t4,t5,t6]

      -- we create an utility curry for us here
      let doRender = Draw.renderWorld r

--      void $ iterateUntilM
      _ <- iterateUntilM
        DT.exiting
        (\xw ->
             SDL.pollEvents >>= (\xw' -> xw' <$ doRender xw') . Lib.updateWorld xw
        )
        $ Lib.initialWorld t

      -- when we are done with the renderer, we need to clean up
      SDL.destroyTexture (fst t1)
      SDL.destroyTexture (fst t2)
      SDL.destroyTexture (fst t3)
      SDL.destroyTexture (fst t4)
      SDL.destroyTexture (fst t5)
      SDL.destroyTexture (fst t6)


-- Given the renderer, and the texture and the state of the World,
-- we can render the world. Note that the rendering results in an IO action.
-- This is a wrapper method that clears the rendering target, draws in the window,
-- and swaps the contexts. The actual drawing is done in drawWorld below.
renderWorld :: SDL.Renderer -> World -> IO ()
renderWorld r w = do
  SDL.clear r
  drawWorld r (textures w) w
  SDL.present r




------------- DRAW FUNCTIONS ----------------------


-- Draw text
drawText :: SDL.Renderer -> Text -> (Int, Int) -> IO ()
drawText r t (x, y) = do
    font <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" 14
    textSurf <- SDL.Font.solid font Lib.textColor t
    surf <- SDL.createTextureFromSurface r textSurf
    info <- SDL.queryTexture surf
    let w = SDL.textureWidth info
    let h = SDL.textureHeight info
    SDL.copy r surf Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 w h))
    SDL.freeSurface textSurf
    SDL.destroyTexture surf



-- The actual method for drawing that is used by the rendering method above.
drawWorld :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
drawWorld r t w = do

  drawBackground r (t !! 0) Lib.windowSize

  drawBoard r (t !! 1)

  drawLines' 0

  checkBoard 0 0

  drawText r letters (70, 15)
  drawText r letters (70, (snd Lib.windowSize) - 50)
  printNumbers Lib.boardSize 25
  printNumbers Lib.boardSize $ 730

  drawText r "White groups:" (750,0)
  drawText r (pack $ show $ whiteGroups w) (750,20)
  drawText r (pack $ show $ whiteMarkerPos w) (750,50)
  drawText r "Black groups:" (750,100)
  drawText r (pack $ show $ blackGroups w) (750,120)
  drawText r (pack $ show $ blackMarkerPos w) (750,150)


  drawText r "Press Q to Quit" (800,150)
  drawText r "Press S to Skip turn" (800,200)

  drawText r (pack $ show $ (length $ whiteGroups w)) (0,150)


  -- Hover marker
  checkMouse


  where


    -- Checks if the mouse is hovering over a slot
    checkMouse :: IO ()
    checkMouse = do

      let inters = Lib.intersect' w

      if (fst inters) >= 0
      then
        -- Draws the hover marker in the correct color
        if Lib.isBlack $ curColor w
          then do drawMarker r (t !! 5) (inters)
        else drawMarker r (t !! 4) (inters)
      else pure()



    letters :: Text
    letters = (pack $ Lib.insertEveryN 11 1 ' ' $ Lib.insertEveryN 1 8 ' ' $ takeWhile (/= (['A'..'Z'] !! Lib.boardSize)) ['A'..'Z'])

    printNumbers :: Int -> Int -> IO ()
    printNumbers n posx = do
      drawText r (pack $ show n) (posx, 10+(35*n))
      if elem n [2..Lib.boardSize]
       then do printNumbers (n-1) posx
      else pure()


  -- Draw the lines where the markers are to be placed along
    drawLines' :: Int -> IO ()
    drawLines' n = do
      horLine r n
      if elem n [0..Lib.boardSize-2]
       then do  drawLines' (n+1)
      else drawVerLines 0

    drawVerLines :: Int -> IO ()
    drawVerLines n = do
      verLine r n
      if elem n [0..Lib.boardSize-2]
       then do  drawVerLines (n+1)
      else pure()

    -- Checks if a slot is empty, and draws a marker in that spot if it isn't
    checkBoard :: Int -> Int -> IO ()
    checkBoard x y = do
      if (Lib.isWhite ((board w !! x) !! y))
        then do drawMarker r (t !! 2) (Lib.markerPos x y)
      else if (Lib.isBlack ((board w !! x) !! y))
        then do drawMarker r (t !! 3) (Lib.markerPos x y)
      else pure()

      if x < (Lib.boardSize-1)
      then do
          checkBoard (x+1) y
      else if y < (Lib.boardSize-1)
        then do
          checkBoard 0 (y+1)
        else pure()



-- Draws a line between two points that are computed based on the number n:

-- Horisontal lines
horLine :: SDL.Renderer -> Int -> IO ()
horLine r n = do
  SDL.drawLine r (C.mkPoint x ay) (C.mkPoint x by)
    where
      x = fromIntegral $ 75 + n*35
      ay = 50
      by = fromIntegral $ 60 + (Lib.boardSize-1)*35

-- Vertical lines
verLine :: SDL.Renderer -> Int -> IO ()
verLine r n = do
  SDL.drawLine r (C.mkPoint ax y) (C.mkPoint bx y)
    where
      ax = 70
      bx = fromIntegral $ 80 + (Lib.boardSize-1)*35
      y = fromIntegral $ 55 + n*35




-- Draw an empty board with texture
drawBoard :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> IO ()
drawBoard r (t, ti) = do
  SDL.copy r t boardTexture board'
  where
    marginx = 70
    marginy = 50
    s = fromIntegral (Lib.boardSize-1)*35 + 11
    boardTexture = (Just $ C.mkRect 0 0 (SDL.textureWidth ti) (SDL.textureHeight ti))
    board' = (Just $ C.mkRect marginx marginy s s)



-- Draw a singular marker with texture
drawMarker :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawMarker r (t, ti) (px, py) = do
  SDL.copy r t markerTexture marker
  where
    posx = fromIntegral px
    posy = fromIntegral py
    d = 20
    markerTexture = (Just $ C.mkRect 0 0 (SDL.textureWidth ti) (SDL.textureHeight ti))
    marker = (Just $ C.mkRect posx posy d d)




-- The actual method for drawing that is used by the rendering method above.
drawBackground :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawBackground r (t, ti) (winWidth, winHeight) = do
  -- Get the size of the texture, and we scale it Empty for a better effect
  let texHeight = SDL.textureHeight ti
  let texWidth = SDL.textureWidth ti

  -- Loop and draw the tiled texture
  let loop x y
          | y >= winHeight = return ()
          | x >= winWidth = loop 0 (y + fromIntegral texHeight)
          | otherwise = do
              SDL.copy r t Nothing (Just $ C.mkRect (fromIntegral x) (fromIntegral y) texWidth texHeight)
              loop (x + fromIntegral texWidth) y

  loop 0 0


