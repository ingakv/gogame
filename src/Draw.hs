{-# LANGUAGE OverloadedStrings #-}

module Draw (
  mainApp
) where


import DataTypes as DT
import qualified Lib
import qualified GameLogic as GL

import qualified SDL
import qualified Common as C

import Data.Text        (Text, pack)
import Control.Monad          (void)
import Control.Monad.Loops    (iterateUntilM)

import SDL.Font

-- Main entry point to the application logic.
-- It initializes the SDL renderer, loads textures and fonts, and starts the main app loop.
mainApp :: [[Slot]] -> SDL.Window -> IO ()
mainApp b w =
    C.withRenderer w $ \r -> do

      -- Load the textures
      t1 <- C.loadTextureWithInfo r "./assets/background.png"
      t2 <- C.loadTextureWithInfo r "./assets/wood.png"
      t3 <- C.loadTextureWithInfo r "./assets/white_stone.png"
      t4 <- C.loadTextureWithInfo r "./assets/black_stone.png"
      t5 <- C.loadTextureWithInfo r "./assets/white_stone_hover.png"
      t6 <- C.loadTextureWithInfo r "./assets/black_stone_hover.png"

      let t = [t1,t2,t3,t4,t5,t6]

      -- Load font for rendering text
      f <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" 14

      let doRender = Draw.renderWorld r

      -- Main loop: handle user input and update the world state.
      void $ iterateUntilM
        DT.exiting
        (\xw ->
             SDL.pollEvents >>= (\xw' -> xw' <$ doRender xw') . Lib.updateWorld xw
        )
        (Lib.initialWorld t f b)

      -- Clean up resources after exiting the loop
      Draw.destroyTextures t

-- Function to destroy loaded textures after use
destroyTextures :: [(SDL.Texture, SDL.TextureInfo)] -> IO()
destroyTextures t = mapM_ SDL.destroyTexture (map fst t)

-- Function to render the world. It clears the renderer, draws the world, and swaps buffers.
renderWorld :: SDL.Renderer -> World -> IO ()
renderWorld r w = do
  SDL.clear r
  drawWorld r (textures w) w
  SDL.present r



------------- DRAW FUNCTIONS ----------------------

-- Draw text
drawText :: SDL.Renderer -> World -> Text -> (Int, Int) -> IO ()
drawText r world t (x, y) = do
    textSurf <- SDL.Font.solid (font world) Lib.textColor t
    surf <- SDL.createTextureFromSurface r textSurf
    info <- SDL.queryTexture surf
    let w = SDL.textureWidth info
    let h = SDL.textureHeight info
    SDL.copy r surf Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 w h))
    SDL.freeSurface textSurf
    SDL.destroyTexture surf

-- Main drawing function that handles different elements on the screen.
drawWorld :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
drawWorld r t w = do

  -- The background
  drawBackground r (t !! 0) Lib.windowSize

  -- The wooden board
  drawBoard r (t !! 1)

  -- The lines on the board
  drawLines r 0

  -- Draws the stones currently on the board
  helperCheckBoard r [t!!2,t!!3] w

  -- Text
  drawUI r w

  -- Hover stone
  checkMouse

  where
    -- Checks if the mouse is hovering over a slot
    checkMouse :: IO ()
    checkMouse = do
      let inters = Lib.intersect' w
      if (fst inters) >= 0
      then
        -- Draw the hover stone in the correct color
        if GL.isBlack $ curColor w
          then do drawStone r (t !! 5) (inters)
        else drawStone r (t !! 4) (inters)
      else pure()

-- Function to render UI components
drawUI :: SDL.Renderer -> World -> IO ()
drawUI r w = do

  -- Draw letters and numbers along the edge of the board
  drawText r w letters (70, 15)
  drawText r w letters (70, (snd Lib.windowSize) - 50)
  printNumbers GL.boardSize 25
  printNumbers GL.boardSize $ 730

  -- Draw game state info
  SDL.rendererDrawColor r SDL.$= SDL.V4 125 155 155 255
  SDL.drawLine r (C.mkPoint p4x $ fromIntegral p3y-10) (C.mkPoint p4x $ fromIntegral p1y+25)

  drawText r w "White" (p2x,p2y)
  drawText r w (pack $ show $ length $ whiteGroups w) (p3x,p3y)
  drawText r w (pack $ show $ length $ whiteFree w) (p3x,p1y)

  drawText r w "Black" (p2x+80,p2y)
  drawText r w (pack $ show $ length $ blackGroups w) (p3x+80,p3y)
  drawText r w (pack $ show $ length $ blackFree w) (p3x+80,p1y)


  drawText r w "Number of" (p1x,p3y-8)
  drawText r w "groups" (p1x+10,p3y+7)

  drawText r w "Degree of" (p1x,p1y-8)
  drawText r w "freedom" (p1x+2,p1y+7)

  -- Draw shortcut instructions
  drawText r w "Press Q to Quit" (800,50)
  drawText r w "Press S to Skip turn" (800,100)
  drawText r w "Press C to Clear the board" (800,150)

    where
      -- Text placement coordinates
      p1x = 750
      p1y = p2y+100

      p2x = p1x+100
      p2y = 280

      p3x = p2x+10
      p3y = p2y+35

      p4x = fromIntegral (fst Lib.windowSize)-252


      letters :: Text
      letters = (pack $ GL.insertEveryN 11 1 ' ' $ GL.insertEveryN 1 8 ' ' $ takeWhile (/= (['A'..'Z'] !! GL.boardSize)) ['A'..'Z'])

      -- Print numbers on the board edges
      printNumbers :: Int -> Int -> IO ()
      printNumbers n posx = do
        drawText r w (pack $ show n) (posx, 10+(35*n))
        if elem n [2..GL.boardSize]
         then do printNumbers (n-1) posx
        else pure()



-- Draw a line between two points that are computed based on the number n

-- Horisontal lines
horLine :: SDL.Renderer -> Int -> IO ()
horLine r n = do
  SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 255
  SDL.drawLine r (C.mkPoint x ay) (C.mkPoint x by)
    where
      x = fromIntegral $ 75 + n*35
      ay = 50
      by = fromIntegral $ 60 + (GL.boardSize-1)*35

-- Vertical lines
verLine :: SDL.Renderer -> Int -> IO ()
verLine r n = do
  SDL.drawLine r (C.mkPoint ax y) (C.mkPoint bx y)
    where
      ax = 70
      bx = fromIntegral $ 80 + (GL.boardSize-1)*35
      y = fromIntegral $ 55 + n*35


-- Draw the lines where the stones are to be placed along
drawLines :: SDL.Renderer -> Int -> IO ()
drawLines r n = do
  horLine r n
  if elem n [0..GL.boardSize-2]
   then do  drawLines r (n+1)
  else drawVerLines r 0

drawVerLines :: SDL.Renderer -> Int -> IO ()
drawVerLines r n = do
  verLine r n
  if elem n [0..GL.boardSize-2]
   then do  drawVerLines r (n+1)
  else pure()


-- Check if a slot is empty, and draw a stone in that spot if it isn't
helperCheckBoard :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
helperCheckBoard r tx w = checkBoard r tx w 0 0

checkBoard :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> Int -> Int -> IO ()
checkBoard r tx w x y = do

  -- Draws the stone in the correct color
  if (GL.isWhite ((board w !! x) !! y))
    then do drawStone r (tx !! 0) (Lib.stonePos x y)
  else if (GL.isBlack ((board w !! x) !! y))
    then do drawStone r (tx !! 1) (Lib.stonePos x y)
  else pure()

  -- Loops through the entire board and recursively draws all of the stones
  if x < (GL.boardSize-1)
  then do
      checkBoard r tx w (x+1) y
  else if y < (GL.boardSize-1)
    then do
      checkBoard r tx w 0 (y+1)
    else pure()


-- Draw an empty board with texture
drawBoard :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> IO ()
drawBoard r (t, ti) = do
  SDL.copy r t boardTexture board'
  where
    marginx = 70
    marginy = 50
    s = fromIntegral (GL.boardSize-1)*35 + 11
    boardTexture = (Just $ C.mkRect 0 0 (SDL.textureWidth ti) (SDL.textureHeight ti))
    board' = (Just $ C.mkRect marginx marginy s s)


-- Draw a singular stone with texture
drawStone :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawStone r (t, ti) (px, py) = do
  SDL.copy r t stoneTexture stone
  where
    posx = fromIntegral px
    posy = fromIntegral py
    d = 20
    stoneTexture = (Just $ C.mkRect 0 0 (SDL.textureWidth ti) (SDL.textureHeight ti))
    stone = (Just $ C.mkRect posx posy d d)


-- Draw the background
drawBackground :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawBackground r (t, ti) (winWidth, winHeight) = do

  let texHeight = SDL.textureHeight ti
  let texWidth = SDL.textureWidth ti


  let loop x y
          | y >= winHeight = return ()
          | x >= winWidth = loop 0 (y + fromIntegral texHeight)
          | otherwise = do
              SDL.copy r t Nothing (Just $ C.mkRect (fromIntegral x) (fromIntegral y) texWidth texHeight)
              loop (x + fromIntegral texWidth) y

  loop 0 0
