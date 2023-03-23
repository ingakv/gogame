{-# LANGUAGE OverloadedStrings #-}

module Lib (
    mainApp
  , windowSize
) where

import qualified SDL
import qualified Common as C

import Control.Monad          (void)
import Control.Monad.Loops    (iterateUntilM)
import Data.Foldable          (foldl')

import Data.Text (Text, pack)
import SDL.Font


import Data.List.Split
import Data.List


windowSize :: (Int, Int)
windowSize = (1000, 750)

boardSize :: Int
boardSize = 19

data Intent
  = Idle
  | Quit
  | Press Quadrant
  | Release Quadrant
  | Hover Quadrant
  | Leave Quadrant


data World = World
  { exiting :: Bool
  , slots   :: PaneMap
  , slotMap :: [[Slot]]
  }


-- Create an empty board based on a given size
initBoard :: Int -> Int -> Int -> [Slot] -> [[Slot]]
initBoard x y size li = do
  let finalList = chunksOf size li
  if x > 0
  then do
      initBoard (x-1) y size (insertAt Empty 0 li)
  else if y > 1
    then do
      initBoard size (y-1) size li
    else
      finalList


data Slot
  = Empty
  | White
  | Black


data PaneMap = PaneMap
  { topLeft     :: Slot
  , topRight    :: Slot
  , bottomLeft  :: Slot
  , bottomRight :: Slot
  }



data Quadrant
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight


initialWorld :: World
initialWorld = World
  { exiting = False
  , slotMap = initBoard boardSize boardSize boardSize []
  , slots = initialPanes
  }



initialPanes :: PaneMap
initialPanes = PaneMap
  { topLeft     = Empty
  }

textColor :: SDL.Font.Color
textColor = SDL.V4 128 25 25 255




-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything Empty and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp w =
    C.withRenderer w $ \r -> do

      t1 <- C.loadTextureWithInfo r "./assets/background.png"
      t2 <- C.loadTextureWithInfo r "./assets/wood.png"
      t3 <- C.loadTextureWithInfo r "./assets/white_marker.png"

      let t = [t1,t2,t3]

      -- we create an utility curry for us here
      let doRender = Lib.renderWorld r t


      void $ iterateUntilM
        Lib.exiting
        (\xw ->
             SDL.pollEvents >>= (\xw' -> xw' <$ doRender xw') . updateWorld xw
        )
        Lib.initialWorld

      -- when we are done with the renderer, we need to clean up
      SDL.destroyTexture (fst t1)
      SDL.destroyTexture (fst t2)
      SDL.destroyTexture (fst t3)


-- Given a list of events, update the world
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w . fmap (payloadToIntent . SDL.eventPayload)


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else Idle
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle

-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = Hover q
  where
    q = selectQuadrant x y
    -- observe clever use of pattern matching to get x and y from the event!
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


  -- | SDL.mouseButtonEventMotion e == SDL.Pressed -> Empty
  --
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = t q
  where
    q = selectQuadrant x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
    t = if SDL.mouseButtonEventMotion e == SDL.Pressed
           then Press
           else Release


selectQuadrant :: (Num a, Ord a) => a -> a -> Quadrant
selectQuadrant x y
  | x <  320 && y <  240 = TopLeft
  | x >= 320 && y <  240 = TopRight
  | x <  320 && y >= 240 = BottomLeft
  | x >= 320 && y >= 240 = BottomRight
  | otherwise            = undefined


applyIntent :: Intent -> World -> World
applyIntent (Press q)   = pressWorld q
applyIntent (Release q) = releaseWorld q
applyIntent (Hover q)   = hoverWorld q
applyIntent (Leave q)   = leaveWorld q
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld


updatePaneMap :: (Slot -> Slot) -> (Slot -> Slot) -> Quadrant -> PaneMap -> PaneMap
updatePaneMap f g TopLeft     (PaneMap tl tr bl br) = PaneMap (f tl) (g tr) (g bl) (g br)


pressWorld :: Quadrant -> World -> World
pressWorld q w = w { slots = slots' }
  where slots' = updatePaneMap setDown id q (slots w)


releaseWorld :: Quadrant -> World -> World
releaseWorld q w = w { slots = slots' }
  where slots' = updatePaneMap setUp id q (slots w)


hoverWorld :: Quadrant -> World -> World
hoverWorld q w = w { slots = slots' }
  where slots' = updatePaneMap setOver setOut q (slots w)


leaveWorld :: Quadrant -> World -> World
leaveWorld q w = w { slots = slots' }
  where slots' = updatePaneMap setOut setOver q (slots w)


-- Checks whether a given slot is empty or not (occupied by a marker)
isEmpty :: Slot -> Bool
isEmpty Empty = True
isEmpty _ = False


setOut :: Slot -> Slot
setOut _ = Empty

setOver :: Slot -> Slot
setOver _ = Empty


setDown :: Slot -> Slot
setDown _ = Empty


setUp :: Slot -> Slot
setUp _ = Empty


idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }



-- Given the renderer, and the texture and the state of the World,
-- we can render the world. Note that the rendering results in an IO action.
-- This is a wrapper method that clears the rendering target, draws in the window,
-- and swaps the contexts. The actual drawing is done in drawWorld below.
renderWorld :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
renderWorld r t w = do
  SDL.clear r
  drawBackground r (t !! 0) windowSize
  drawWorld r (t !! 1) w
  SDL.present r


-- Draw text
drawText :: SDL.Renderer -> Text -> (Int, Int) -> IO ()
drawText r t (x, y) = do
    font <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" 14
    textSurf <- SDL.Font.solid font textColor t
    surf <- SDL.createTextureFromSurface r textSurf
    info <- SDL.queryTexture surf
    let w = SDL.textureWidth info
    let h = SDL.textureHeight info
    SDL.copy r surf Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 w h))
    SDL.freeSurface textSurf
    SDL.destroyTexture surf


-- The actual method for drawing that is used by the rendering method above.
drawWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
drawWorld r (t, ti) w = do

  drawBoard r (t, ti)

  drawLines' 0

  checkBoard 0 0

  drawText r letters (70, 15)
  drawText r letters (70, (snd windowSize) - 50)
  printNumbers boardSize 25
  printNumbers boardSize $ 730

  where
    letters :: Text
    letters = (pack $ insertEveryN 12 1 ' ' $ insertEveryN 1 8 ' ' $ takeWhile (/= (['A'..'Z'] !! boardSize)) ['A'..'Z'])

    printNumbers :: Int -> Int -> IO ()
    printNumbers n posx = do
      drawText r (pack $ show n) (posx, 10+(35*n))
      if elem n [2..boardSize]
       then do printNumbers (n-1) posx
      else pure()


  -- Draw the lines where the markers are to be placed along
    drawLines' :: Int -> IO ()
    drawLines' n = do
      horLine r n
      if elem n [0..boardSize-2]
       then do  drawLines' (n+1)
      else drawVerLines 0

    drawVerLines :: Int -> IO ()
    drawVerLines n = do
      verLine r n
      if elem n [0..boardSize-2]
       then do  drawVerLines (n+1)
      else pure()

    -- Checks if a slot is empty, and draws a marker in that spot if it isn't
    checkBoard :: Int -> Int -> IO ()
    checkBoard x y = do
      if not (isEmpty ((slotMap w !! x) !! y))
        then do drawMarker r (t, ti) (67 + 47*x, 60 + 31*y)
      else pure()

      if x < (boardSize-1)
      then do
          checkBoard (x+1) y
      else if y < (boardSize-1)
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
      ay = fromIntegral $ 50
      by = fromIntegral $ 60 + (boardSize-1)*35

-- Vertical lines
verLine :: SDL.Renderer -> Int -> IO ()
verLine r n = do
  SDL.drawLine r (C.mkPoint ax y) (C.mkPoint bx y)
    where
      ax = fromIntegral $ 70
      bx = fromIntegral $ 80 + (boardSize-1)*35
      y = fromIntegral $ 55 + n*35




-- Draw an empty board with texture
drawBoard :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> IO ()
drawBoard r (t, ti) = do
  SDL.copy r t boardTexture board
  where
    marginx = fromIntegral 70
    marginy = fromIntegral 50
    size = fromIntegral (boardSize-1)*35 + 11
    boardTexture = (Just $ C.mkRect 0 0 (SDL.textureWidth ti) (SDL.textureHeight ti))
    board = (Just $ C.mkRect marginx marginy size size)



-- Draw a singular marker with texture
drawMarker :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawMarker r (t, ti) (px, py) = do
  SDL.copy r t markerTexture marker
  where
    posx = fromIntegral px
    posy = fromIntegral py
    d = 25
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



-- A few useful functions:

-- Inserts a given character t times for every n characters provided in the string
insertEveryN :: Int ->  Int -> Char -> [Char] -> [Char]
insertEveryN 0 t y xs = xs
insertEveryN n t y [] = []
insertEveryN n t y xs
 | length xs < n = xs
 | t < 1 = xs
 | otherwise = take n xs ++ (concatMap (replicate t) [y]) ++ insertEveryN n t y (drop n xs)



insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

