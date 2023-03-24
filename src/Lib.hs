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
import Data.List        (intersect)


windowSize :: (Int, Int)
windowSize = (1000, 750)

boardSize :: Int
boardSize = 19

textColor :: SDL.Font.Color
textColor = SDL.V4 150 0 0 255



data Intent
  = Idle
  | MouseMoved (Int, Int)
  | Quit
  | Press Slot
  | Release Slot
  | Hover Slot
  | Leave Slot


data World = World
  { exiting :: Bool
  , slots   :: SlotMap
  , mouseCoords   :: (Int, Int)
  , slotMap :: [[Slot]]
  , textures :: [(SDL.Texture, SDL.TextureInfo)]
  , mPos :: [(Int, Int)]
  }


-- Create an empty board based on a given size
initBoard :: Int -> Int -> Int -> [Slot] -> [[Slot]]
initBoard x y size li = do
  let finalList = chunksOf size li
  if x > 0
  then do
      initBoard (x-1) y size (insertAt Black 0 li)
  else if y > 1
    then do
      initBoard size (y-1) size li
    else
      finalList


data Slot
  = Empty
  | White
  | Black




data SlotMap = SlotMap
  { topLeft    :: Slot
  }



markerPos :: Int -> Int -> (Int, Int)
markerPos x y = (65 + 35*x, 45 + 35*y)



-- An array of all marker positions
allMarkerPos :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
allMarkerPos x y li = do
  if x > 0
  then do
      allMarkerPos (x-1) y (insertAt (markerPos x y) 0 li)
  else if y > 1
    then do
      allMarkerPos boardSize (y-1) li
    else
      li





initialWorld :: World
initialWorld = World
  { exiting = False
  , slotMap = initBoard boardSize boardSize boardSize []
  , mouseCoords = (0,0)
  , slots = initialSlots
  , textures = []
  , mPos = allMarkerPos boardSize boardSize []
  }


initialSlots :: SlotMap
initialSlots = SlotMap
  { topLeft    = Empty }








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
      SDL.destroyTexture (fst t4)


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
--payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle



-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = (MouseMoved (fromIntegral x, fromIntegral y))
  where
    q = selectTopLeft x y
    -- observe clever use of pattern matching to get x and y from the event!
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = t q
  where
    q = selectTopLeft x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
    t = if SDL.mouseButtonEventMotion e == SDL.Pressed
           then Press
           else Release


selectTopLeft :: (Num a, Ord a) => a -> a -> Slot
selectTopLeft x y
  | x >= 3200 && y <  2400 = Empty
  | otherwise            = undefined

applyIntent :: Intent -> World -> World
--applyIntent (Press q)   = pressWorld q
--applyIntent (Release q) = releaseWorld q
--applyIntent (Hover q)   = hoverWorld q
--applyIntent (Leave q)   = leaveWorld q
applyIntent (MouseMoved coords)  = hoverWorld coords
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld

updateSlotMap :: (Slot -> Slot) -> Slot -> SlotMap -> SlotMap
updateSlotMap f _    (SlotMap tr) = SlotMap (f tr)

{-
pressWorld :: Slot -> World -> World
pressWorld q w = w { slots = slots' }
  where slots' = updateSlotMap setDown id q (slots w)


releaseWorld :: Slot -> World -> World
releaseWorld q w = w { slots = slots' }
  where slots' = updateSlotMap setUp id q (slots w)

leaveWorld :: Slot -> World -> World
leaveWorld q w = w { slots = slots' }
  where slots' = updateSlotMap setOut setOver q (slots w)


-}

hoverWorld :: (Int, Int) -> World -> World
hoverWorld coords w = w { mouseCoords = coords }







-- Checks whether a given slot is empty or not (occupied by a marker)
isEmpty :: Slot -> Bool
isEmpty Empty = True
isEmpty _ = False

isWhite :: Slot -> Bool
isWhite White = True
isWhite _ = False

isBlack :: Slot -> Bool
isBlack Black = True
isBlack _ = False


setEmpty :: Slot -> Slot
setEmpty _ = Empty
setOut :: Slot -> Slot
setOut _ = Empty

setDown :: Slot -> Slot
setDown _ = Empty

setUp :: Slot -> Slot
setUp _ = Empty


setOver :: Slot -> Slot
setOver _ = White





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
  drawWorld r t w
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



------------- DRAW FUNCTIONS ----------------------


-- The actual method for drawing that is used by the rendering method above.
drawWorld :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
drawWorld r t w = do


  drawBoard r (t !! 1)

  drawLines' 0

  checkBoard 0 0

  drawText r letters (70, 15)
  drawText r letters (70, (snd windowSize) - 50)
  printNumbers boardSize 25
  printNumbers boardSize $ 730



  -- Prints mouse coordinates
  drawText r (pack (show $ mouseCoords w)) (0,0)


  -- Hover marker
  checkMouse



  where

    checkMouse :: IO ()
    checkMouse = do
      let a = fst $ mouseCoords w
      let b = snd $ mouseCoords w

      let lix = [(a-20) .. (a)]
      let liy = [(b-20) .. (b)]

      let coordCombo = [ (x,y) | x <- lix, y <- liy ]


      drawText r (pack (show $ (coordCombo !! 0))) (0,50)

      let inters = intersect coordCombo $ mPos w

      if (length inters) > 0
      then do drawText r (pack (show $ (((inters) !! 0)))) (0,100)
      else pure()



      if elem (a-10, b-10) $ mPos w
        then do drawMarker r (t !! 3) (a-10, b-10)
      else pure()

    letters :: Text
    letters = (pack $ insertEveryN 11 1 ' ' $ insertEveryN 1 8 ' ' $ takeWhile (/= (['A'..'Z'] !! boardSize)) ['A'..'Z'])

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
      if (isWhite ((slotMap w !! x) !! y))
        then do drawMarker r (t !! 2) (markerPos x y)
      else if (isBlack ((slotMap w !! x) !! y))
        then do drawMarker r (t !! 3) (markerPos x y)
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
      ay = 50
      by = fromIntegral $ 60 + (boardSize-1)*35

-- Vertical lines
verLine :: SDL.Renderer -> Int -> IO ()
verLine r n = do
  SDL.drawLine r (C.mkPoint ax y) (C.mkPoint bx y)
    where
      ax = 70
      bx = fromIntegral $ 80 + (boardSize-1)*35
      y = fromIntegral $ 55 + n*35




-- Draw an empty board with texture
drawBoard :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> IO ()
drawBoard r (t, ti) = do
  SDL.copy r t boardTexture board
  where
    marginx = 70
    marginy = 50
    s = fromIntegral (boardSize-1)*35 + 11
    boardTexture = (Just $ C.mkRect 0 0 (SDL.textureWidth ti) (SDL.textureHeight ti))
    board = (Just $ C.mkRect marginx marginy s s)



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



------------------ A few small useful functions -----------------------------

-- Inserts a given character t times for every n characters provided in the string
insertEveryN :: Int ->  Int -> Char -> [Char] -> [Char]
insertEveryN 0 _ _ xs = xs
insertEveryN _ _ _ [] = []
insertEveryN n t y xs
 | length xs < n = xs
 | t < 1 = xs
 | otherwise = take n xs ++ (concatMap (replicate t) [y]) ++ insertEveryN n t y (drop n xs)



insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

