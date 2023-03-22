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


windowSize :: (Int, Int)
windowSize = (1000, 700)

data Intent
  = Idle
  | Quit
  | Press Quadrant
  | Release Quadrant
  | Hover Quadrant
  | Leave Quadrant


data World = World
  { exiting :: Bool
  , width   :: Int
  , height  :: Int
  , slots   :: PaneMap
  }



data PaneMap = PaneMap
  { topLeft     :: Pane
  , topRight    :: Pane
  , bottomLeft  :: Pane
  , bottomRight :: Pane
  }


data Pane
  = Out
  | Over
  | Down
  | Up


data Quadrant
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight


initialWorld :: World
initialWorld = World
  { exiting = False
  , width = 840
  , Lib.height = 480
  , slots = initialPanes
  }


initialPanes :: PaneMap
initialPanes = PaneMap
  { topLeft     = Out
  , topRight    = Out
  , bottomLeft  = Out
  , bottomRight = Out
  }

gray :: SDL.Font.Color
gray = SDL.V4 128 25 25 255




-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp w =
    C.withRenderer w $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/background.png"

      -- we create an utility curry for us here
      let doRender = Lib.renderWorld r t

      void $ iterateUntilM
        Lib.exiting
        (\xw ->
             SDL.pollEvents >>= (\xw' -> xw' <$ doRender xw') . updateWorld xw
        )
        Lib.initialWorld

      -- when we are done with the renderer, we need to clean up
      SDL.destroyTexture (fst t)


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


  -- | SDL.mouseButtonEventMotion e == SDL.Pressed -> Down
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


updatePaneMap :: (Pane -> Pane) -> (Pane -> Pane) -> Quadrant -> PaneMap -> PaneMap
updatePaneMap f g TopLeft     (PaneMap tl tr bl br) = PaneMap (f tl) (g tr) (g bl) (g br)
updatePaneMap f g TopRight    (PaneMap tl tr bl br) = PaneMap (g tl) (f tr) (g bl) (g br)
updatePaneMap f g BottomLeft  (PaneMap tl tr bl br) = PaneMap (g tl) (g tr) (f bl) (g br)
updatePaneMap f g BottomRight (PaneMap tl tr bl br) = PaneMap (g tl) (g tr) (g bl) (f br)


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


setOut :: Pane -> Pane
setOut _ = Out


setOver :: Pane -> Pane
setOver Down = Down
setOver Up = Up
setOver _ = Over


setDown :: Pane -> Pane
setDown _ = Down


setUp :: Pane -> Pane
setUp _ = Up


idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }

-- Given the renderer, and the texture and the state of the World,
-- we can render the world. Note that the rendering results in an IO action.
-- This is a wrapper method that clears the rendering target, draws in the window,
-- and swaps the contexts. The actual drawing is done in drawWorld below.
renderWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
renderWorld r t w = do
  SDL.clear r
  drawBackground r t windowSize
  drawWorld r t w
  SDL.present r


-- Draw text
drawText :: SDL.Renderer -> Text -> (Int, Int) -> IO ()
drawText r t (x, y) = do
    font <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" 20
    textSurf <- SDL.Font.solid font gray t
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

  bt <- C.loadTextureWithInfo r "./assets/wood.png"
  drawBoard r bt

  drawText r letters (70, 25)
  drawText r letters (70, (snd windowSize) - 50)
  printNumbers 19 25
  printNumbers 19 $ (fst windowSize) - 50

  where
    letters :: Text
    letters = (pack $ insertEveryN 1 7 ' ' $ takeWhile (/= (['A'..'Z'] !! 19)) ['A'..'Z'])

    printNumbers :: Int -> Int -> IO ()
    printNumbers n posx = do
      drawText r (pack $ show n) (posx, 30+(31*n))
      if elem n [2..19]
       then do printNumbers (n-1) posx
      else pure()



-- Draw an empty board with texture
drawBoard :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> IO ()
drawBoard r (t, ti) = do

  SDL.copy r t board board

  where
    margin = fromIntegral 70
    w = fromIntegral (fst windowSize) - margin*2
    h = fromIntegral (snd windowSize) - margin*2
    board = (Just $ C.mkRect margin margin w h)





-- The actual method for drawing that is used by the rendering method above.
drawBackground :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawBackground r (t, ti) (winWidth, winHeight) = do
  -- Get the size of the texture, and we scale it down for a better effect
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


-- Inserts a given character t times for every n characters provided in the string
insertEveryN :: Int ->  Int -> Char -> [Char] -> [Char]
insertEveryN 0 t y xs = xs
insertEveryN n t y [] = []
insertEveryN n t y xs
 | length xs < n = xs
 | t < 1 = xs
 | otherwise = take n xs ++ (concatMap (replicate t) [y]) ++ insertEveryN n t y (drop n xs)


