{-# LANGUAGE OverloadedStrings #-}

module Lib (
    mainApp
  , windowSize
) where

import qualified SDL
import qualified Common as C
import Lib2

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
  , panes   :: PaneMap
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
  , panes = initialPanes
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
      t <- C.loadTextureWithInfo r "./assets/wood.png"

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
pressWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setDown id q (panes w)


releaseWorld :: Quadrant -> World -> World
releaseWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setUp id q (panes w)


hoverWorld :: Quadrant -> World -> World
hoverWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setOver setOut q (panes w)


leaveWorld :: Quadrant -> World -> World
leaveWorld q w = w { panes = panes' }
  where panes' = updatePaneMap setOut setOver q (panes w)


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
  drawBoard r
  drawText r letters (70, 25)
  drawText r letters (70, (snd windowSize) - 50)
  printNumbers 19

  where
    letters :: Text
    letters = (pack $ insertEveryN 1 7 ' ' $ takeWhile (/= (['A'..'Z'] !! 19)) ['A'..'Z'])

    printNumbers :: Int -> IO ()
    printNumbers n = do
      drawText r (pack $ show n) (25, 60+(29*n))
      if elem n [1..19]
       then do printNumbers (n-1)
      else pure()



-- Draw an empty board
drawBoard :: SDL.Renderer -> IO ()
drawBoard r = do
  SDL.drawRect r (Just $ C.mkRect margin margin w h)
  where
    margin = fromIntegral 70
    w = fromIntegral (fst windowSize) - margin*2
    h = fromIntegral (snd windowSize) - margin*2




-- The actual method for drawing that is used by the rendering method above.
drawBackground :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawBackground r (t, ti) (winWidth, winHeight) = do
  -- Get the size of the texture, and we scale it down for a better effect
  -- the original wood.png file is "too big"
  let texHeight = SDL.textureHeight ti `div` 3
  let texWidth = SDL.textureWidth ti `div` 3

  -- Loop and draw the tiled texture
  let loop x y
          | y >= winHeight = return ()
          | x >= winWidth = loop 0 (y + fromIntegral texHeight)
          | otherwise = do
              SDL.copy r t Nothing (Just $ C.mkRect (fromIntegral x) (fromIntegral y) texWidth texHeight)
              loop (x + fromIntegral texWidth) y

  loop 0 0




-- Converts Pane to the texture coordinates.
getMask :: (Num a) => Pane -> (a, a)
getMask Out  = (  0,   0)
getMask Over = (320,   0)
getMask Down = (  0, 240)
getMask Up   = (320, 240)


