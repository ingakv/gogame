module Common (
    withSDL
  , withSDLImage
  , withRenderer
  , withWindow
  , mkPoint
  , mkRect
  , rectMoveTo
  , setHintQuality
  , loadTextureWithInfo
) where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)

-- State assignment operator
import SDL (($=))

-- Sets SDL library and executes an operation in SDL context.
-- Clears SDL when operation finishes.
withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []  -- Initializes SDL with default flags
  void op            -- Executes the operation provided as an argument
  SDL.quit           -- Cleans up SDL when operation finishes

-- Sets SDL.Image library and executes any given operation in the context.
withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []   -- Initializes the SDL.Image library
  SDL.Font.initialize       -- Initializes the SDL.Font library
  void op                   -- Executes the operation provided as an argument
  SDL.Font.quit             -- Quits the SDL.Font library
  SDL.Image.quit            -- Quits the SDL.Image library

-- Creates an SDL Window with the specified title and dimensions (x, y).
-- Runs the provided operation with the window and cleans up afterward.
withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p      -- Creates a new SDL Window with the specified title and size
  SDL.showWindow w                   -- Makes the window visible
  void $ op w                        -- Executes the operation provided, passing the window to it
  SDL.destroyWindow w                -- Destroys the window after operation is done
  where
    p = SDL.defaultWindow { SDL.windowInitialSize = z }
    z = SDL.V2 (fromIntegral x) (fromIntegral y)

-- Given SDL window, it sets up render and uses it to execute rendering function,
-- Cleans up the renderer when done.
withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig  -- Creates a renderer for the window with specified config
  void $ op r                                    -- Executes the provided rendering operation
  SDL.destroyRenderer r                          -- Destroys the renderer after use

-- Configuration for the SDL renderer
rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer   -- Use hardware acceleration and VSync
  , SDL.rendererTargetTexture = False                 -- Don't render to a target texture
  }

-- Sets SDL.HintRenderScaleQuality
setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest

-- Loads the texture with info as a tuple.
loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p  -- Loads the texture from the file path
  i <- SDL.queryTexture t         -- Retrieves the texture's information (e.g., width, height)
  pure (t, i)                     -- Returns the texture and its information as a tuple

-- Makes SDL point out of x y
mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)

-- Makes SDL rectangle out of x y width and height
mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)  -- The top-left corner of the rectangle
    z = SDL.V2 w h          -- The width and height of the rectangle

-- Moves SDL.Rectangle to a new position given by tuple (x, y).
-- Rectangle dimensions stay as before.
rectMoveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
rectMoveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (mkPoint x y) d
