{-
Copyright 2012 Alexander Midgley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts, BangPatterns, QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import qualified Data.Array.Repa as A
import Data.Array.Repa ((:.) (..))
import qualified Data.Array.Repa.Stencil as A
import Data.Array.Repa.Stencil.Dim2
import Data.Bits
import Data.Maybe (isJust)
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (catch)
import System.Environment

import LifePattern
import PatternParsers

type Grid = A.Array A.U A.DIM2 Word8

main = do
  args <- getArgs
  case args of
    [size, pattern] -> run (read size) pattern Nothing
    [size, pattern, generations] -> run (read size) pattern (Just $ read generations)
    _ -> putStrLn "Usage: repa-life <grid size> <pattern file> [generations]"

run :: Int -> FilePath -> Maybe Int -> IO ()
run gridSize pattern generations = do
  parseResult <- parseFile pattern
  case parseResult of
    SuccessfulParse pattern' -> startSimulation gridSize pattern' generations
    err -> putStrLn $ "Error in pattern file: " ++ show err

startSimulation :: Int -> LifePattern -> Maybe Int -> IO ()
startSimulation gridSize pattern generations = do
  -- initialize has to come first. If it doesn't return True,
  -- this crashes with a pattern match error.
  True <- GLFW.initialize

  let winSize = 2 * gridSize
  
  -- Set the RGB bits to get a color window. See the GLFW-b docs for all the options
  True <- GLFW.openWindow GLFW.defaultDisplayOptions
          { GLFW.displayOptions_numRedBits = 8
          , GLFW.displayOptions_numGreenBits = 8
          , GLFW.displayOptions_numBlueBits = 8
          , GLFW.displayOptions_width = winSize
          , GLFW.displayOptions_height = winSize
          }

  GLFW.setWindowSizeCallback $ resize gridSize

  [tex] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D $= Just tex
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.texture GL.Texture2D $= GL.Enabled
  forM_ [GL.S, GL.T, GL.R] $ \coord ->
      GL.textureWrapMode GL.Texture2D coord $= (GL.Repeated, GL.Clamp)

  -- Use `finally` so that `quit` is called whether or not `mainLoop` throws an exception
  finally (mainLoop (freshGrid gridSize pattern) generations False) quit

-- | Resize the viewport and set the projection matrix
resize :: (Integral n1, Integral n2) => n1 -> n2 -> n2 -> IO ()
resize gridSize w h = do
  -- These are all analogous to the standard OpenGL functions
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho left right bottom top 0 1
  GL.matrixMode $= GL.Modelview 0

    where
      (left, right, bottom, top) =
          let aspect = dw / dh
              dw = fromIntegral w
              dh = fromIntegral h
              gs = fromIntegral gridSize
          in if aspect >= 1
             then let cwl = gs * (aspect - 1) / 2
                  in (-cwl, gs + cwl, 0, gs)
             else let chl = gs * (recip aspect - 1) / 2
                  in (0, gs, -chl, gs + chl)

-- | Close the window and terminate GLFW
quit = GLFW.closeWindow >> GLFW.terminate

-- | This will print and clear the OpenGL errors
printErrors = GL.get GL.errors >>= mapM_ print

-- | Draw the window and handle input
mainLoop :: Grid -> Maybe Int -> Bool -> IO ()
mainLoop !grid generations active = do
  now <- GLFW.getTime
  grid' <- if (active || isJust generations) then updateGrid grid else return grid
  draw grid'

  -- Input is polled each time swapBuffers is called
  space <- GLFW.keyIsPressed GLFW.KeySpace
  esc <- GLFW.keyIsPressed GLFW.KeyEsc
  isClosed <- fmap not GLFW.windowIsOpen
  unless (esc || isClosed) $ do
      -- Sleep for the rest of the frame
      afterFrame <- GLFW.getTime
      let frameLeft = spf - (afterFrame - now)
      when (frameLeft > 0) $
           threadDelay (truncate $ 1000000 * frameLeft)

      unless (generations == (Just 0)) $
             mainLoop grid' (fmap pred generations) (active || space)

  where
    -- maximum frame rate
    fps = 15
    spf = recip fps

-- | Create a new grid
freshGrid :: Int -> LifePattern -> Grid
freshGrid gridSize pattern =
    A.computeS . A.fromFunction (A.Z :. gridSize :. gridSize) $ initCell'

    where
      cx = gridSize `div` 2

      -- | Initialize a grid cell. Guards are used to make the initial pattern.
      initCell' (A.Z :. y :. x) =
          if initCell pattern (x - cx, cx - y)
          then 1
          else 0

-- | Evolve the grid one generation
updateGrid :: Grid -> IO Grid
updateGrid = A.computeP
             . A.smap step
             . mapStencil2 (A.BoundConst 0x20)
               [stencil2| 1  1  1
                          1 16  1
                          1  1  1 |]

    where
      {-# INLINE step #-}
      step x
          | x == (16 .|. 2) = 1
          | (x .&. 0xF) == 3 = 1
          | otherwise = 0

-- | Draw a frame
draw :: Grid -> IO ()
draw grid = do
  uploadGridAsTexture

  GL.clear [GL.ColorBuffer]
  GL.loadIdentity
  GL.scale (fromIntegral width) (fromIntegral height) (1 :: GL.GLfloat)

  GL.renderPrimitive GL.Quads $
    -- Draw a unit square
    forM_ [(0, 0), (1, 0), (1, 1), (0, 1)] $ \(x, y) -> do
        GL.texCoord $ GL.TexCoord2 (texS * x) (texT * y)
        GL.vertex (GL.Vertex3 x y 0 :: GL.Vertex3 GL.GLfloat)

  printErrors
  GL.flush
  GLFW.swapBuffers

    where
      gridShape@(A.Z :. height :. width) = A.extent grid

      maxS = nextPowerOf2 width
      maxT = nextPowerOf2 height

      texS = fromIntegral width / fromIntegral maxS
      texT = fromIntegral height / fromIntegral maxT

      -- | Convert the grid to a texture and send it to the graphics card
      uploadGridAsTexture = do
              -- convert the grid to grayscale and pad it to the texture size
              grayscale <- A.computeUnboxedP . pad . A.map toGrayscale
                           $ grid :: IO Grid

              -- convert the grid to a storable vector
              let unboxed = VS.convert . A.toUnboxed $ grayscale

              -- upload the storable vector to texture memory
              VS.unsafeWith unboxed $ \ptr -> do
                  GL.texImage2D Nothing GL.NoProxy 0 GL.Luminance'
                    (GL.TextureSize2D (fromIntegral maxS) (fromIntegral maxT))
                    0
                    (GL.PixelData GL.Luminance GL.UnsignedByte ptr)

      {-# INLINE toGrayscale #-}
      toGrayscale 0 = 255
      toGrayscale _ = 0

      pad ar = A.traverse ar
               (const (A.Z :. (fromIntegral maxT) :. (fromIntegral maxS))) $ \lkup ix ->
                   if A.inShape gridShape ix
                   then lkup ix
                   else 0

-- | Calculate the first power of 2 >= val. This is not an
--   efficient method, but it's only called once.
nextPowerOf2 val = go 1
    where
      go n
          | n >= val = n
          | otherwise = go (2 * n)
