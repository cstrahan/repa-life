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
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign.Storable
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (catch)
import System.Environment

import Debug.Trace

type Grid = A.Array A.U A.DIM2 Word8

main = do
  args <- getArgs
  case args of
    [size] -> run (read size) Nothing
    [size, generations] -> run (read size) (Just $ read generations)
    _ -> putStrLn "Usage: repa-life <grid size> [generations]"

run :: Int -> Maybe Int -> IO ()
run gridSize generations = do
  -- initialize has to come first. If it doesn't return True,
  -- this crashes with a pattern match error.
  True <- GLFW.initialize
  
  -- Set the RGB bits to get a color window. See the GLFW-b docs for all the options
  True <- GLFW.openWindow GLFW.defaultDisplayOptions
          { GLFW.displayOptions_numRedBits = 8
          , GLFW.displayOptions_numGreenBits = 8
          , GLFW.displayOptions_numBlueBits = 8
          , GLFW.displayOptions_width = 640
          , GLFW.displayOptions_height = 480
          }

  GLFW.setWindowSizeCallback $ resize gridSize

  [tex] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D $= Just tex
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.texture GL.Texture2D $= GL.Enabled
  forM_ [GL.S, GL.T, GL.R] $ \coord ->
      GL.textureWrapMode GL.Texture2D coord $= (GL.Repeated, GL.Clamp)

  -- Use `finally` so that `quit` is called whether or not `mainLoop` throws an exception
  finally (mainLoop (freshGrid gridSize) generations) quit

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
mainLoop :: Grid -> Maybe Int -> IO ()
mainLoop !grid generations = do
  now <- GLFW.getTime
  grid' <- updateGrid grid
  draw grid'

  -- Input is polled each time swapBuffers is called
  esc <- GLFW.keyIsPressed GLFW.KeyEsc
  isClosed <- fmap not GLFW.windowIsOpen
  unless (esc || isClosed) $ do
      -- Sleep for the rest of the frame
      afterFrame <- GLFW.getTime
      let frameLeft = spf - (afterFrame - now)
      when (frameLeft > 0) $
           threadDelay (truncate $ 1000000 * frameLeft)

      unless (generations == (Just 0)) $
             mainLoop grid' (fmap pred generations)

  where
    -- maximum frame rate
    fps = 15
    spf = recip fps

freshGrid :: Int -> Grid
freshGrid gridSize =
    A.computeS . A.fromFunction (A.Z :. gridSize :. gridSize) $ initCells

    where
      cx = gridSize `div` 2
      initCells ix

{- Blinker
          | ix == (A.Z :. cx :. cx) = 1
          | ix == (A.Z :. cx-1 :. cx) = 1
          | ix == (A.Z :. cx+1 :. cx) = 1
-}

{- Acorn -}
          | ix == (A.Z :. cx :. cx) = 1
          | ix == (A.Z :. cx+1 :. cx) = 1
          | ix == (A.Z :. cx+1 :. cx+2) = 1
          | ix == (A.Z :. cx+3 :. cx+1) = 1
          | ix == (A.Z :. cx+4 :. cx) = 1
          | ix == (A.Z :. cx+5 :. cx) = 1
          | ix == (A.Z :. cx+6 :. cx) = 1

          | otherwise = 0

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

  -- Again, the functions in GL all map to standard OpenGL functions
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
      gridShape@(A.Z :. width :. height) = A.extent grid

      maxS = nextPowerOf2 width
      maxT = nextPowerOf2 height

      texS = fromIntegral width / fromIntegral maxS
      texT = fromIntegral height / fromIntegral maxT

      uploadGridAsTexture = do
              grayscale <- A.computeUnboxedP . pad . A.transpose . A.map toGrayscale $ grid :: IO Grid
              let unboxed = VS.convert . A.toUnboxed $ grayscale
              VS.unsafeWith unboxed $ \ptr -> do
                  GL.texImage2D Nothing GL.NoProxy 0 GL.Luminance'
                    (GL.TextureSize2D (fromIntegral maxS) (fromIntegral maxT))
                    0
                    (GL.PixelData GL.Luminance GL.UnsignedByte ptr)

      {-# INLINE toGrayscale #-}
      toGrayscale 0 = 255
      toGrayscale _ = 0

      pad ar = A.traverse ar
               (const (A.Z :. (fromIntegral maxS) :. (fromIntegral maxT))) $ \lkup ix ->
                   if A.inShape gridShape ix
                   then lkup ix
                   else 0

nextPowerOf2 val = go 1
    where
      go n
          | n >= val = n
          | otherwise = go (2 * n)

byteCount :: VS.Storable a => VS.Vector a -> GL.GLsizeiptr
byteCount vec = (elSize undefined vec) * (fromIntegral . VS.length $ vec)
    where
      elSize :: VS.Storable a => a -> VS.Vector a -> GL.GLsizeiptr
      elSize dummy _ = fromIntegral $ sizeOf dummy
