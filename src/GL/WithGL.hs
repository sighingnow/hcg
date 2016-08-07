{-# LANGUAGE RecordWildCards #-}

module GL.WithGL
    ( cast
    , withGL
    , module GL.GLEnv
    ) where

import           Control.Concurrent ( threadDelay )
import           Control.Monad      ( unless, when )
import           Data.Bits          ( (.|.) )

import           Graphics.GL
import qualified Graphics.UI.GLFW   as W

import           GL.GLEnv

-- | Casting between haskell floating type and OpenGL floating type.
cast :: (Real a, Fractional c) => a -> c
cast = fromRational . toRational

withGL :: Int -- ^ w
       -> Int -- ^ h
       -> String -- ^ title
       -> IO GLEnv -- global env variable, like vbo
       -> (W.Window -> GLEnv -> IO ()) -- ^ binding all event callbacks.
       -> (GLEnv -> IO ()) -- ^ render of main loop
       -> IO ()
withGL w h title env binder render =
    W.init >>=
        \r -> when r $ do
            W.windowHint $ W.WindowHint'OpenGLDebugContext True
            W.windowHint $ W.WindowHint'ClientAPI W.ClientAPI'OpenGL
            W.windowHint $ W.WindowHint'ContextVersionMajor 4
            W.windowHint $ W.WindowHint'ContextVersionMinor 3
            W.windowHint $ W.WindowHint'OpenGLProfile W.OpenGLProfile'Core
            window <- W.createWindow w h title Nothing Nothing
            case window of
                Just win -> do
                    W.makeContextCurrent window
                    glClearColor 0.0 0.0 0.0 0
                    glEnable GL_CULL_FACE
                    glEnable GL_DEPTH_TEST
                    glDepthFunc GL_LESS
                    e <- env
                    binder win e
                    mainLoop win render e
                    W.destroyWindow win
                Nothing -> putStrLn "Can't create GL window."
            W.terminate >> putStrLn "Normal termination."

mainLoop :: W.Window -> (GLEnv -> IO ()) -> GLEnv -> IO ()
mainLoop win render env@GLEnv{..} = do
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
    render env

    -- handleError
    W.swapBuffers win >>
        glFlush >>
        W.pollEvents

    threadDelay 10000
    closed <- W.windowShouldClose win
    unless closed (mainLoop win render env)
