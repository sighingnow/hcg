module GL.WithGL
    ( withGL
    , createVBO
    , module GL.GLEnv
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Vector.Storable  as V

import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Storable

import           Graphics.GL
import qualified Graphics.UI.GLFW      as W

import           GL.GLEnv

withGL :: Int -- ^ w
       -> Int -- ^ h
       -> String -- ^ title
       -> IO GLEnv -- global env variable, like vbo
       -> (GLEnv -> IO ()) -- ^ render of main loop
       -> IO ()
withGL w h title env render =
    W.init >>=
        \r -> when r $ do
            window <- W.createWindow w h title Nothing Nothing
            case window of
                Just win -> do
                    W.makeContextCurrent window
                    W.setErrorCallback $ Just errorCallback
                    W.setWindowCloseCallback win $ Just closeCallback
                    W.setWindowFocusCallback win $ Just focusCallback
                    W.setKeyCallback win $ Just keyCallback
                    glClearColor 0 0 0 0
                    env >>= mainLoop win render
                    W.destroyWindow win
                Nothing -> putStrLn "Can't create GL window."
            W.terminate >> putStrLn "Normal termination."
  where
    errorCallback e s = print $ concat [ show e, ": ", s ]
    closeCallback _ = print "Window are closing."
    focusCallback _ W.FocusState'Focused =
        print "Mouse focused"
    focusCallback _ W.FocusState'Defocused =
        print "Mouse defocused"
    keyCallback win key _ action _ =
        when (key == W.Key'Escape && action == W.KeyState'Pressed) $ W.setWindowShouldClose win True

mainLoop :: W.Window -> (GLEnv -> IO ()) -> GLEnv -> IO ()
mainLoop win render env = do
    glClear GL_COLOR_BUFFER_BIT
    render env

    glFlush
    W.swapBuffers win
    handleError
    W.pollEvents

    threadDelay 10000
    closed <- W.windowShouldClose win
    unless closed (mainLoop win render env)

handleError :: IO ()
handleError = do
    err <- glGetError
    case err of
        GL_NO_ERROR -> return ()
        e -> (putStrLn . disp) e >> handleError
  where
    disp :: GLuint -> String
    disp GL_INVALID_ENUM = "GL_INVALID_ENUM"
    disp GL_INVALID_VALUE = "GL_INVALID_VALUE"
    disp GL_INVALID_OPERATION =
        "GL_INVALID_OPERATION"
    disp GL_INVALID_FRAMEBUFFER_OPERATION =
        "GL_INVALID_FRAMEBUFFER_OPERATION"
    disp GL_OUT_OF_MEMORY = "GL_OUT_OF_MEMORY"
    disp GL_STACK_UNDERFLOW =
        "GL_STACK_UNDERFLOW"
    disp GL_STACK_OVERFLOW =
        "GL_STACK_OVERFLOW"
    disp x = "Unknow error: " ++ show x

createVBO :: V.Vector GLfloat -> IO GLuint
createVBO vertices = do
    let vsize = sizeOf (V.head vertices) * V.length vertices
    vbo' <- alloca $
                \p -> glGenBuffers 1 p >> peek p
    glBindBuffer GL_ARRAY_BUFFER vbo'
    V.unsafeWith vertices $
        \p -> glBufferData GL_ARRAY_BUFFER (fromIntegral vsize) (castPtr p) GL_STATIC_DRAW
    return vbo'
