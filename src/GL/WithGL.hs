module GL.WithGL
    ( withGL
    , createVBO
    , makeProg
    , module GL.GLEnv
    ) where

import           Control.Concurrent       ( threadDelay )
import           Control.Monad            ( liftM2, unless, when )
import           Control.Monad.IO.Class   ( MonadIO(..), liftIO )
import           Control.Monad.Trans.Cont ( ContT(..), evalContT )
import qualified Data.Vector.Storable     as V
import qualified Data.ByteString          as B
import           Data.Bits                ( (.|.) )

import           Foreign.Ptr
import           Foreign.Marshal.Alloc    ( alloca, allocaBytes )
import           Foreign.Marshal.Utils    ( with )
import           Foreign.Storable

import           Graphics.GL
import qualified Graphics.UI.GLFW         as W

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
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
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

peekf :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
peekf f = liftIO . alloca $ liftM2 (>>) f peek

makeProg :: B.ByteString -> B.ByteString -> IO GLuint
makeProg vstext fstext = do
    prog <- glCreateProgram
    vs <- loadShader GL_VERTEX_SHADER vstext
    fs <- loadShader GL_FRAGMENT_SHADER fstext
    glAttachShader prog vs
    glAttachShader prog fs
    glLinkProgram prog
    status <- peekf $ glGetProgramiv prog GL_LINK_STATUS
    when (status == GL_FALSE) $ do
        len <- peekf $ glGetProgramiv prog GL_INFO_LOG_LENGTH
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetProgramInfoLog prog len nullPtr buf
                B.putStr =<< B.packCStringLen (buf, fromIntegral len)
    glDeleteShader vs
    glDeleteShader fs
    glUseProgram prog >> return prog

loadShader :: GLenum -> B.ByteString -> IO GLuint
loadShader t code = do
    shader <- glCreateShader t
    evalContT $ do
        (s, l) <- ContT (B.useAsCStringLen code)
        sp <- ContT (with s)
        lp <- ContT (with . fromIntegral $ l)
        liftIO $ glShaderSource shader 1 sp lp
    glCompileShader shader
    status <- peekf $ glGetShaderiv shader GL_COMPILE_STATUS
    when (status == GL_FALSE) $ do
        len <- peekf $ glGetShaderiv shader GL_INFO_LOG_LENGTH
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetShaderInfoLog shader len nullPtr buf
                B.putStr =<< B.packCStringLen (buf, fromIntegral len)
    return $ if status == GL_TRUE then shader else undefined
