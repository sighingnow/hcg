{-# LANGUAGE RecordWildCards   #-}

module GL.WithGL
    ( withGL
    , peekFrom
    , pokeTo
    , createIBO
    , createVAO
    , createVBO
    , makeProg
    , locateUniform
    , locateAttribute
    , module GL.GLEnv
    ) where

import           Control.Concurrent       ( threadDelay )
import           Control.Monad            ( liftM2, unless, when )
import           Control.Monad.IO.Class   ( MonadIO(..), liftIO )
import           Control.Monad.Trans.Cont ( ContT(..), evalContT )
import qualified Data.Vector.Storable     as V
import qualified Data.Text                as T
import qualified Data.Text.Foreign           as T
import qualified Data.Text.IO                as T
import           Data.Bits                ( (.|.) )

import           Foreign.C.String         ( withCString )
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
       -> (W.Window -> GLEnv -> IO ()) -- ^ binding all event callbacks.
        -> (GLEnv -> IO ()) -- ^ render of main loop
       -> IO ()
withGL w h title env binder render =
    W.init >>=
        \r -> when r $ do
            W.windowHint $ W.WindowHint'ClientAPI W.ClientAPI'OpenGL
            W.windowHint $ W.WindowHint'OpenGLDebugContext True
            window <- W.createWindow w h title Nothing Nothing
            case window of
                Just win -> do
                    W.makeContextCurrent window
                    glClearColor 0 0 0 0
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

    glFlush
    W.swapBuffers win
    -- handleError
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

-- | Create index buffer object object.
createIBO :: V.Vector GLuint -> IO GLuint
createIBO indices = do
    let isize = fromIntegral $ sizeOf (V.head indices) * V.length indices
    ibo' <- alloca $
                \p -> glGenBuffers 1 p >> peek p
    V.unsafeWith indices $
        \p -> glBufferData GL_ELEMENT_ARRAY_BUFFER isize (castPtr p) GL_STATIC_DRAW
    return ibo'

-- | Create vertex array object.
createVAO :: V.Vector GLfloat -> IO GLuint
createVAO vertices = do
    let vsize = fromIntegral $ sizeOf (V.head vertices) * V.length vertices
    vao' <- alloca $
                \p -> glGenVertexArrays 1 p >> peek p
    glBindVertexArray vao'
    V.unsafeWith vertices $
        \p -> glBufferData GL_ARRAY_BUFFER vsize (castPtr p) GL_STATIC_DRAW
    return vao'

-- | Create vertex buffer object.
createVBO :: V.Vector GLfloat -> IO GLuint
createVBO vertices = do
    let vsize = fromIntegral $ sizeOf (V.head vertices) * V.length vertices
    vbo' <- alloca $
                \p -> glGenBuffers 1 p >> peek p
    V.unsafeWith vertices $
        \p -> glBufferData GL_ARRAY_BUFFER vsize (castPtr p) GL_STATIC_DRAW
    return vbo'

peekFrom :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
peekFrom f = liftIO . alloca $ liftM2 (>>) f peek

pokeTo :: (MonadIO m, Storable a) => a -> (Ptr a -> IO b) -> m b
pokeTo v f = liftIO . alloca $ liftM2 (>>) (`poke` v) f

makeProg :: T.Text -> T.Text -> IO GLuint
makeProg vstext fstext = do
    prog <- glCreateProgram
    vs <- makeShader GL_VERTEX_SHADER vstext
    fs <- makeShader GL_FRAGMENT_SHADER fstext
    glAttachShader prog vs
    glAttachShader prog fs
    glLinkProgram prog
    status <- peekFrom $ glGetProgramiv prog GL_LINK_STATUS
    when (status == GL_FALSE) $ do
        len <- peekFrom $ glGetProgramiv prog GL_INFO_LOG_LENGTH
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetProgramInfoLog prog len nullPtr buf
                T.putStr =<< T.peekCStringLen (buf, fromIntegral len)
    mapM_ glDeleteShader [vs, fs]
    glUseProgram prog >> return prog

makeShader :: GLenum -> T.Text -> IO GLuint
makeShader t code = do
    shader <- glCreateShader t
    evalContT $ do
        (s, l) <- ContT (T.withCStringLen code)
        sp <- ContT (with s)
        lp <- ContT (with . fromIntegral $ l)
        liftIO $ glShaderSource shader 1 sp lp
    glCompileShader shader
    status <- peekFrom $ glGetShaderiv shader GL_COMPILE_STATUS
    when (status == GL_FALSE) $ do
        len <- peekFrom $ glGetShaderiv shader GL_INFO_LOG_LENGTH
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetShaderInfoLog shader len nullPtr buf
                T.putStr =<< T.peekCStringLen (buf, fromIntegral len)
    return $ if status == GL_TRUE then shader else undefined

locateUniform :: GLuint -> String -> IO GLint
locateUniform prog name = withCString name $ \p -> glGetUniformLocation prog p

locateAttribute :: GLuint -> String -> IO GLint
locateAttribute prog name = withCString name $ \p -> glGetAttribLocation prog p
