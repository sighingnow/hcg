{-# LANGUAGE RecordWildCards #-}

module GL.WithGL
    ( withGL
    , createEBO
    , createVAO
    , createVBO
    , makeProg
    , setupAttrib
    , locateUniform
    , module GL.GLEnv
    ) where

import           Control.Concurrent       ( threadDelay )
import           Control.Monad            ( unless, when )
import           Control.Monad.IO.Class   ( MonadIO(..), liftIO )
import           Control.Monad.Trans.Cont ( ContT(..), evalContT )
import qualified Data.Vector.Storable     as V
import qualified Data.Text                as T
import qualified Data.Text.Foreign        as T
import qualified Data.Text.IO             as T ( putStrLn, readFile )
import           Data.Bits                ( (.|.) )

import           Foreign.C.String         ( withCString )
import           Foreign.Ptr
import           Foreign.Marshal.Alloc    ( allocaBytes )
import           Foreign.Marshal.Utils    ( with )
import           Foreign.Storable

import           Graphics.GL
import qualified Graphics.UI.GLFW         as W

import           GL.Foreign
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

-- | Create element buffer (index buffer) object.
createEBO :: V.Vector GLuint -> IO GLuint
createEBO indices = do
    let isize = fromIntegral $ sizeOf (V.head indices) * V.length indices
    ibo <- peekFrom $ glGenBuffers 1
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
    V.unsafeWith indices $
        \p -> glBufferData GL_ELEMENT_ARRAY_BUFFER isize (castPtr p) GL_STATIC_DRAW
    return ibo

-- | Create vertex array object.
createVAO :: IO GLuint
createVAO = do
    vao <- peekFrom $ glGenVertexArrays 1
    glBindVertexArray vao
    return vao

-- | Create vertex buffer object.
createVBO :: V.Vector GLfloat -> IO GLuint
createVBO vertices = do
    let vsize = fromIntegral $ sizeOf (V.head vertices) * V.length vertices
    vbo <- peekFrom $ glGenBuffers 1
    glBindBuffer GL_ARRAY_BUFFER vbo
    V.unsafeWith vertices $
        \p -> glBufferData GL_ARRAY_BUFFER vsize (castPtr p) GL_STATIC_DRAW
    return vbo

makeProg :: [(FilePath, GLenum)] -> IO GLuint
makeProg res = do
    prog <- glCreateProgram
    shaders <- mapM (\(fp, st) -> T.readFile fp >>= makeShader st) res
    mapM_ (glAttachShader prog) shaders >> glLinkProgram prog
    status <- peekFrom $ glGetProgramiv prog GL_LINK_STATUS
    len <- peekFrom $ glGetProgramiv prog GL_INFO_LOG_LENGTH
    when (len > 0) $
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetProgramInfoLog prog len nullPtr buf
                T.putStrLn =<< T.peekCStringLen (buf, fromIntegral len)
    mapM_ glDeleteShader shaders >> glUseProgram prog >> return prog

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
    len <- peekFrom $ glGetShaderiv shader GL_INFO_LOG_LENGTH
    when (len > 0) $
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetShaderInfoLog shader len nullPtr buf
                T.putStrLn =<< T.peekCStringLen (buf, fromIntegral len)
    return $ if status == GL_TRUE then shader else undefined

setupAttrib :: GLuint -- ^ shader program object
            -> String -- ^ name in shader
            -> GLint -- ^ size
            -> GLenum -- ^ data type
            -> GLboolean -- ^ normalized
            -> Int -- ^ stride
            -> Int -- ^ offset
            -> IO ()
setupAttrib prog name size dt norm stride offset = do
    loc <- withCString name $ \p -> glGetAttribLocation prog p
    when (loc >= 0) $ do
        let loc' = fromIntegral loc
        glEnableVertexAttribArray loc'
        glVertexAttribPointer loc' size dt norm (fromIntegral stride) (intPtrToPtr . fromIntegral $ offset)
        -- glDisableVertexAttribArray loc' -- DO NOT disable it!
    unless (loc >= 0) $
        error $ "Failed to get the location of attribute " ++ name

locateUniform :: GLuint -- ^ shader program object
              -> String -- ^ name
              -> IO GLint
locateUniform prog name =
    withCString name $ \p -> glGetUniformLocation prog p
