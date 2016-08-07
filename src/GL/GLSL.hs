module GL.GLSL
    ( program
    , locateUniform
    ) where

import           Control.Monad            ( when )
import           Control.Monad.Trans.Cont
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Maybe               ( fromJust )
import qualified Data.Text                as T
import qualified Data.Text.Foreign        as T
import qualified Data.Text.IO             as T ( readFile )

import           Graphics.GL

import           GL.Foreign

program :: [(FilePath, GLenum)] -> IO GLuint
program res = do
    prog <- glCreateProgram
    shaders <- mapM (\(fp, st) -> T.readFile fp >>= fmap fromJust . makeShader st) res
    mapM_ (glAttachShader prog) shaders >> glLinkProgram prog
    status <- peekFrom $ glGetProgramiv prog GL_LINK_STATUS
    len <- peekFrom $ glGetProgramiv prog GL_INFO_LOG_LENGTH
    when (status == GL_FALSE || len > 0) $
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetProgramInfoLog prog len nullPtr buf
                putStrLn =<< peekCString buf
    mapM_ glDeleteShader shaders >> glUseProgram prog >> return prog

makeShader :: GLenum -> T.Text -> IO (Maybe GLuint)
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
    when (status == GL_FALSE || len > 0) $
        allocaBytes (fromIntegral len) $
            \buf -> do
                glGetShaderInfoLog shader len nullPtr buf
                putStrLn =<< peekCString buf
    return $ if status == GL_TRUE then Just shader else Nothing

locateUniform :: GLuint -- ^ shader program object
              -> String -- ^ name
              -> IO GLint
locateUniform prog name =
    withCString name $ \p -> glGetUniformLocation prog p
