module GL.Texture ( createTex ) where

import           Codec.Picture        ( DynamicImage(..), Image(..), readPng )
import qualified Data.Vector.Storable as V
import           Foreign.Ptr

import           Graphics.GL

import           GL.Foreign

-- | Create texture buffer.
createTex :: FilePath -- ^ texture image path
    -> GLenum -- ^ texture unit
    -> IO GLuint
createTex img unit = do
    png <- readPng img
    (Image w h raw) <- case png of
                           Right (ImageRGBA8 i) -> return i
                           Left s -> error s
                           _ -> error "Invalid image format, true color with alpha channel is required"
    tex <- peekFrom $ glGenTextures 1
    glBindTexture GL_TEXTURE_2D tex
    glActiveTexture unit
    V.unsafeWith raw $
        \p -> glTexImage2D GL_TEXTURE_2D
                           0
                           GL_RGBA
                           (fromIntegral w)
                           (fromIntegral h)
                           0
                           GL_RGBA
                           GL_UNSIGNED_BYTE
                           (castPtr p)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
    return tex
