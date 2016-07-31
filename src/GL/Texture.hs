module GL.Texture ( createTex ) where

import           Codec.Picture        ( DynamicImage(..), Image(..), readPng )
import qualified Data.Vector.Storable as V
import           Foreign.Ptr

import           Graphics.GL

import           GL.Foreign

-- | Create texture buffer.
createTex :: FilePath -> IO GLuint
createTex img = do
    png <- readPng img
    (Image w h raw) <- case png of
                           Right (ImageRGBA8 i) -> return i
                           Left s -> error s
                           _ -> error "Invalid image format, true color with alpha channel is required"
    textureID <- peekFrom $ glGenTextures 1
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D textureID
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
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR_MIPMAP_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
    return textureID
