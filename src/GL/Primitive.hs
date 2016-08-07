{-# LANGUAGE RecordWildCards #-}

module GL.Primitive
    ( Primitive(..)
    , primitive
    , primitiveIO
    , createArray
    , createArrayWithTex
    , createElement
    , createElementWithTex
    ) where

import           Control.Monad        ( void, when )
import           Graphics.GL
import qualified Data.Vector.Storable as V

import           GL.Foreign

data Primitive = PArray { mode  :: GLenum -- ^ draw mode
                        , vao   :: GLuint -- ^ vertex array object
                        , first :: GLint -- ^ offset of start point
                        , count :: GLsizei -- ^ point number
                        }
               | PElement { mode  :: GLenum -- ^ draw mode
                          , vao   :: GLuint -- ^ vertex array object
                          , first :: GLint -- ^ offset of start point
                          , count :: GLsizei -- ^ point number
                          }
    deriving (Eq, Show)

-- | Render a primitive object.
primitive :: Primitive -> IO ()
primitive PArray{..} = do
    glBindVertexArray vao
    glDrawArrays mode first count
    glBindVertexArray 0
primitive PElement{..} = do
    glBindVertexArray vao
    glDrawElements mode count GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0

-- | Render a primitive object produced from IO cation.
primitiveIO :: IO Primitive -> IO ()
primitiveIO = (primitive =<<)

-- | Create vertex array object, input data for a point has three parts /location [, color, texture]/ and last two are
-- optional. The vertex location will be bound to attribute pointer 0, color will be bound to attribute pointer 1, and
-- texture will be bound to attribute pointer 2.
createPrimImpl :: V.Vector GLfloat -- ^ vertex data
               -> Maybe (V.Vector GLuint) -- ^ element index array, not necessary
               -> Bool -- if color
               -> Bool -- if texture
               -> IO GLuint
createPrimImpl vertices indices color texture = do
    vao <- createVAO
    _ <- createVBO vertices
    case indices of
        Just index -> void $ createEBO index
        Nothing -> return ()

    let stride = 3 + (if color then 2 else 0) + (if texture then 2 else 0)
        stride' = fromIntegral $ stride * sizeOfFloat
        offset1 = 3
        offset1' = intPtrToPtr . fromIntegral $ offset1 * sizeOfFloat
        offset2 = 3 + if color then 2 else 0
        offset2' = intPtrToPtr . fromIntegral $ offset2 * sizeOfFloat

    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride' nullPtr
    when color $ do
        glEnableVertexAttribArray 1
        glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride' offset1'
    when texture $ do
        glEnableVertexAttribArray 2
        glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE stride' offset2'

    glBindVertexArray 0
    return vao

-- | Construct a primitive vertex array.
createArray :: GLenum -- ^ draw mode
            -> V.Vector GLfloat -- ^ vertex data
            -> (GLint, GLsizei) -- ^ offset of start point and number of points to render
            -> IO Primitive
createArray m vertices (s, cnt) = do
    vao <- createPrimImpl vertices Nothing False False
    return PArray { mode = m, vao = vao, first = s, count = cnt }

-- | Construct a primitive vertex array.
createArrayWithTex :: GLenum -- ^ draw mode
                   -> V.Vector GLfloat -- ^ vertex data and texture data
                   -> (GLint, GLsizei) -- ^ offset of start point and number of points to render
                   -> IO Primitive
createArrayWithTex m vertices (s, cnt) = do
    vao <- createPrimImpl vertices Nothing False True
    return PArray { mode = m, vao = vao, first = s, count = cnt }

-- | Construct a primitive vertex index array.
createElement :: GLenum -- ^ draw mode
              -> V.Vector GLfloat -- ^ vertex data
              -> V.Vector GLuint -- ^ vertex indices
              -> (GLint, GLsizei) -- ^ offset of start point and number of points to render
              -> IO Primitive
createElement m vertices indices (s, cnt) = do
    vao <- createPrimImpl vertices (Just indices) False False
    return PElement { mode = m, vao = vao, first = s, count = cnt }

-- | Construct a primitive vertex index array.
createElementWithTex :: GLenum -- ^ draw mode
                     -> V.Vector GLfloat -- ^ vertex data and texture data
                     -> V.Vector GLuint -- ^ vertex indices
                     -> (GLint, GLsizei) -- ^ offset of start point and number of points to render
                     -> IO Primitive
createElementWithTex m vertices indices (s, cnt) = do
    vao <- createPrimImpl vertices (Just indices) False True
    return PElement { mode = m, vao = vao, first = s, count = cnt }

-- | Storage size of GLfloat type.
sizeOfFloat :: Int
sizeOfFloat = sizeOf (undefined :: GLfloat)

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
