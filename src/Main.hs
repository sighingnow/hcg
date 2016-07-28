{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Vector.Storable        as V
import           Foreign.Ptr

import           Graphics.GL
import qualified Graphics.UI.GLFW            as W

import           GL.WithGL

main :: IO ()
main = withGL 400 300 "GL Window" makeEnv render

makeEnv :: IO GLEnv
makeEnv = do
    vbo <- createVBO [0, 0, 0, 0.5, 0.5, 0.5]
    return $ GLEnv vbo

render :: GLEnv -> IO ()
render GLEnv{..} = do
    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER vbo
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
    glDrawArrays GL_LINES 0 2
    glDisableVertexAttribArray 0
