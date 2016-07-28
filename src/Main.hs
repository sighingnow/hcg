{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.HashMap.Strict         ((!))
import           Foreign.Ptr

import           Graphics.GL
import           Graphics.GL.Compatibility45 ()
import qualified Graphics.UI.GLFW            as W ()
import qualified Data.ByteString as B

import           GL.WithGL
import           GL.MissingH.TH

main :: IO ()
main = withGL 400 300 "GL Window" makeEnv render

makeEnv :: IO GLEnv
makeEnv = do
    vbo <- createVBO [ 0, 0, 0, 0.5, 0.5, 0.5, 0.5, -0.5, 0.5 ]
    prog <- makeProg vstext fstext
    gScale <- B.useAsCString "gScale" $ \p -> glGetUniformLocation prog p
    return GLEnv { vbo = vbo
                 , uniform = [("gScale", gScale)]
                 }

render :: GLEnv -> IO ()
render GLEnv{..} = do
    glUniform1f (uniform ! "gScale") 1.2

    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER vbo
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
    glDrawArrays GL_TRIANGLES 0 3
    glDisableVertexAttribArray 0

vstext :: B.ByteString
vstext = [raw|
#version 400

layout (location = 0) in vec3 Position;
uniform float gScale;
void main() {
    gl_Position = vec4(gScale * Position.x, gScale * Position.y, Position.z, 1.0);
}|]

fstext  :: B.ByteString
fstext = [raw|
#version 400

out vec4 FragColor;
void main() {
    FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}|]
