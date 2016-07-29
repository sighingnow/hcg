{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.HashMap.Strict         ( (!) )
import           Foreign.Ptr

import           Graphics.GL
import           Graphics.GL.Compatibility43 ()
import qualified Graphics.UI.GLFW            as W ()
import qualified Data.Text                   as T ( Text )
import qualified Data.Text.Foreign           as T ( withCStringLen )
import qualified Data.HashMap.Strict         as M ( fromList )
import           Foreign.Storable
import           Foreign.Marshal.Alloc       ( alloca )

import           GL.Math
import           GL.MissingH.TH
import           GL.WithGL

main :: IO ()
main = withGL 400 400 "GL Window" makeEnv render

makeEnv :: IO GLEnv
makeEnv = do
    vbo <- createVBO [ -1, -1, 0.5773, 0, -1, -1.154, 1, -1, 0.5773, 0, 1, 0 ]
    ibo <- createIBO [ 0, 3, 1, 1, 3, 2, 2, 3, 0, 0, 1, 2 ]
    prog <- makeProg vstext fstext
    let vars = [ "model", "view", "projection" ]
    uniforms <- mapM (locateUniform prog) vars
    return GLEnv { vbo = vbo, ibo = ibo, uniform = M.fromList $ zip vars uniforms }

render :: GLEnv -> IO ()
render GLEnv{..} = do
    let t = translate 0 0 5
        s = scale 1 1 1
        r = rotate 0
        model = t .* r .* s
    let view = lookat [ 0, 0, 0 ] [ 0, 0, 1 ] [ 0, 1, 0 ]
    let projection = perspect 1.0 (60 / 180 * pi) 0 100

    let vars = zip [ "model", "view", "projection" ] [ model, view, projection ]
    let setUniformMatrix (name, mat) =
            alloca $
                \p -> do
                    poke p (mat :: M44 GLfloat)
                    glUniformMatrix4fv (uniform ! name) 1 GL_TYPE (castPtr p)
    mapM_ setUniformMatrix vars

    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER vbo
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
    glDrawElements GL_TRIANGLES 12 GL_UNSIGNED_INT nullPtr
    glDisableVertexAttribArray 0

-- | Vertex shader.
vstext :: T.Text
vstext = [raw|
#version 430

layout (location = 0) in vec3 vpos;
out vec4 color;

uniform mat4 model, view, projection;

void main() {
    mat4 mvp = projection * view * model;
    gl_Position = mvp * vec4(vpos, 1.0);
    color = vec4(clamp(vpos, 0.0, 1.0), 1.0);
}|]

-- | Fragment shader.
fstext :: T.Text
fstext = [raw|
#version 430

in vec4 color;

void main() {
    gl_FragColor = color;
}|]
