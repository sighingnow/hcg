{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe                   ( fromJust )
import           Data.HashMap.Strict          ( (!) )
import qualified Data.HashMap.Strict          as M ( fromList )
import qualified Data.Vector.Storable         as V
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL
import qualified Graphics.UI.GLFW             as W
import           Foreign.Marshal.Alloc        ( alloca )

import           GL.Math
import           GL.WithGL
import qualified GL.Geometric                 as G

main :: IO ()
main = withGL 400 400 "GL Window" makeEnv binder render

makeEnv :: IO GLEnv
makeEnv = do
    vao <- createVAO
    ballVertex <- G.ball [ 0, 0, 0 ] 20
    vbo <- createVBO $ [ -20, 0, 0, 20, 0, 0, 0, -20, 0, 0, 20, 0, 0, 0, -20, 0, 0, 20 ] V.++ ballVertex
    ebo <- createEBO [ 0, 1, 2, 3, 4, 5 ]
    prog <- makeProg [ ("glsl/ball/vertex.glsl", GL_VERTEX_SHADER), ("glsl/ball/fragment.glsl", GL_FRAGMENT_SHADER) ]

    setupAttrib prog "position" 3 GL_FLOAT GL_FALSE 0 0

    let vars = [ "model", "view", "projection" ]
    uniforms <- mapM (locateUniform prog) vars
    return GLEnv { ebo' = 6, uniform = M.fromList $ zip vars uniforms }

binder :: W.Window -> GLEnv -> IO ()
binder _ _ = return ()

render :: GLEnv -> IO ()
render GLEnv{..} = do
    time <- fmap (fromRational . toRational . fromJust) W.getTime

    let t = translate 0 0 0
        s = scale 1 1 1
        r = rotate time [ time, time, time ]
        model = t .* r .* s

    let view = lookat [ 0, 0, 0 ] [ 0, 0, 1 ] [ 0, -1, 0 ]
    let projection = ortho (-30) 30 (-30) 30 (-30) 30

    glDrawArrays GL_TRIANGLE_STRIP 6 40000
    glDrawElements GL_LINES ebo' GL_UNSIGNED_INT nullPtr -- GL_TRIANGLE_STRIP

    let vars = zip [ "model", "view", "projection" ] [ model, view, projection ]
    let setUniformMatrix (name, mat) =
            alloca $
                \p -> do
                    poke p (mat :: M44 GLfloat)
                    glUniformMatrix4fv (uniform ! name) 1 GL_TYPE (castPtr p)
    mapM_ setUniformMatrix vars
