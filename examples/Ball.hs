{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe           ( fromJust )
import           Data.HashMap.Strict  ( (!) )
import qualified Data.HashMap.Strict  as M ( fromList )
import           Graphics.GL
import qualified Graphics.UI.GLFW     as W

import           GL.Foreign
import           GL.Math
import           GL.WithGL
import           GL.Primitive
import           GL.GLSL
import qualified GL.Geometric         as G

main :: IO ()
main = withGL 400 400 "GL Window" makeEnv binder render

makeEnv :: IO GLEnv
makeEnv = do
    ballVertex <- G.ball [ 0, 0, 0 ] 20
    p1 <- createElement GL_LINES
                        [ -20, 0, 0, 20, 0, 0, 0, -20, 0, 0, 20, 0, 0, 0, -20, 0, 0, 20 ]
                        [ 0, 1, 2, 3, 4, 5 ]
                        (0, 6)
    p2 <- createArray GL_TRIANGLE_STRIP ballVertex (0, 40000)

    prog <- program [ ("glsl/ball/vertex.glsl", GL_VERTEX_SHADER)
                    , ("glsl/ball/fragment.glsl", GL_FRAGMENT_SHADER)
                    ]

    let vars = [ "model", "view", "projection" ]
    uniforms <- mapM (locateUniform prog) vars
    return GLEnv { ebo' = 6, uniform = M.fromList $ zip vars uniforms, things = [ p1, p2 ] }

binder :: W.Window -> GLEnv -> IO ()
binder _ _ = return ()

render :: GLEnv -> IO ()
render GLEnv{..} = do
    time <- fmap (fromRational . toRational . fromJust) W.getTime

    let t = translate 0 0 0
        s = scale 1 1 1
        r = rotate time [ time, time, time ]
        model = t !*! r !*! s

    let view = lookat [ 0, 0, 0 ] [ 0, 0, 1 ] [ 0, -1, 0 ]
    let projection = ortho (-30) 30 (-30) 30 (-30) 30

    mapM_ primitive things

    let vars = zip [ "model", "view", "projection" ] [ model, view, projection ]
    let setUniformMatrix (name, mat) =
            alloca $
                \p -> do
                    poke p (mat :: M44 GLfloat)
                    glUniformMatrix4fv (uniform ! name) 1 GL_TYPE (castPtr p)
    mapM_ setUniformMatrix vars
