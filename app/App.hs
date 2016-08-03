{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.HashMap.Strict         ( (!) )
import qualified Data.HashMap.Strict         as M ( fromList )
import           Data.IORef
import           Foreign.Ptr
import           Graphics.GL
import           Graphics.GL.Compatibility43 ()
import qualified Graphics.UI.GLFW            as W
import           Foreign.Storable
import           Foreign.Marshal.Alloc       ( alloca )

import           GL.Math
import           GL.WithGL
import           GL.Texture

main :: IO ()
main = withGL 400 400 "GL Window" makeEnv binder render

makeEnv :: IO GLEnv
makeEnv = do
    vao <- createVAO
    vbo <- createVBO [ -1, -1, 0.5773, 0, 0, 0, 1, -1.154, 0.5, 0, 1, -1, 0.5773, 1, 0, 0, 1, 0, 0.5, 1 ]
    ebo <- createEBO [ 0, 3, 1, 1, 3, 2, 2, 3, 0, 0, 1, 2 ]
    tex <- createTex "image/texture.png" GL_TEXTURE0

    prog <- makeProg [ ("glsl/app/vertex.glsl", GL_VERTEX_SHADER), ("glsl/app/fragment.glsl", GL_FRAGMENT_SHADER) ]

    let sizef = sizeOf (undefined :: GLfloat)

    setupAttrib prog "position" 3 GL_FLOAT GL_FALSE (5 * sizef) 0
    setupAttrib prog "tex" 2 GL_FLOAT GL_FALSE (5 * sizef) (3 * sizef)

    let vars = [ "model", "view", "projection", "sampler" ]
    uniforms <- mapM (locateUniform prog) vars
    s'h <- newIORef 0
    s'v <- newIORef 0
    distance <- newIORef 1
    mousep <- newIORef (0, 0)
    return GLEnv { ebo' = 12
                 , distance = distance
                 , uniform = M.fromList $ zip vars uniforms
                 , s'h = s'h
                 , s'v = s'v
                 , mousep = mousep
                 }

binder :: W.Window -> GLEnv -> IO ()
binder win GLEnv{..} = do
    W.setErrorCallback $ Just errorCallback
    W.setWindowCloseCallback win $ Just closeCallback
    W.setWindowFocusCallback win $ Just focusCallback
    W.setKeyCallback win $ Just keyCallback
    W.setCursorPosCallback win $ Just cursorPosCallback
    W.setMouseButtonCallback win $ Just mouseButtonCallback
  where
    errorCallback :: W.Error -> String -> IO ()
    errorCallback e s = undefined -- putStrLn (concat [ "callback: ", show e, ": ", s ] :: String)
    closeCallback _ = putStrLn "callback: Window are closing."
    focusCallback _ W.FocusState'Focused =
        putStrLn "callback: Mouse focused"
    focusCallback _ W.FocusState'Defocused =
        putStrLn "callback: Mouse defocused"
    keyCallback win key _ action _ =
        case key of
            W.Key'Escape -> when (action == W.KeyState'Pressed) $ W.setWindowShouldClose win True
            W.Key'Up -> modifyIORef s'v (+ 0.1)
            W.Key'Down -> modifyIORef s'v (\x -> x - 0.1)
            W.Key'Left -> modifyIORef s'h (\x -> x - 0.1)
            W.Key'Right -> modifyIORef s'h (+ 0.1)
            _ -> putStrLn "callback: unhandled key event, " >> print key
    cursorPosCallback _ x y = do
        mb <- W.getMouseButton win W.MouseButton'1
        when (mb == W.MouseButtonState'Pressed) $ writeIORef mousep (x', y')
      where
        x' = fromRational . toRational $ (x - 200) / 200
        y' = fromRational . toRational $ (y - 200) / 200
    mouseButtonCallback _ m ms _ =
        putStrLn "callback: mouse, " >> print m >> print ms

render :: GLEnv -> IO ()
render GLEnv{..} = do

    s'h <- readIORef s'h
    s'v <- readIORef s'v
    distance <- readIORef distance
    (x, y) <- readIORef mousep

    let z = sqrt $ 1 - x * x - y * y
        theta = acos $ V3 x y z .*. V3 0 0 1
        v = V3 x y z .** V3 0 0 1
    let t = translate s'h s'v 10
        s = scale 1 1 1
        r = rotate (8 * theta) v
        model = t .* r .* s

    let view = lookat [ 0, 0, 0 ] [ 0, 0, 1 ] [ 0, -1, 0 ]
    let projection = perspect 1.0 (90 / 180 * pi) distance 100 .* translate 0 0 distance

    -- draw
    glDrawElements GL_TRIANGLES ebo' GL_UNSIGNED_INT nullPtr

    -- setup all uniform variables.
    let vars = zip [ "model", "view", "projection" ] [ model, view, projection ]
    let setUniformMatrix (name, mat) =
            alloca $
                \p -> do
                    poke p (mat :: M44 GLfloat)
                    glUniformMatrix4fv (uniform ! name) 1 GL_TYPE (castPtr p)
    mapM_ setUniformMatrix vars

    glUniform1i (uniform ! "sampler") GL_TEXTURE0
