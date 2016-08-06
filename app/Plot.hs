{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Maybe            ( fromJust )
import           Data.HashMap.Strict   ( (!) )
import qualified Data.HashMap.Strict   as M ( fromList )
import           Data.IORef
import qualified Data.Vector.Storable  as V
import           Foreign.Ptr
import           Graphics.GL
import qualified Graphics.UI.GLFW      as W
import           Foreign.Storable
import           Foreign.Marshal.Alloc ( alloca )

import           GL.Math
import           GL.WithGL
import qualified GL.Geometric          as G

main :: IO ()
main = withGL 400 300 "GL Window" makeEnv binder render

makeEnv :: IO GLEnv
makeEnv = do
    ball <- G.ball (V3 0 0 0) 20
    vao <- createVAO
    vbo <- createVBO $ ([ -20, 0, 0, 20, 0, 0, 0, -20, 0, 0, 20, 0, 0, 0, -20, 0, 0, 20 ] :: V.Vector GLfloat) V.++ ball
    ebo <- createEBO [ 0, 1, 2, 3, 4, 5 ]
    prog <- makeProg [ ("glsl/plot/vertex.glsl", GL_VERTEX_SHADER), ("glsl/plot/fragment.glsl", GL_FRAGMENT_SHADER) ]

    setupAttrib prog "position" 3 GL_FLOAT GL_FALSE 0 0

    let vars = [ "model", "view", "projection" ]
    uniforms <- mapM (locateUniform prog) vars
    s'h <- newIORef 0
    s'v <- newIORef 0
    distance <- newIORef 10
    mousep <- newIORef (0, 0)
    return GLEnv { ebo' = 6
                 , distance = distance
                 , uniform = M.fromList $ zip vars uniforms
                 , s'h = s'h
                 , s'v = s'v
                 , mousep = mousep
                 }

binder :: W.Window -> GLEnv -> IO ()
binder win GLEnv{..} = do
    glClearColor 1.0 1.0 1.0 0
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
    time <- fmap (fromRational . toRational . fromJust) W.getTime

    s'h <- readIORef s'h
    s'v <- readIORef s'v
    distance <- readIORef distance
    (x, y) <- readIORef mousep

    let z = sqrt $ 1 - x * x - y * y
        theta = acos $ V3 x y z .*. V3 0 0 1
        v = V3 x y z .** V3 0 0 1
    let t = translate s'h s'v 0
        s = scale 1 1 1
        r = rotate theta v
        model = t .* r .* s

    let view = lookat [ 0, 0, 0 ] [ 0, 0, 1 ] [ 0, -1, 0 ]
    -- let projection = perspect (4 / 3) (pi / 2) 0.001 60
    let projection = ortho (-30) 30 (-30) 30 (-30) 30

    glDrawArrays GL_TRIANGLE_STRIP 6 40000
    glDrawElements GL_LINES 6 GL_UNSIGNED_INT nullPtr -- GL_TRIANGLE_STRIP

    let vars = zip [ "model", "view", "projection" ] [ model, view, projection ]
    let setUniformMatrix (name, mat) =
            alloca $
                \p -> do
                    poke p (mat :: M44 GLfloat)
                    glUniformMatrix4fv (uniform ! name) 1 GL_TYPE (castPtr p)
    mapM_ setUniformMatrix vars
