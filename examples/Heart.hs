{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Maybe                  ( fromJust )
import           Data.HashMap.Strict         ( (!) )
import           Data.IORef
import           Graphics.GL
import qualified Graphics.UI.GLFW            as W
import qualified Data.HashMap.Strict         as M ( fromList )

import           GL.Foreign
import           GL.Math
import           GL.WithGL
import           GL.Texture
import           GL.Primitive
import           GL.GLSL

main :: IO ()
main = withGL 400 400 "GL Window" makeEnv binder render

makeEnv :: IO GLEnv
makeEnv = do
    p1 <- createArray GL_LINES [ -1, 0, 0, 1, 0, 0 ] (0, 2)
    p2 <- createArray GL_LINES [ 0, -1, 0, 0, 1, 0 ] (0, 2)
    p3 <- createElement GL_TRIANGLES
                        [ -0.5, -0.5, 0, 0.5, -0.5, 0, 0.5, 0.5, 0, -0.5, 0.5, 0 ]
                        [ 0, 1, 2, 0, 2, 3 ]
                        (0, 6)

    prog <- program [ ("glsl/heart/vertex.glsl", GL_VERTEX_SHADER)
                    , ("glsl/heart/fragment.glsl", GL_FRAGMENT_SHADER)
                    ]

    let vars = [ "model", "view", "projection" ]
    uniforms <- mapM (locateUniform prog) vars
    s'h <- newIORef 0
    s'v <- newIORef 0
    distance <- newIORef 1
    mouse <- newIORef (0, 0, 0, 0)
    return GLEnv { things = [ p1, p2, p3 ]
                 , uniform = M.fromList $ zip vars uniforms
                 , s'h = s'h
                 , s'v = s'v
                 , distance = distance
                 , mouse = mouse
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
        when (mb == W.MouseButtonState'Pressed) $
            modifyIORef' mouse $ \(_, _, cx, cy) -> (x', y', cx, cy)
      where
        x' = fromRational . toRational $ (x - 200) / 200
        y' = fromRational . toRational $ (y - 200) / 200
    mouseButtonCallback _ m ms _ =
        putStrLn "callback: mouse, " >> print m >> print ms

render :: GLEnv -> IO ()
render GLEnv{..} = do
    time <- fmap (cast . fromJust) W.getTime

    s'h <- readIORef s'h
    s'v <- readIORef s'v
    distance <- readIORef distance
    (x, y, _, _) <- readIORef mouse

    let z = sqrt $ 1 - x * x - y * y
        theta = acos $ V3 x y z `dot` V3 0 0 1
        v = V3 x y z `cross` V3 0 0 1
    let t = translate s'h s'v 10
        s = scale 1 1 1
        r = rotate (8 * theta) v -- time
        model = t !*! r !*! s

    let view = lookat [ 0, 0, 0 ] [ 0, 0, -1 ] [ 0, 1, 0 ]
    let projection = perspect 1.0 (90 / 180 * pi) distance 100 !*! translate 0 0 distance

    mapM_ primitive things

    let vars = zip [ "model", "view", "projection" ] [ model, view, projection ]
    let setUniformMatrix (name, mat) =
            alloca $
                \p -> do
                    poke p (mat :: M44 GLfloat)
                    glUniformMatrix4fv (uniform ! name) 1 GL_TYPE (castPtr p)
    mapM_ setUniformMatrix vars
