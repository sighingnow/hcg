{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.HashMap.Strict         ( (!) )
import qualified Data.HashMap.Strict         as M ( fromList )
import           Data.IORef
import           Graphics.GL
import qualified Graphics.UI.GLFW            as W

import           GL.Foreign
import           GL.Math
import           GL.WithGL
import           GL.Texture
import           GL.GLSL
import           GL.Primitive

main :: IO ()
main = withGL 400 400 "GL Window" makeEnv binder render

makeEnv :: IO GLEnv
makeEnv = do
    p <- createElementWithTex GL_TRIANGLES
                              [ -1
                              , -1
                              , 0.5773
                              , 0
                              , 0
                              , 0
                              , 1
                              , -1.154
                              , 0.5
                              , 0
                              , 1
                              , -1
                              , 0.5773
                              , 1
                              , 0
                              , 0
                              , 1
                              , 0
                              , 0.5
                              , 1
                              ]
                              [ 0, 3, 1, 1, 3, 2, 2, 3, 0, 0, 1, 2 ]
                              (0, 12)

    tex <- createTex "image/texture.png" GL_TEXTURE0

    prog <- program [ ("glsl/app/vertex.glsl", GL_VERTEX_SHADER)
                    , ("glsl/app/fragment.glsl", GL_FRAGMENT_SHADER)
                    ]

    let vars = [ "model", "view", "projection", "sampler" ]
    uniforms <- mapM (locateUniform prog) vars
    s'h <- newIORef 0
    s'v <- newIORef 0
    distance <- newIORef 1
    mouse <- newIORef (0, 0, 0, 0)
    return GLEnv { things = [p]
                 , uniform = M.fromList $ zip vars uniforms
                 , s'h = s'h
                 , s'v = s'v
                 , distance = distance
                 , sensitive = 2
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
        (x0, y0, cx0, cy0) <- readIORef mouse
        (_, _, w, h) <- readIORef resolution
        let x' = cast x
            y' = cast y
            w' = fromIntegral w
            h' = fromIntegral h
        writeIORef mouse (x', y', cx0, cy0)
        mb <- W.getMouseButton win W.MouseButton'1
        when (mb == W.MouseButtonState'Pressed) $ do
            let v1 = normalize (V3 (x0-w'/2) (y0-h'/2) ((w'+h')/4))
                v2 = normalize (V3 (x'-w'/2) (y'-h'/2) ((w'+h')/4))
                theta = acos (v1 `dot` v2)
                v = v1 `cross` v2
            modifyIORef' rotation (rotate (theta * sensitive) v !*!)
    mouseButtonCallback _ m ms _ =
        putStrLn "callback: mouse, " >> print m >> print ms

render :: GLEnv -> IO ()
render GLEnv{..} = do
    s'h <- readIORef s'h
    s'v <- readIORef s'v
    distance <- readIORef distance
    (x, y, _, _) <- readIORef mouse

    r <- readIORef rotation
    let t = translate s'h s'v 10
        s = scale 1 1 1
        model = t !*! r !*! s

    let view = lookat [ 0, 0, 0 ] [ 0, 0, 1 ] [ 0, -1, 0 ]
    let projection = perspect 1.0 (90 / 180 * pi) distance 100 !*! translate 0 0 distance

    -- draw
    mapM_ primitive things

    -- setup all uniform variables.
    let vars = zip [ "model", "view", "projection" ] [ model, view, projection ]
    let setUniformMatrix (name, mat) =
            alloca $
                \p -> do
                    poke p (mat :: M44 GLfloat)
                    glUniformMatrix4fv (uniform ! name) 1 GL_TYPE (castPtr p)
    mapM_ setUniformMatrix vars

    glUniform1i (uniform ! "sampler") GL_TEXTURE0
