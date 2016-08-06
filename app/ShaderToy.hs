{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Monad
import           Data.Maybe                  ( fromJust )
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
    vbo <- createVBO [ -1, -1, 0, 1, -1, 0, 1, 1, 0, -1, 1, 0 ]
    ebo <- createEBO [ 0, 1, 2, 0, 2, 3 ]

    createTex "image/tex00.jpg" GL_TEXTURE0
    createTex "image/tex01.jpg" GL_TEXTURE1

    prog <- makeProg [ ("glsl/shadertoy/vertex.glsl", GL_VERTEX_SHADER)
                     , ("glsl/shadertoy/fragment.glsl", GL_FRAGMENT_SHADER)
                     ]
    setupAttrib prog "vertex" 3 GL_FLOAT GL_FALSE 0 0

    let vars = [ "iGlobalTime", "iResolution", "iMouse", "iChannel0", "iChannel1", "iDate", "iSampleRate" ]
    uniforms <- mapM (locateUniform prog) vars
    mouse <- newIORef (0, 0, 0, 0)
    resolution <- newIORef (0, 0, 400, 400)
    return GLEnv { uniform = M.fromList $ zip vars uniforms, mouse = mouse, resolution = resolution }

binder :: W.Window -> GLEnv -> IO ()
binder win GLEnv{..} = do
    W.setErrorCallback $ Just errorCallback
    W.setWindowCloseCallback win $ Just closeCallback
    W.setWindowFocusCallback win $ Just focusCallback
    W.setWindowPosCallback win $ Just posCallback
    W.setWindowSizeCallback win $ Just sizeCallback
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
    posCallback _ t l = modifyIORef' resolution (\(_, _, w, h) -> (cast t, cast l, w, h))
    sizeCallback _ w h = modifyIORef' resolution (\(t, l, _, _) -> (t, l, fromIntegral w, fromIntegral h))
    cursorPosCallback win x y =
        modifyIORef' mouse (\(_, _, cx, cy) -> (cast x, cast y, cx, cy))
    mouseButtonCallback win m ms _
        | m == W.MouseButton'1 = case ms of
              W.MouseButtonState'Pressed -> do
                  (cx, cy) <- W.getCursorPos win
                  modifyIORef' mouse (\(x, y, _, _) -> (x, y, cast cx, cast cy))
              _ -> modifyIORef' mouse (\(x, y, _, _) -> (x, y, -1, -1))
        | otherwise = return ()

render :: GLEnv -> IO ()
render GLEnv{..} = do
    t <- fmap (cast . fromJust) W.getTime
    (_, _, w, h) <- readIORef resolution
    mouse' <- readIORef mouse
    glUniform1f (uniform ! "iGlobalTime") t
    glUniform3f (uniform ! "iResolution") (cast w) (cast h) 0.0
    glUniform4f (uniform ! "iMouse") (mouse' ^. _1) (mouse' ^. _2) (mouse' ^. _3) (mouse' ^. _4)
    glUniform1i (uniform ! "iChannel0") GL_TEXTURE0
    glUniform1i (uniform ! "iChannel1") GL_TEXTURE1

    glViewport 0 0 w h
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
