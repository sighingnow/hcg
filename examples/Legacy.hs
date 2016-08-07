{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Monad
import           Graphics.GL
import           Graphics.GL.Compatibility43
import qualified Graphics.UI.GLFW            as W

import           GL.WithGL

main :: IO ()
main = withGL 400 300 "GL Window" makeEnv binder render

makeEnv :: IO GLEnv
makeEnv = return $ GLEnv { } -- unused

binder :: W.Window -> GLEnv -> IO ()
binder win GLEnv{..} = do
    W.setErrorCallback $ Just errorCallback
    W.setWindowCloseCallback win $ Just closeCallback
    W.setWindowFocusCallback win $ Just focusCallback
    W.setKeyCallback win $ Just keyCallback
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
            _ -> putStrLn "callback: unhandled key event, " >> print key

render :: GLEnv -> IO ()
render GLEnv{..} = do
    glLoadIdentity
    Just t <- W.getTime
    glRotated (realToFrac t * 50) 0 0 1

    glBegin GL_TRIANGLES >> do
        glColor3f 1 0 0
        glVertex3f (-0.6) (-0.4) 0
        glColor3f 0 1 0
        glVertex3f 0.6 (-0.4) 0
        glColor3f 0 0 1
        glVertex3f 0 0.6 0
    glEnd
