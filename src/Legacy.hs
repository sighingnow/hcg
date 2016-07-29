{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

import           Graphics.GL
import           Graphics.GL.Compatibility43
import qualified Graphics.UI.GLFW            as W

import           GL.WithGL

main :: IO ()
main = withGL 400 300 "GL Window" makeEnv render

makeEnv :: IO GLEnv
makeEnv = return $ GLEnv (-1) -- unused

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
