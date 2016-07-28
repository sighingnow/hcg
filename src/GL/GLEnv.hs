module GL.GLEnv where

import           Graphics.GL

data GLEnv = GLEnv { vbo :: GLuint }
    deriving (Eq, Show)
