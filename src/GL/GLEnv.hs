module GL.GLEnv where

import           Graphics.GL
import           Data.HashMap.Strict as M

data GLEnv = GLEnv { vbo :: GLuint
                   , ibo :: GLuint
                   , uniform :: M.HashMap String GLint
                   }
    deriving (Eq, Show)
