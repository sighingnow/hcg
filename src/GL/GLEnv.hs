module GL.GLEnv where

import           Data.IORef
import           Data.HashMap.Strict as M
import           Graphics.GL

data GLEnv = GLEnv { ebo'     :: GLsizei
                   , distance :: IORef GLfloat
                   , uniform  :: M.HashMap String GLint
                   , s'h      :: IORef GLfloat -- ^ horizontal movement
                   , s'v      :: IORef GLfloat -- ^ vertical movement
                   , mousep   :: IORef (GLfloat, GLfloat) -- ^ cursor position with left mouse button pressed.
                   }
    deriving (Eq)
