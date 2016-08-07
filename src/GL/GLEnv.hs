module GL.GLEnv where

import           Data.IORef
import           Data.HashMap.Strict as M
import           Graphics.GL

import           GL.Primitive

data GLEnv = GLEnv { ebo'       :: GLsizei
                   , distance   :: IORef GLfloat
                   , uniform    :: M.HashMap String GLint
                   , s'h        :: IORef GLfloat -- ^ horizontal movement
                   , s'v        :: IORef GLfloat -- ^ vertical movement
                   , mousep     :: IORef (GLfloat, GLfloat) -- ^ cursor position with left mouse button pressed.
                   , resolution :: IORef (GLfloat, GLfloat, GLsizei, GLsizei) -- ^ window's top, left, width, height
                   , mouse      :: IORef (GLfloat, GLfloat, GLfloat, GLfloat) -- ^ x, y, click x, click y
                   , things     :: [Primitive]
                   }
    deriving (Eq)
