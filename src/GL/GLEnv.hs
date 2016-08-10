module GL.GLEnv where

import           Data.IORef
import           Data.HashMap.Strict as M
import           Graphics.GL

import           GL.Math
import           GL.Primitive

data GLEnv = GLEnv { things     :: [Primitive]
                   , uniform    :: M.HashMap String GLint
                   , s'h        :: IORef GLfloat -- ^ horizontal movement
                   , s'v        :: IORef GLfloat -- ^ vertical movement
                   , distance   :: IORef GLfloat
                   , rotation   :: IORef (M44 GLfloat)
                   , resolution :: IORef (GLfloat, GLfloat, GLsizei, GLsizei) -- ^ window's top, left, width, height
                   , mouse      :: IORef (GLfloat, GLfloat, GLfloat, GLfloat) -- ^ x, y, click x, click y
                   , sensitive  :: GLfloat -- ^ mouse sensitity
                   }
    deriving (Eq)
