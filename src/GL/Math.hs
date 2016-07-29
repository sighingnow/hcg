{-# LANGUAGE OverloadedLists #-}

module GL.Math
    ( identity
    , translate
    , scale
    , rotate
    , lookat
    , perspect
    , ortho
    , module GL.Math.Tensor
    ) where

import           GL.Math.Tensor

-- | Build a identity matrix.
identity :: Floating a => M44 a
identity = [ 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 ]

-- | Build a translation matrix.
translate :: Floating a => a -> a -> a -> M44 a
translate a b c = [ 1, 0, 0, a, 0, 1, 0, b, 0, 0, 1, c, 0, 0, 0, 1 ]

-- | Build a scaling matrix.
scale :: Floating a => a -> a -> a -> M44 a
scale a b c = [ a, 0, 0, 0, 0, b, 0, 0, 0, 0, c, 0, 0, 0, 0, 1 ]

-- | Build a Ratotion matrix.
rotate :: Floating a
       => a -- ^ alpha, in radian
       -> M44 a
rotate theta = [ cos theta, -sin theta, 0, 0, sin theta, cos theta, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 ]

lookat :: Floating a
       => V3 a -- ^ camera position
       -> V3 a -- ^ target position
       -> V3 a -- ^ up direction, if head is up, then the value is @V3 0 1 0@
       -> M44 a
lookat e target up = [ u1, u2, u3, 0, v1, v2, v3, 0, n1, n2, n3, 0, 0, 0, 0, 1 ]
  where
    n@(V3 n1 n2 n3) = norm $ target - e -- back
    u@(V3 u1 u2 u3) = norm $ up .** n          -- right
    (V3 v1 v2 v3) = norm $ n .** u            -- up

-- | Build a perspective projection matrix.
perspect :: Floating a
         => a -- ^ aspect ratio, w / h
         -> a -- ^ field of view, in radian
         -> a -- ^ near, z-axis
         -> a -- ^ far, z-axis
         -> M44 a
perspect ar alpha near far =
    [ x, 0, 0, 0, 0, x / ar, 0, 0, 0, 0, z, w, 0, 0, 1, 0 ]
  where
    x = 1 / tan (alpha / 2)
    fpn = far + near
    fmn = far - near
    z = fpn / fmn
    w = -2 * near * far / fmn

-- | Build a orthonomal perspective projection matrix.
ortho :: Floating a
      => a -- ^ left
      -> a -- ^ right
      -> a -- ^ bottom
      -> a -- ^ top
      -> a -- ^ near
      -> a -- ^ far
      -> M44 a
ortho l r b t n f = [ -2 * x, 0, 0, (r + l) * x, 0, -2 * y, 0, (t + b) * y, 0, 0, 2 * z, (f + n) * z, 0, 0, 0, 1 ]
  where
    x = 1 / (l - r)
    y = 1 / (b - t)
    z = 1 / (n - f)
