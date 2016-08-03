{-# LANGUAGE OverloadedLists #-}

module GL.Math
    ( identity
    , translate
    , scale
    , rotate
    , lookat
    , ortho
    , perspect
    , frustum
    , frustum'inv
    , module GL.Math.Tensor
    ) where

import           GL.Math.Tensor

-- | Build a identity matrix.
identity :: Floating a => M44 a
identity = [ 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 ]

{-# INLINE identity #-}

-- | Build a translation matrix.
translate :: Floating a => a -> a -> a -> M44 a
translate a b c = [ 1, 0, 0, a, 0, 1, 0, b, 0, 0, 1, c, 0, 0, 0, 1 ]

{-# INLINE translate #-}

-- | Build a scaling matrix.
scale :: Floating a => a -> a -> a -> M44 a
scale a b c = [ a, 0, 0, 0, 0, b, 0, 0, 0, 0, c, 0, 0, 0, 0, 1 ]

{-# INLINE scale #-}

-- | Build a Ratotion matrix.
rotate :: (Eq a, Floating a)
       => a -- ^ alpha, in radian
       -> V3 a -- ^ rotation axis
       -> M44 a
rotate theta v
    | v == V3 0 0 0 = identity
    | theta == 0 = identity
    | otherwise = [ u11
                  , u12 - s * u3
                  , u13 + s * u2
                  , 0
                  , u12 + s * u3
                  , u22
                  , u23 - s * u1
                  , 0
                  , u13 - s * u2
                  , u23 + s * u1
                  , u33
                  , 0
                  , 0
                  , 0
                  , 0
                  , 1
                  ]
  where
    c = cos theta
    s = sin theta
    u@(V3 u1 u2 u3) = norm v
    u11 = c + (1 - c) * u1 * u1
    u22 = c + (1 - c) * u2 * u2
    u33 = c + (1 - c) * u3 * u3
    u12 = (1- c) * u1 * u2
    u13 = (1- c) * u1 * u3
    u23 = (1- c) * u2 * u3

{-# INLINE rotate #-}

-- | Camera transform matrix, right hand coordinate, default value:
--
-- > view = lookat [ 0, 0, 0 ] [ 0, 0, -1 ] [ 0, 1, 0 ]
lookat :: Floating a
       => V3 a -- ^ camera position
       -> V3 a -- ^ target position
       -> V3 a -- ^ up direction, if head is up, then the value is @V3 0 1 0@
       -> M44 a
lookat e@(V3 x1 x2 x3) target up =
    [ u1, u2, u3, -u .*. e, v1, v2, v3, -v .*. e, -n1, -n2, -n3, n .*. e, 0, 0, 0, 1 ]
  where
    n@(V3 n1 n2 n3) = norm $ target - e  -- forward
    u@(V3 u1 u2 u3) = norm $ n .** up    -- side
    v@(V3 v1 v2 v3) = norm $ u .** n     -- up

{-# INLINE lookat #-}

-- | Build a orthonomal perspective projection matrix, right hand coordinate.
ortho :: Floating a
      => a -- ^ left
      -> a -- ^ right
      -> a -- ^ bottom
      -> a -- ^ top
      -> a -- ^ near
      -> a -- ^ far
      -> M44 a
ortho l r b t n f = [ 2 * x, 0, 0, -(r + l) * x, 0, 2 * y, 0, -(t + b) * y, 0, 0, -2 * z, -(f + n) * z, 0, 0, 0, 1 ]
  where
    x = 1 / (r - l)
    y = 1 / (t - b)
    z = 1 / (f - n)

{-# INLINE ortho #-}

-- | Build a perspective projection matrix, right hand coordinate.
perspect :: Floating a
         => a -- ^ aspect ratio, w / h
         -> a -- ^ field of view, in radian
         -> a -- ^ near, z-axis
         -> a -- ^ far, z-axis
         -> M44 a
perspect aspect alpha near far =
    [ x / aspect, 0, 0, 0, 0, x, 0, 0, 0, 0, z, w, 0, 0, -1, 0 ]
  where
    x = 1 / tan (alpha / 2)
    fpn = far + near
    fmn = far - near
    z = -fpn / fmn
    w = -2 * near * far / fmn

{-# INLINE perspect #-}

-- | Build a perspective projection matrix using frustum arguments, right hand coordinate.
--
-- For example, /frustum (-2) 2 (-2)  2 10 100 = perspect 1.0 0.1974 10 100/.
frustum :: Floating a
        => a -- ^ left
        -> a -- ^ right
        -> a -- ^ bottom
        -> a -- ^ top
        -> a -- ^ near
        -> a -- ^ far
        -> M44 a
frustum l r b t n f = [ n' / x, 0, x' / x, 0, 0, n' / y, y' / y, 0, 0, 0, -z' / z, -z'' / z, 0, 0, -1, 0 ]
  where
    n' = 2 * n
    x = r - l
    x' = r + l
    y = t - b
    y' = t + b
    z = f - n
    z' = f + n
    z'' = 2 * n * f

{-# INLINE frustum #-}

-- | Inverse matrix of frustum.
frustum'inv :: Floating a
            => a -- ^ left
            -> a -- ^ right
            -> a -- ^ bottom
            -> a -- ^ top
            -> a -- ^ near
            -> a -- ^ far
            -> M44 a
frustum'inv l r b t n f =
    [ x / n', 0, 0, x' / n', 0, y / n', 0, y' / n', 0, 0, 0, -1, 0, 0, -z / z'', z' / z'' ]
  where
    n' = 2 * n
    x = r - l
    x' = r + l
    y = t - b
    y' = t + b
    z = f - n
    z' = f + n
    z'' = n' * f

{-# INLINE frustum'inv #-}
