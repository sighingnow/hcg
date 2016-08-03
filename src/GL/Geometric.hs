module GL.Geometric ( ball ) where

import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign.Storable

import           GL.Math

loopM_ :: Int -> (Int -> IO ()) -> IO ()
loopM_ n f = go 0
  where
    go k
        | k >= n = return ()
        | k < n = f k >> go (k + 1)

ball :: (Floating a, Storable a) => V3 a -> a -> IO (V.Vector a)
ball (V3 x1 x2 x3) r = do
    vec <- MV.new $ 3 * count * (4 * count)
    loopM_ count (calc vec)
    V.unsafeFreeze vec
  where
    calc vec n = loopM_ (2 * count) (go vec)
      where
        go vec k = do
            MV.write vec (p + 0) $ x1 + r1 * c
            MV.write vec (p + 1) $ x2 + r1 * s
            MV.write vec (p + 2) $ x3 + h1
            MV.write vec (p + 3) $ x1 + r2 * c
            MV.write vec (p + 4) $ x2 + r2 * s
            MV.write vec (p + 5) $ x3 + h2
          where
            c = cos beta
            s = sin beta
            p = 3 * (n * 4 * count + 2 * k)
            beta = 0 + delta * fromIntegral k
        r1 = r * cos alpha
        r2 = r * cos (alpha + delta)
        h1 = r * sin alpha
        h2 = r * sin (alpha + delta)
        alpha = -pi / 2 + delta * fromIntegral n
    count = 100 :: Int
    delta = pi / fromIntegral count
