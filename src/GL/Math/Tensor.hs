-- | Re-export types and useful functions from the linear package.
-- And provide orphan instance for IsList typeclass, will work with
-- the @OverLoadingLists@ extension of GHC.
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GL.Math.Tensor
    ( Epsilon(..)
    , V1(..)
    , V2(..)
    , V3(..)
    , V4(..)
    , M33
    , M44
    , (!*!)
    , (!+!)
    , (!-!)
    , (!*)
    , dot
    , trace
    , cross
    , transpose
    , normalize
    ) where

import           GHC.Exts ( IsList(..) )
import           Linear

instance IsList (V1 a) where
    type Item (V1 a) = a
    fromList [ x ] = V1 x
    toList (V1 x) = [ x ]

instance IsList (V2 a) where
    type Item (V2 a) = a
    fromList [ x1, x2 ] = V2 x1 x2
    toList (V2 x1 x2) = [ x1, x2 ]

instance IsList (V3 a) where
    type Item (V3 a) = a
    fromList [ x1, x2, x3 ] =
        V3 x1 x2 x3
    toList (V3 x1 x2 x3) = [ x1, x2, x3 ]

instance IsList (V4 a) where
    type Item (V4 a) = a
    fromList [ x1, x2, x3, x4 ] =
        V4 x1 x2 x3 x4
    toList (V4 x1 x2 x3 x4) =
        [ x1, x2, x3, x4 ]
