{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GL.Math.Tensor where

import           Control.Exception     ( assert )
import           Control.Monad         ( ap, void )
import           Data.Foldable         ( foldlM )
import           Data.Traversable      ( Traversable(..), mapAccumL )
import           Data.List.Split       ( chunksOf )
import           Foreign.Marshal.Array ( advancePtr )
import           Foreign.Ptr           ( Ptr, castPtr, plusPtr )
import           Foreign.Storable      ( Storable(..) )
import           GHC.Exts              ( IsList(..) )

-- | A one-dimensional vector.
data V1 a = V1 {-# UNPACK #-} !a
    deriving (Eq, Ord, Show)

instance Num a => Num (V1 a) where
    V1 x + V1 x' = V1 (x + x')
    V1 x * V1 x' = V1 (x * x')
    abs = fmap abs
    signum = fmap signum
    fromInteger = V1 . fromInteger
    negate = fmap negate

instance Fractional a => Fractional (V1 a) where
    fromRational = V1 . fromRational
    recip = fmap recip

instance Floating a => Floating (V1 a) where
    pi = V1 pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

instance Functor V1 where
    fmap f (V1 x) = V1 (f x)

instance Applicative V1 where
    pure = V1
    V1 f <*> V1 x = V1 (f x)

instance Foldable V1 where
    foldr f a (V1 x) = x `f` a
    foldl f a (V1 x) = a `f` x
    foldr1 _ (V1 x) = x
    foldl1 _ (V1 x) = x

instance Traversable V1 where
    traverse f (V1 x) = pure V1 <*> f x
    sequenceA (V1 x) = pure V1 <*> x
    mapM f (V1 x) = return V1 `ap` f x
    sequence (V1 x) = return V1 `ap` x

instance Storable a => Storable (V1 a) where
    sizeOf ~(V1 s) = sizeOf s
    alignment ~(V1 s) = alignment s
    peek = peekAll
    poke = pokeAll

-- | A two-dimensional vector.
data V2 a = V2 {-# UNPACK #-} !a {-# UNPACK #-} !a
    deriving (Eq, Ord, Show)

instance Num a => Num (V2 a) where
    V2 x1 x2 + V2 x1' x2' = V2 (x1 + x1') (x2 + x2')
    V2 x1 x2 * V2 x1' x2' = V2 (x1 * x1') (x2 * x2')
    abs = fmap abs
    signum = fmap signum
    fromInteger v = V2 (fromInteger v) (fromInteger v)
    negate = fmap negate

instance Fractional a => Fractional (V2 a) where
    fromRational v = V2 (fromRational v) (fromRational v)
    recip = fmap recip

instance Floating a => Floating (V2 a) where
    pi = V2 pi pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

instance Functor V2 where
    fmap f (V2 x y) = V2 (f x) (f y)

instance Applicative V2 where
    pure a = V2 a a
    V2 f g <*> V2 x y = V2 (f x) (g y)

instance Foldable V2 where
    foldr f a (V2 x y) = x `f` (y `f` a)
    foldl f a (V2 x y) = (a `f` x) `f` y
    foldr1 f (V2 x y) = x `f` y
    foldl1 f (V2 x y) = x `f` y

instance Traversable V2 where
    traverse f (V2 x y) = pure V2 <*> f x <*> f y
    sequenceA (V2 x y) = pure V2 <*> x <*> y
    mapM f (V2 x y) = return V2 `ap` f x `ap` f y
    sequence (V2 x y) = return V2 `ap` x `ap` y

instance Storable a => Storable (V2 a) where
    sizeOf ~(V2 x _) = 2 * sizeOf x
    alignment ~(V2 x _) = alignment x
    peek = peekAll
    poke = pokeAll

-- | A three-dimensional vector.
data V3 a = V3 {-# UNPACK #-} !a {-# UNPACK #-} !a {-# UNPACK #-} !a
    deriving (Eq, Ord, Show)

instance Num a => Num (V3 a) where
    V3 x1 x2 x3 + V3 x1' x2' x3' =
        V3 (x1 + x1') (x2 + x2') (x3 + x3')
    V3 x1 x2 x3 * V3 x1' x2' x3' =
        V3 (x1 * x1') (x2 * x2') (x3 * x3')
    abs = fmap abs
    signum = fmap signum
    fromInteger v = V3 (fromInteger v) (fromInteger v) (fromInteger v)
    negate = fmap negate

instance Fractional a => Fractional (V3 a) where
    fromRational v = V3 (fromRational v) (fromRational v) (fromRational v)
    recip = fmap recip

instance Floating a => Floating (V3 a) where
    pi = V3 pi pi pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

instance Functor V3 where
    fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Applicative V3 where
    pure a = V3 a a a
    V3 f g h <*> V3 x y z = V3 (f x) (g y) (h z)

instance Foldable V3 where
    foldr f a (V3 x y z) = x `f` (y `f` (z `f` a))
    foldl f a (V3 x y z) = ((a `f` x) `f` y) `f` z
    foldr1 f (V3 x y z) = x `f` (y `f` z)
    foldl1 f (V3 x y z) = (x `f` y) `f` z

instance Traversable V3 where
    traverse f (V3 x y z) = pure V3 <*> f x <*> f y <*> f z
    sequenceA (V3 x y z) = pure V3 <*> x <*> y <*> z
    mapM f (V3 x y z) = return V3 `ap` f x `ap` f y `ap` f z
    sequence (V3 x y z) = return V3 `ap` x `ap` y `ap` z

instance Storable a => Storable (V3 a) where
    sizeOf ~(V3 x _ _) = 3 * sizeOf x
    alignment ~(V3 x _ _) = alignment x
    peek = peekAll
    poke = pokeAll

-- | A four-dimensional vector.
data V4 a = V4 {-# UNPACK #-} !a {-# UNPACK #-} !a {-# UNPACK #-} !a {-# UNPACK #-} !a
    deriving (Eq, Ord, Show)

instance Num a => Num (V4 a) where
    V4 x1 x2 x3 x4 + V4 x1' x2' x3' x4' =
        V4 (x1 + x1') (x2 + x2') (x3 + x3') (x4 + x4')
    V4 x1 x2 x3 x4 * V4 x1' x2' x3' x4' =
        V4 (x1 * x1') (x2 * x2') (x3 * x3') (x4 * x4')
    abs = fmap abs
    signum = fmap signum
    fromInteger v = V4 (fromInteger v) (fromInteger v) (fromInteger v) (fromInteger v)
    negate = fmap negate

instance Fractional a => Fractional (V4 a) where
    fromRational v = V4 (fromRational v) (fromRational v) (fromRational v) (fromRational v)
    recip = fmap recip

instance Floating a => Floating (V4 a) where
    pi = V4 pi pi pi pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

instance Functor V4 where
    fmap f (V4 x y z w) = V4 (f x) (f y) (f z) (f w)

instance Applicative V4 where
    pure a = V4 a a a a
    V4 f g h i <*> V4 x y z w =
        V4 (f x) (g y) (h z) (i w)

instance Foldable V4 where
    foldr f a (V4 x y z w) =
        x `f` (y `f` (z `f` (w `f` a)))
    foldl f a (V4 x y z w) =
        (((a `f` x) `f` y) `f` z) `f` w
    foldr1 f (V4 x y z w) = x `f` (y `f` (z `f` w))
    foldl1 f (V4 x y z w) = ((x `f` y) `f` z) `f` w

instance Traversable V4 where
    traverse f (V4 x y z w) =
        pure V4 <*> f x <*> f y <*> f z <*> f w
    sequenceA (V4 x y z w) =
        pure V4 <*> x <*> y <*> z <*> w
    mapM f (V4 x y z w) = return V4 `ap` f x `ap` f y `ap` f z `ap` f w
    sequence (V4 x y z w) = return V4 `ap` x `ap` y `ap` z `ap` w

instance Storable a => Storable (V4 a) where
    sizeOf ~(V4 x _ _ _) = 4 * sizeOf x
    alignment ~(V4 x _ _ _) =
        alignment x
    peek = peekAll
    poke = pokeAll

peekAll :: (Applicative t, Traversable t, Storable a) => Ptr (t a) -> IO (t a)
peekAll = Data.Traversable.mapM peek . addresses
  where
    addresses = snd . mapAccumL nextPtr 0 . pure . castPtr
    nextPtr offset ptr = (offset + 1, advancePtr ptr offset)

pokeAll :: (Foldable t, Storable a) => Ptr (t a) -> t a -> IO ()
pokeAll ptr = void . foldlM pokeAndAdvance (castPtr ptr)
  where
    pokeAndAdvance ptr value = do
        poke ptr value
        return $ ptr `plusPtr` sizeOf value

-- Matrices.
newtype M33 a = M33 { unM33 :: V3 (V3 a) }
    deriving (Num, Fractional, Floating, Eq, Ord, Functor, Foldable, Traversable, Storable, Show)

newtype M44 a = M44 { unM44 :: V4 (V4 a) }
    deriving (Num, Fractional, Floating, Eq, Ord, Functor, Foldable, Traversable, Storable, Show)

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

instance IsList (M33 a) where
    type Item (M33 a) = a
    fromList xs = assert (length xs == 9) (M33 . fromList . map fromList . chunksOf 3 $ xs)
    toList = concat . toList . fmap toList . unM33

instance IsList (M44 a) where
    type Item (M44 a) = a
    fromList xs = assert (length xs == 16) (M44 . fromList . map fromList . chunksOf 4 $ xs)
    toList = concat . toList . fmap toList . unM44

class (Functor t, Foldable t) => Vec (t :: * -> *) where
    -- | inner product
    (.*.) :: Num a => t a -> t a -> a
    -- | cross product
    (.**) :: Num a => t a -> t a -> t a

infixl 7 .*.
infix 7 .**

instance Vec V1 where
    V1 x .*. V1 x' = x * x'
    {-# INLINE (.*.) #-}

instance Vec V2 where
    V2 x1 x2 .*. V2 x1' x2' =
        x1 * x1' + x2 * x2'
    {-# INLINE (.*.) #-}

instance Vec V3 where
    V3 x1 x2 x3 .*. V3 x1' x2' x3' =
        x1 * x1' + x2 * x2' + x3 * x3'
    {-# INLINE (.*.) #-}
    V3 x1 x2 x3 .** V3 x1' x2' x3' =
        V3 (x2 * x3' - x2' * x3)
           (x3 * x1' - x3' * x1)
           (x1 * x2' - x1' * x2)
    {-# INLINE (.**) #-}

instance Vec V4 where
    V4 x1 x2 x3 x4 .*. V4 x1' x2' x3' x4' =
        x1 * x1' + x2 * x2' + x3 * x3' + x4 * x4'
    {-# INLINE (.*.) #-}

class (Functor t, Foldable t) => Tensor (t :: * -> *) where
    type TElem t :: * -> *
    -- | matrix muliplication.
    (.*) :: Num a => t a -> t a -> t a
    -- | linear transformation.
    (.->) :: Num a => t a -> TElem t a -> TElem t a
    -- | matrix transposition.
    transpose :: t a -> t a

infixl 8 .*

infix 4 .->

instance Tensor M33 where
    type TElem M33 = V3
    m1 .* m2 = M33 $ dot (unM33 m1) (unM33 . transpose $ m2)
        where dot m1' m2' = fmap (\v -> fmap (v .*.) m2') m1'
    {-# INLINE (.*) #-}
    M33 m .-> v = fmap (.*. v) m
    {-# INLINE (.->) #-}
    transpose (M33 (V3 (V3 x11 x12 x13) (V3 x21 x22 x23) (V3 x31 x32 x33))) =
        M33 $ V3 (V3 x11 x21 x31) (V3 x12 x22 x32) (V3 x13 x23 x33)
    {-# INLINE transpose #-}

instance Tensor M44 where
    type TElem M44 = V4
    m1 .* m2 = M44 $ dot (unM44 m1) (unM44 . transpose $ m2)
        where dot m1' m2' = fmap (\v -> fmap (v .*.) m2') m1'
    {-# INLINE (.*) #-}
    M44 m .-> v = fmap (.*. v) m
    {-# INLINE (.->) #-}
    transpose (M44 (V4 (V4 x11 x12 x13 x14) (V4 x21 x22 x23 x24) (V4 x31 x32 x33 x34) (V4 x41 x42 x43 x44))) =
        M44 $ V4 (V4 x11 x21 x31 x41) (V4 x12 x22 x32 x42) (V4 x13 x23 x33 x43) (V4 x14 x24 x34 x44)
    {-# INLINE transpose #-}

-- | Norm all value to /0-1/.
norm :: (Foldable f, Functor f, Floating a, Num (f a)) => f a -> f a
norm m = fmap (/ (sqrt . sum) (m * m)) m

{-# INLINE norm #-}
