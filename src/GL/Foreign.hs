module GL.Foreign
    ( peekFrom
    , pokeTo
    ) where

import           Control.Monad          ( liftM2, unless, when )
import           Control.Monad.IO.Class ( MonadIO(..), liftIO )
import           Foreign.Ptr
import           Foreign.Marshal.Alloc  ( alloca )
import           Foreign.Storable

-- | Allocate memory, apply this function to the pointer and return the result value there.
peekFrom :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
peekFrom f = liftIO . alloca $ liftM2 (>>) f peek

-- | Allocate memory, put value into there and apply this function to the pointer.
pokeTo :: (MonadIO m, Storable a) => a -> (Ptr a -> IO b) -> m b
pokeTo v f = liftIO . alloca $ liftM2 (>>) (`poke` v) f
