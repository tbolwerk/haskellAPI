{-# LINE 1 "Network/Socket/Posix/IOVec.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the POSIX writev system call.
module Network.Socket.Posix.IOVec
    ( IOVec(..)
    , withIOVec
    ) where

import Foreign.Marshal.Array (allocaArray)

import Network.Socket.Imports




data IOVec = IOVec
    { iovBase :: !(Ptr Word8)
    , iovLen  :: !CSize
    }

instance Storable IOVec where
  sizeOf    _ = (16)
{-# LINE 23 "Network/Socket/Posix/IOVec.hsc" #-}
  alignment _ = alignment (0 :: CInt)

  peek p = do
    base <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 27 "Network/Socket/Posix/IOVec.hsc" #-}
    len  <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))  p
{-# LINE 28 "Network/Socket/Posix/IOVec.hsc" #-}
    return $ IOVec base len

  poke p iov = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (iovBase iov)
{-# LINE 32 "Network/Socket/Posix/IOVec.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))  p (iovLen  iov)
{-# LINE 33 "Network/Socket/Posix/IOVec.hsc" #-}

-- | @withIOVec cs f@ executes the computation @f@, passing as argument a pair
-- consisting of a pointer to a temporarily allocated array of pointers to
-- IOVec made from @cs@ and the number of pointers (@length cs@).
-- /Unix only/.
withIOVec :: [(Ptr Word8, Int)] -> ((Ptr IOVec, Int) -> IO a) -> IO a
withIOVec [] f = f (nullPtr, 0)
withIOVec cs f =
    allocaArray csLen $ \aPtr -> do
        zipWithM_ pokeIov (ptrs aPtr) cs
        f (aPtr, csLen)
  where
    csLen = length cs
    ptrs = iterate (`plusPtr` sizeOf (IOVec nullPtr 0))
    pokeIov ptr (sPtr, sLen) = poke ptr $ IOVec sPtr (fromIntegral sLen)
