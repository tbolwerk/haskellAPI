{-# LINE 1 "Network/Socket/Posix/MsgHdr.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the POSIX 'sendmsg' system call.
module Network.Socket.Posix.MsgHdr
    ( MsgHdr(..)
    ) where




import Network.Socket.Imports
import Network.Socket.Internal (zeroMemory)

import Network.Socket.Posix.IOVec (IOVec)

data MsgHdr sa = MsgHdr
    { msgName    :: !(Ptr sa)
    , msgNameLen :: !CUInt
    , msgIov     :: !(Ptr IOVec)
    , msgIovLen  :: !CSize
    , msgCtrl    :: !(Ptr Word8)
    , msgCtrlLen :: !CInt
    , msgFlags   :: !CInt
    }

instance Storable (MsgHdr sa) where
  sizeOf    _ = (48)
{-# LINE 28 "Network/Socket/Posix/MsgHdr.hsc" #-}
  alignment _ = alignment (0 :: CInt)

  peek p = do
    name       <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))       p
{-# LINE 32 "Network/Socket/Posix/MsgHdr.hsc" #-}
    nameLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))    p
{-# LINE 33 "Network/Socket/Posix/MsgHdr.hsc" #-}
    iov        <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))        p
{-# LINE 34 "Network/Socket/Posix/MsgHdr.hsc" #-}
    iovLen     <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))     p
{-# LINE 35 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ctrl       <- ((\hsc_ptr -> peekByteOff hsc_ptr 32))    p
{-# LINE 36 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ctrlLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) p
{-# LINE 37 "Network/Socket/Posix/MsgHdr.hsc" #-}
    flags      <- ((\hsc_ptr -> peekByteOff hsc_ptr 44))      p
{-# LINE 38 "Network/Socket/Posix/MsgHdr.hsc" #-}
    return $ MsgHdr name nameLen iov iovLen ctrl ctrlLen flags

  poke p mh = do
    -- We need to zero the msg_control, msg_controllen, and msg_flags
    -- fields, but they only exist on some platforms (e.g. not on
    -- Solaris).  Instead of using CPP, we zero the entire struct.
    zeroMemory p (48)
{-# LINE 45 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))       p (msgName       mh)
{-# LINE 46 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))    p (msgNameLen    mh)
{-# LINE 47 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16))        p (msgIov        mh)
{-# LINE 48 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24))     p (msgIovLen     mh)
{-# LINE 49 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 32))    p (msgCtrl       mh)
{-# LINE 50 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) p (msgCtrlLen    mh)
{-# LINE 51 "Network/Socket/Posix/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 44))      p (msgFlags      mh)
{-# LINE 52 "Network/Socket/Posix/MsgHdr.hsc" #-}
