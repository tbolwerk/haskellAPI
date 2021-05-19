{-# LINE 1 "Network/Socket/Posix/CmsgHdr.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}



module Network.Socket.Posix.CmsgHdr (
    Cmsg(..)
  , withCmsgs
  , parseCmsgs
  ) where




import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.ForeignPtr
import qualified Data.ByteString as B
import Data.ByteString.Internal

import Network.Socket.Imports
import Network.Socket.Posix.Cmsg
import Network.Socket.Posix.MsgHdr
import Network.Socket.Types

data CmsgHdr = CmsgHdr {
    cmsgHdrLen   :: !CInt
  , cmsgHdrLevel :: !CInt
  , cmsgHdrType  :: !CInt
  } deriving (Eq, Show)

instance Storable CmsgHdr where
  sizeOf    _ = ((12))
{-# LINE 32 "Network/Socket/Posix/CmsgHdr.hsc" #-}
  alignment _ = alignment (0 :: CInt)

  peek p = do
    len <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))   p
{-# LINE 36 "Network/Socket/Posix/CmsgHdr.hsc" #-}
    lvl <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 37 "Network/Socket/Posix/CmsgHdr.hsc" #-}
    typ <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))  p
{-# LINE 38 "Network/Socket/Posix/CmsgHdr.hsc" #-}
    return $ CmsgHdr len lvl typ

  poke p (CmsgHdr len lvl typ) = do
    zeroMemory p ((12))
{-# LINE 42 "Network/Socket/Posix/CmsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))   p len
{-# LINE 43 "Network/Socket/Posix/CmsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p lvl
{-# LINE 44 "Network/Socket/Posix/CmsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))  p typ
{-# LINE 45 "Network/Socket/Posix/CmsgHdr.hsc" #-}

withCmsgs :: [Cmsg] -> (Ptr CmsgHdr -> Int -> IO a) -> IO a
withCmsgs cmsgs0 action
  | total == 0 = action nullPtr 0
  | otherwise  = allocaBytes total $ \ctrlPtr -> do
        loop ctrlPtr cmsgs0 spaces
        action ctrlPtr total
  where
    loop ctrlPtr (cmsg:cmsgs) (s:ss) = do
        toCmsgHdr cmsg ctrlPtr
        let nextPtr = ctrlPtr `plusPtr` s
        loop nextPtr cmsgs ss
    loop _ _ _ = return ()
    cmsg_space = fromIntegral . c_cmsg_space . fromIntegral
    spaces = map (cmsg_space . B.length . cmsgData) cmsgs0
    total = sum spaces

toCmsgHdr :: Cmsg -> Ptr CmsgHdr -> IO ()
toCmsgHdr (Cmsg (CmsgId lvl typ) (PS fptr off len)) ctrlPtr = do
    poke ctrlPtr $ CmsgHdr (c_cmsg_len (fromIntegral len)) lvl typ
    withForeignPtr fptr $ \src0 -> do
        let src = src0 `plusPtr` off
        dst <- c_cmsg_data ctrlPtr
        memcpy dst src len

parseCmsgs :: SocketAddress sa => Ptr (MsgHdr sa) -> IO [Cmsg]
parseCmsgs msgptr = do
    ptr <- c_cmsg_firsthdr msgptr
    loop ptr id
  where
    loop ptr build
      | ptr == nullPtr = return $ build []
      | otherwise = do
            cmsg <- fromCmsgHdr ptr
            nextPtr <- c_cmsg_nxthdr msgptr ptr
            loop nextPtr (build . (cmsg :))

fromCmsgHdr :: Ptr CmsgHdr -> IO Cmsg
fromCmsgHdr ptr = do
    CmsgHdr len lvl typ <- peek ptr
    src <- c_cmsg_data ptr
    let siz = fromIntegral len - (src `minusPtr` ptr)
    Cmsg (CmsgId lvl typ) <$> create (fromIntegral siz) (\dst -> memcpy dst src siz)

foreign import ccall unsafe "cmsg_firsthdr"
  c_cmsg_firsthdr :: Ptr (MsgHdr sa) -> IO (Ptr CmsgHdr)

foreign import ccall unsafe "cmsg_nxthdr"
  c_cmsg_nxthdr :: Ptr (MsgHdr sa) -> Ptr CmsgHdr -> IO (Ptr CmsgHdr)

foreign import ccall unsafe "cmsg_data"
  c_cmsg_data :: Ptr CmsgHdr -> IO (Ptr Word8)

foreign import ccall unsafe "cmsg_space"
  c_cmsg_space :: CInt -> CInt

foreign import ccall unsafe "cmsg_len"
  c_cmsg_len :: CInt -> CInt
