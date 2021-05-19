{-# LINE 1 "Network/Socket/Posix/Cmsg.hsc" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Socket.Posix.Cmsg where






import Data.ByteString.Internal
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Posix.Types (Fd(..))

import Network.Socket.Imports
import Network.Socket.Types
import Network.Socket.ReadShow

import qualified Text.Read as P

-- | Control message (ancillary data) including a pair of level and type.
data Cmsg = Cmsg {
    cmsgId   :: !CmsgId
  , cmsgData :: !ByteString
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | Identifier of control message (ancillary data).
data CmsgId = CmsgId {
    cmsgLevel :: !CInt
  , cmsgType  :: !CInt
  } deriving (Eq)

-- | Unsupported identifier
pattern UnsupportedCmsgId :: CmsgId
pattern UnsupportedCmsgId = CmsgId (-1) (-1)

-- | The identifier for 'IPv4TTL'.
pattern CmsgIdIPv4TTL :: CmsgId

{-# LINE 48 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv4TTL = CmsgId (0) (24)
{-# LINE 49 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 52 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv6HopLimit'.
pattern CmsgIdIPv6HopLimit :: CmsgId
pattern CmsgIdIPv6HopLimit = CmsgId (41) (47)
{-# LINE 56 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv4TOS'.
pattern CmsgIdIPv4TOS :: CmsgId

{-# LINE 60 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv4TOS = CmsgId (0) (27)
{-# LINE 61 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 64 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv6TClass'.
pattern CmsgIdIPv6TClass :: CmsgId
pattern CmsgIdIPv6TClass = CmsgId (41) (36)
{-# LINE 68 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv4PktInfo'.
pattern CmsgIdIPv4PktInfo :: CmsgId

{-# LINE 72 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv4PktInfo = CmsgId (0) (26)
{-# LINE 73 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 76 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'IPv6PktInfo'.
pattern CmsgIdIPv6PktInfo :: CmsgId

{-# LINE 80 "Network/Socket/Posix/Cmsg.hsc" #-}
pattern CmsgIdIPv6PktInfo = CmsgId (41) (46)
{-# LINE 81 "Network/Socket/Posix/Cmsg.hsc" #-}

{-# LINE 84 "Network/Socket/Posix/Cmsg.hsc" #-}

-- | The identifier for 'Fd'.
pattern CmsgIdFd :: CmsgId
pattern CmsgIdFd = CmsgId (65535) (1)
{-# LINE 88 "Network/Socket/Posix/Cmsg.hsc" #-}

----------------------------------------------------------------

-- | Locate a control message of the given type in a list of control
--   messages. The following shows an example usage:
--
-- > (lookupCmsg CmsgIdIPv4TOS cmsgs >>= decodeCmsg) :: Maybe IPv4TOS
lookupCmsg :: CmsgId -> [Cmsg] -> Maybe Cmsg
lookupCmsg cid cmsgs = find (\cmsg -> cmsgId cmsg == cid) cmsgs

-- | Filtering control message.
filterCmsg :: CmsgId -> [Cmsg] -> [Cmsg]
filterCmsg cid cmsgs = filter (\cmsg -> cmsgId cmsg == cid) cmsgs

----------------------------------------------------------------

-- | Control message type class.
--   Each control message type has a numeric 'CmsgId' and a 'Storable'
--   data representation.
class Storable a => ControlMessage a where
    controlMessageId :: CmsgId

encodeCmsg :: forall a . ControlMessage a => a -> Cmsg
encodeCmsg x = unsafeDupablePerformIO $ do
    bs <- create siz $ \p0 -> do
        let p = castPtr p0
        poke p x
    let cmsid = controlMessageId @a
    return $ Cmsg cmsid bs
  where
    siz = sizeOf x

decodeCmsg :: forall a . (ControlMessage a, Storable a) => Cmsg -> Maybe a
decodeCmsg (Cmsg cmsid (PS fptr off len))
  | cid /= cmsid = Nothing
  | len < siz    = Nothing
  | otherwise    = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = castPtr (p0 `plusPtr` off)
        Just <$> peek p
  where
    cid = controlMessageId @a
    siz = sizeOf (undefined :: a)

----------------------------------------------------------------

-- | Time to live of IPv4.

{-# LINE 135 "Network/Socket/Posix/Cmsg.hsc" #-}
newtype IPv4TTL = IPv4TTL CChar deriving (Eq, Show, Storable)

{-# LINE 139 "Network/Socket/Posix/Cmsg.hsc" #-}

instance ControlMessage IPv4TTL where
    controlMessageId = CmsgIdIPv4TTL

----------------------------------------------------------------

-- | Hop limit of IPv6.
newtype IPv6HopLimit = IPv6HopLimit CInt deriving (Eq, Show, Storable)

instance ControlMessage IPv6HopLimit where
    controlMessageId = CmsgIdIPv6HopLimit

----------------------------------------------------------------

-- | TOS of IPv4.
newtype IPv4TOS = IPv4TOS CChar deriving (Eq, Show, Storable)

instance ControlMessage IPv4TOS where
    controlMessageId = CmsgIdIPv4TOS

----------------------------------------------------------------

-- | Traffic class of IPv6.
newtype IPv6TClass = IPv6TClass CInt deriving (Eq, Show, Storable)

instance ControlMessage IPv6TClass where
    controlMessageId = CmsgIdIPv6TClass

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv4PktInfo = IPv4PktInfo Int HostAddress HostAddress deriving (Eq)

instance Show IPv4PktInfo where
    show (IPv4PktInfo n sa ha) = "IPv4PktInfo " ++ show n ++ " " ++ show (hostAddressToTuple sa) ++ " " ++ show (hostAddressToTuple ha)

instance ControlMessage IPv4PktInfo where
    controlMessageId = CmsgIdIPv4PktInfo

instance Storable IPv4PktInfo where

{-# LINE 180 "Network/Socket/Posix/Cmsg.hsc" #-}
    sizeOf    _ = ((12))
{-# LINE 181 "Network/Socket/Posix/Cmsg.hsc" #-}
    alignment _ = alignment (0 :: CInt)
    poke p (IPv4PktInfo n sa ha) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p (fromIntegral n :: CInt)
{-# LINE 184 "Network/Socket/Posix/Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p sa
{-# LINE 185 "Network/Socket/Posix/Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8))     p ha
{-# LINE 186 "Network/Socket/Posix/Cmsg.hsc" #-}
    peek p = do
        n  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  p
{-# LINE 188 "Network/Socket/Posix/Cmsg.hsc" #-}
        sa <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 189 "Network/Socket/Posix/Cmsg.hsc" #-}
        ha <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))     p
{-# LINE 190 "Network/Socket/Posix/Cmsg.hsc" #-}
        return $ IPv4PktInfo n sa ha

{-# LINE 197 "Network/Socket/Posix/Cmsg.hsc" #-}

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv6PktInfo = IPv6PktInfo Int HostAddress6 deriving (Eq)

instance Show IPv6PktInfo where
    show (IPv6PktInfo n ha6) = "IPv6PktInfo " ++ show n ++ " " ++ show (hostAddress6ToTuple ha6)

instance ControlMessage IPv6PktInfo where
    controlMessageId = CmsgIdIPv6PktInfo

instance Storable IPv6PktInfo where

{-# LINE 211 "Network/Socket/Posix/Cmsg.hsc" #-}
    sizeOf    _ = ((20))
{-# LINE 212 "Network/Socket/Posix/Cmsg.hsc" #-}
    alignment _ = alignment (0 :: CInt)
    poke p (IPv6PktInfo n ha6) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (fromIntegral n :: CInt)
{-# LINE 215 "Network/Socket/Posix/Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))    p (In6Addr ha6)
{-# LINE 216 "Network/Socket/Posix/Cmsg.hsc" #-}
    peek p = do
        In6Addr ha6 <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))    p
{-# LINE 218 "Network/Socket/Posix/Cmsg.hsc" #-}
        n :: CInt   <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 219 "Network/Socket/Posix/Cmsg.hsc" #-}
        return $ IPv6PktInfo (fromIntegral n) ha6

{-# LINE 226 "Network/Socket/Posix/Cmsg.hsc" #-}

----------------------------------------------------------------

instance ControlMessage Fd where
    controlMessageId = CmsgIdFd

cmsgIdBijection :: Bijection CmsgId String
cmsgIdBijection =
    [ (UnsupportedCmsgId, "UnsupportedCmsgId")
    , (CmsgIdIPv4TTL, "CmsgIdIPv4TTL")
    , (CmsgIdIPv6HopLimit, "CmsgIdIPv6HopLimit")
    , (CmsgIdIPv4TOS, "CmsgIdIPv4TOS")
    , (CmsgIdIPv6TClass, "CmsgIdIPv6TClass")
    , (CmsgIdIPv4PktInfo, "CmsgIdIPv4PktInfo")
    , (CmsgIdIPv6PktInfo, "CmsgIdIPv6PktInfo")
    , (CmsgIdFd, "CmsgIdFd")
    ]

instance Show CmsgId where
    showsPrec = bijectiveShow cmsgIdBijection def
      where
        defname = "CmsgId"
        unId = \(CmsgId l t) -> (l,t)
        def = defShow defname unId showIntInt

instance Read CmsgId where
    readPrec = bijectiveRead cmsgIdBijection def
      where
        defname = "CmsgId"
        def = defRead defname (uncurry CmsgId) readIntInt
