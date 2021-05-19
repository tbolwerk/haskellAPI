{-# LINE 1 "Network/Socket/Types.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


#include "HsNetDef.h"

module Network.Socket.Types (
    -- * Socket type
      Socket
    , withFdSocket
    , unsafeFdSocket
    , touchSocket
    , socketToFd
    , fdSocket
    , mkSocket
    , invalidateSocket
    , close
    , close'
    , c_close
    -- * Types of socket
    , SocketType(GeneralSocketType, UnsupportedSocketType, NoSocketType
                , Stream, Datagram, Raw, RDM, SeqPacket)
    , isSupportedSocketType
    , packSocketType
    , unpackSocketType

    -- * Family
    , Family(GeneralFamily, UnsupportedFamily
            ,AF_UNSPEC,AF_UNIX,AF_INET,AF_INET6,AF_IMPLINK,AF_PUP,AF_CHAOS
            ,AF_NS,AF_NBS,AF_ECMA,AF_DATAKIT,AF_CCITT,AF_SNA,AF_DECnet
            ,AF_DLI,AF_LAT,AF_HYLINK,AF_APPLETALK,AF_ROUTE,AF_NETBIOS
            ,AF_NIT,AF_802,AF_ISO,AF_OSI,AF_NETMAN,AF_X25,AF_AX25,AF_OSINET
            ,AF_GOSSIP,AF_IPX,Pseudo_AF_XTP,AF_CTF,AF_WAN,AF_SDL,AF_NETWARE
            ,AF_NDD,AF_INTF,AF_COIP,AF_CNT,Pseudo_AF_RTIP,Pseudo_AF_PIP
            ,AF_SIP,AF_ISDN,Pseudo_AF_KEY,AF_NATM,AF_ARP,Pseudo_AF_HDRCMPLT
            ,AF_ENCAP,AF_LINK,AF_RAW,AF_RIF,AF_NETROM,AF_BRIDGE,AF_ATMPVC
            ,AF_ROSE,AF_NETBEUI,AF_SECURITY,AF_PACKET,AF_ASH,AF_ECONET
            ,AF_ATMSVC,AF_IRDA,AF_PPPOX,AF_WANPIPE,AF_BLUETOOTH,AF_CAN)
    , isSupportedFamily
    , packFamily
    , unpackFamily

    -- * Socket address typeclass
    , SocketAddress(..)
    , withSocketAddress
    , withNewSocketAddress

    -- * Socket address type
    , SockAddr(..)
    , isSupportedSockAddr
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    , FlowInfo
    , ScopeID
    , peekSockAddr
    , pokeSockAddr
    , withSockAddr

    -- * Unsorted
    , ProtocolNumber
    , defaultProtocol
    , PortNumber
    , defaultPort

    -- * Low-level helpers
    , zeroMemory
    , htonl
    , ntohl
    , In6Addr(..)
    ) where

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef', mkWeakIORef)
import Foreign.C.Error (throwErrno)
import Foreign.Marshal.Alloc
import GHC.Conc (closeFdWith)
import System.Posix.Types (Fd)
import Control.DeepSeq (NFData (..))
import GHC.Exts (touch#)
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.IO (IO (..))

import qualified Text.Read as P


{-# LINE 97 "Network/Socket/Types.hsc" #-}
import Foreign.Marshal.Array

{-# LINE 99 "Network/Socket/Types.hsc" #-}

import Network.Socket.Imports

----- readshow module import
import Network.Socket.ReadShow


-----------------------------------------------------------------------------

-- | Basic type for a socket.
data Socket = Socket !(IORef CInt) !CInt {- for Show -}

instance Show Socket where
    show (Socket _ ofd) = "<socket: " ++ show ofd ++ ">"

instance Eq Socket where
    Socket ref1 _ == Socket ref2 _ = ref1 == ref2

{-# DEPRECATED fdSocket "Use withFdSocket or unsafeFdSocket instead" #-}
-- | Currently, this is an alias of `unsafeFdSocket`.
fdSocket :: Socket -> IO CInt
fdSocket = unsafeFdSocket

-- | Getting a file descriptor from a socket.
--
--   If a 'Socket' is shared with multiple threads and
--   one uses 'unsafeFdSocket', unexpected issues may happen.
--   Consider the following scenario:
--
--   1) Thread A acquires a 'Fd' from 'Socket' by 'unsafeFdSocket'.
--
--   2) Thread B close the 'Socket'.
--
--   3) Thread C opens a new 'Socket'. Unfortunately it gets the same 'Fd'
--      number which thread A is holding.
--
--   In this case, it is safer for Thread A to clone 'Fd' by
--   'System.Posix.IO.dup'. But this would still suffer from
--   a race condition between 'unsafeFdSocket' and 'close'.
--
--   If you use this function, you need to guarantee that the 'Socket' does not
--   get garbage-collected until after you finish using the file descriptor.
--   'touchSocket' can be used for this purpose.
--
--   A safer option is to use 'withFdSocket' instead.
unsafeFdSocket :: Socket -> IO CInt
unsafeFdSocket (Socket ref _) = readIORef ref

-- | Ensure that the given 'Socket' stays alive (i.e. not garbage-collected)
--   at the given place in the sequence of IO actions. This function can be
--   used in conjunction with 'unsafeFdSocket' to guarantee that the file
--   descriptor is not prematurely freed.
--
-- > fd <- unsafeFdSocket sock
-- > -- using fd with blocking operations such as accept(2)
-- > touchSocket sock
touchSocket :: Socket -> IO ()
touchSocket (Socket ref _) = touch ref

touch :: IORef a -> IO ()
touch (IORef (STRef mutVar)) =
  -- Thanks to a GHC issue, this touch# may not be quite guaranteed
  -- to work. There's talk of replacing the touch# primop with one
  -- that works better with the optimizer. But this seems to be the
  -- "right" way to do it for now.
  IO $ \s -> (# touch# mutVar s, () #)

-- | Get a file descriptor from a 'Socket'. The socket will never
-- be closed automatically before @withFdSocket@ completes, but
-- it may still be closed by an explicit call to 'close' or `close'`,
-- either before or during the call.
--
-- The file descriptor must not be used after @withFdSocket@ returns, because
-- the 'Socket' may have been garbage-collected, invalidating the file
-- descriptor.
--
-- Since: 3.1.0.0
withFdSocket :: Socket -> (CInt -> IO r) -> IO r
withFdSocket (Socket ref _) f = do
  fd <- readIORef ref
  -- Should we throw an exception if the socket is already invalid?
  -- That will catch some mistakes but certainly not all.

  r <- f fd

  touch ref
  return r

-- | Socket is closed and a duplicated file descriptor is returned.
--   The duplicated descriptor is no longer subject to the possibility
--   of unexpectedly being closed if the socket is finalized. It is
--   now the caller's responsibility to ultimately close the
--   duplicated file descriptor.
socketToFd :: Socket -> IO CInt
socketToFd s = do

{-# LINE 204 "Network/Socket/Types.hsc" #-}
    fd <- unsafeFdSocket s
    -- FIXME: throw error no if -1
    fd2 <- c_dup fd
    close s
    return fd2

foreign import ccall unsafe "dup"
   c_dup :: CInt -> IO CInt

{-# LINE 213 "Network/Socket/Types.hsc" #-}

-- | Creating a socket from a file descriptor.
mkSocket :: CInt -> IO Socket
mkSocket fd = do
    ref <- newIORef fd
    let s = Socket ref fd
    void $ mkWeakIORef ref $ close s
    return s

invalidSocket :: CInt

{-# LINE 226 "Network/Socket/Types.hsc" #-}
invalidSocket = -1

{-# LINE 228 "Network/Socket/Types.hsc" #-}

invalidateSocket ::
      Socket
   -> (CInt -> IO a)
   -> (CInt -> IO a)
   -> IO a
invalidateSocket (Socket ref _) errorAction normalAction = do
    oldfd <- atomicModifyIORef' ref $ \cur -> (invalidSocket, cur)
    if oldfd == invalidSocket then errorAction oldfd else normalAction oldfd

-----------------------------------------------------------------------------

-- | Close the socket. This function does not throw exceptions even if
--   the underlying system call returns errors.
--
--   If multiple threads use the same socket and one uses 'unsafeFdSocket' and
--   the other use 'close', unexpected behavior may happen.
--   For more information, please refer to the documentation of 'unsafeFdSocket'.
close :: Socket -> IO ()
close s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    -- closeFd ignores the return value of c_close and
    -- does not throw exceptions
    closeFd :: Fd -> IO ()
    closeFd = void . c_close . fromIntegral

-- | Close the socket. This function throws exceptions if
--   the underlying system call returns errors.
close' :: Socket -> IO ()
close' s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    closeFd :: Fd -> IO ()
    closeFd fd = do
        ret <- c_close $ fromIntegral fd
        when (ret == -1) $ throwErrno "Network.Socket.close'"


{-# LINE 276 "Network/Socket/Types.hsc" #-}
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt

{-# LINE 279 "Network/Socket/Types.hsc" #-}

-----------------------------------------------------------------------------

-- | Protocol number.
type ProtocolNumber = CInt

-- | This is the default protocol for a given service.
--
-- >>> defaultProtocol
-- 0
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

-----------------------------------------------------------------------------
-- Socket types

-- There are a few possible ways to do this.  The first is convert the
-- structs used in the C library into an equivalent Haskell type. An
-- other possible implementation is to keep all the internals in the C
-- code and use an Int## and a status flag. The second method is used
-- here since a lot of the C structures are not required to be
-- manipulated.

-- Originally the status was non-mutable so we had to return a new
-- socket each time we changed the status.  This version now uses
-- mutable variables to avoid the need to do this.  The result is a
-- cleaner interface and better security since the application
-- programmer now can't circumvent the status information to perform
-- invalid operations on sockets.

-- | Socket Types.
--
-- Some of the defined patterns may be unsupported on some systems:
-- see 'isSupportedSocketType'.
newtype SocketType = SocketType { packSocketType :: CInt }
        deriving (Eq, Ord)

unpackSocketType :: CInt -> SocketType
unpackSocketType = SocketType
{-# INLINE unpackSocketType #-}

-- | Is the @SOCK_xxxxx@ constant corresponding to the given SocketType known
-- on this system?  'GeneralSocketType' values, not equal to any of the named
-- patterns or 'UnsupportedSocketType', will return 'True' even when not
-- known on this system.
isSupportedSocketType :: SocketType -> Bool
isSupportedSocketType = (/= UnsupportedSocketType)

-- | Pattern for a general socket type.
pattern GeneralSocketType    :: CInt -> SocketType
pattern GeneralSocketType n  =  SocketType n

{-# LINE 331 "Network/Socket/Types.hsc" #-}
{-# COMPLETE GeneralSocketType #-}

{-# LINE 333 "Network/Socket/Types.hsc" #-}
-- The actual constructor is not exported, which keeps the internal
-- representation private, but for all purposes other than 'coerce' the
-- above pattern is just as good.

-- | Unsupported socket type, equal to any other types not supported on this
-- system.
pattern UnsupportedSocketType :: SocketType
pattern UnsupportedSocketType  = SocketType (-1)

-- | Used in getAddrInfo hints, for example.
pattern NoSocketType        :: SocketType
pattern NoSocketType         = SocketType 0

pattern Stream              :: SocketType

{-# LINE 348 "Network/Socket/Types.hsc" #-}
pattern Stream               = SocketType (1)
{-# LINE 349 "Network/Socket/Types.hsc" #-}

{-# LINE 352 "Network/Socket/Types.hsc" #-}

pattern Datagram            :: SocketType

{-# LINE 355 "Network/Socket/Types.hsc" #-}
pattern Datagram             = SocketType (2)
{-# LINE 356 "Network/Socket/Types.hsc" #-}

{-# LINE 359 "Network/Socket/Types.hsc" #-}

pattern Raw                 :: SocketType

{-# LINE 362 "Network/Socket/Types.hsc" #-}
pattern Raw                  = SocketType (3)
{-# LINE 363 "Network/Socket/Types.hsc" #-}

{-# LINE 366 "Network/Socket/Types.hsc" #-}

pattern RDM                 :: SocketType

{-# LINE 369 "Network/Socket/Types.hsc" #-}
pattern RDM                  = SocketType (4)
{-# LINE 370 "Network/Socket/Types.hsc" #-}

{-# LINE 373 "Network/Socket/Types.hsc" #-}

pattern SeqPacket           :: SocketType

{-# LINE 376 "Network/Socket/Types.hsc" #-}
pattern SeqPacket            = SocketType (5)
{-# LINE 377 "Network/Socket/Types.hsc" #-}

{-# LINE 380 "Network/Socket/Types.hsc" #-}

------------------------------------------------------------------------
-- Protocol Families.


-- | Address families.  The @AF_xxxxx@ constants are widely used as synonyms
-- for the corresponding @PF_xxxxx@ protocol family values, to which they are
-- numerically equal in mainstream socket API implementations.
--
-- Stictly correct usage would be to pass the @PF_xxxxx@ constants as the first
-- argument when creating a 'Socket', while the @AF_xxxxx@ constants should be
-- used as @addrFamily@ values with 'getAddrInfo'.  For now only the @AF_xxxxx@
-- constants are provided.
--
-- Some of the defined patterns may be unsupported on some systems:
-- see 'isSupportedFamily'.
newtype Family = Family { packFamily :: CInt } deriving (Eq, Ord)


-- | Does one of the AF_ constants correspond to a known address family on this
-- system.  'GeneralFamily' values, not equal to any of the named @AF_xxxxx@
-- patterns or 'UnsupportedFamily', will return 'True' even when not known on
-- this system.
isSupportedFamily :: Family -> Bool
isSupportedFamily f = case f of
    UnsupportedFamily -> False
    GeneralFamily _   -> True

-- | Convert 'CInt' to 'Family'.
unpackFamily :: CInt -> Family
unpackFamily = Family
{-# INLINE unpackFamily #-}

-- | Pattern for a general protocol family (a.k.a. address family).
--
-- @since 3.2.0.0
pattern GeneralFamily      :: CInt -> Family
pattern GeneralFamily n     = Family n

{-# LINE 419 "Network/Socket/Types.hsc" #-}
{-# COMPLETE GeneralFamily #-}

{-# LINE 421 "Network/Socket/Types.hsc" #-}
-- The actual constructor is not exported, which keeps the internal
-- representation private, but for all purposes other than 'coerce' the
-- above pattern is just as good.

-- | Unsupported address family, equal to any other families that are not
-- supported on the system.
--
-- @since 3.2.0.0
pattern UnsupportedFamily  :: Family
pattern UnsupportedFamily   = Family (-1)

-- | unspecified
pattern AF_UNSPEC          :: Family
pattern AF_UNSPEC           = Family (0)
{-# LINE 435 "Network/Socket/Types.hsc" #-}

-- | UNIX-domain
pattern AF_UNIX            :: Family

{-# LINE 439 "Network/Socket/Types.hsc" #-}
pattern AF_UNIX             = Family (1)
{-# LINE 440 "Network/Socket/Types.hsc" #-}

{-# LINE 443 "Network/Socket/Types.hsc" #-}

-- | Internet Protocol version 4
pattern AF_INET            :: Family

{-# LINE 447 "Network/Socket/Types.hsc" #-}
pattern AF_INET             = Family (2)
{-# LINE 448 "Network/Socket/Types.hsc" #-}

{-# LINE 451 "Network/Socket/Types.hsc" #-}

-- | Internet Protocol version 6
pattern AF_INET6           :: Family

{-# LINE 455 "Network/Socket/Types.hsc" #-}
pattern AF_INET6            = Family (30)
{-# LINE 456 "Network/Socket/Types.hsc" #-}

{-# LINE 459 "Network/Socket/Types.hsc" #-}

-- | Arpanet imp addresses
pattern AF_IMPLINK         :: Family

{-# LINE 463 "Network/Socket/Types.hsc" #-}
pattern AF_IMPLINK          = Family (3)
{-# LINE 464 "Network/Socket/Types.hsc" #-}

{-# LINE 467 "Network/Socket/Types.hsc" #-}

-- | pup protocols: e.g. BSP
pattern AF_PUP             :: Family

{-# LINE 471 "Network/Socket/Types.hsc" #-}
pattern AF_PUP              = Family (4)
{-# LINE 472 "Network/Socket/Types.hsc" #-}

{-# LINE 475 "Network/Socket/Types.hsc" #-}

-- | mit CHAOS protocols
pattern AF_CHAOS           :: Family

{-# LINE 479 "Network/Socket/Types.hsc" #-}
pattern AF_CHAOS            = Family (5)
{-# LINE 480 "Network/Socket/Types.hsc" #-}

{-# LINE 483 "Network/Socket/Types.hsc" #-}

-- | XEROX NS protocols
pattern AF_NS              :: Family

{-# LINE 487 "Network/Socket/Types.hsc" #-}
pattern AF_NS               = Family (6)
{-# LINE 488 "Network/Socket/Types.hsc" #-}

{-# LINE 491 "Network/Socket/Types.hsc" #-}

-- | nbs protocols
pattern AF_NBS             :: Family

{-# LINE 497 "Network/Socket/Types.hsc" #-}
pattern AF_NBS              = Family (-1)

{-# LINE 499 "Network/Socket/Types.hsc" #-}

-- | european computer manufacturers
pattern AF_ECMA            :: Family

{-# LINE 503 "Network/Socket/Types.hsc" #-}
pattern AF_ECMA             = Family (8)
{-# LINE 504 "Network/Socket/Types.hsc" #-}

{-# LINE 507 "Network/Socket/Types.hsc" #-}

-- | datakit protocols
pattern AF_DATAKIT         :: Family

{-# LINE 511 "Network/Socket/Types.hsc" #-}
pattern AF_DATAKIT          = Family (9)
{-# LINE 512 "Network/Socket/Types.hsc" #-}

{-# LINE 515 "Network/Socket/Types.hsc" #-}

-- | CCITT protocols, X.25 etc
pattern AF_CCITT           :: Family

{-# LINE 519 "Network/Socket/Types.hsc" #-}
pattern AF_CCITT            = Family (10)
{-# LINE 520 "Network/Socket/Types.hsc" #-}

{-# LINE 523 "Network/Socket/Types.hsc" #-}

-- | IBM SNA
pattern AF_SNA             :: Family

{-# LINE 527 "Network/Socket/Types.hsc" #-}
pattern AF_SNA              = Family (11)
{-# LINE 528 "Network/Socket/Types.hsc" #-}

{-# LINE 531 "Network/Socket/Types.hsc" #-}

-- | DECnet
pattern AF_DECnet          :: Family

{-# LINE 535 "Network/Socket/Types.hsc" #-}
pattern AF_DECnet           = Family (12)
{-# LINE 536 "Network/Socket/Types.hsc" #-}

{-# LINE 539 "Network/Socket/Types.hsc" #-}

-- | Direct data link interface
pattern AF_DLI             :: Family

{-# LINE 543 "Network/Socket/Types.hsc" #-}
pattern AF_DLI              = Family (13)
{-# LINE 544 "Network/Socket/Types.hsc" #-}

{-# LINE 547 "Network/Socket/Types.hsc" #-}

-- | LAT
pattern AF_LAT             :: Family

{-# LINE 551 "Network/Socket/Types.hsc" #-}
pattern AF_LAT              = Family (14)
{-# LINE 552 "Network/Socket/Types.hsc" #-}

{-# LINE 555 "Network/Socket/Types.hsc" #-}

-- | NSC Hyperchannel
pattern AF_HYLINK          :: Family

{-# LINE 559 "Network/Socket/Types.hsc" #-}
pattern AF_HYLINK           = Family (15)
{-# LINE 560 "Network/Socket/Types.hsc" #-}

{-# LINE 563 "Network/Socket/Types.hsc" #-}

-- | Apple Talk
pattern AF_APPLETALK       :: Family

{-# LINE 567 "Network/Socket/Types.hsc" #-}
pattern AF_APPLETALK        = Family (16)
{-# LINE 568 "Network/Socket/Types.hsc" #-}

{-# LINE 571 "Network/Socket/Types.hsc" #-}

-- | Internal Routing Protocol (aka AF_NETLINK)
pattern AF_ROUTE           :: Family

{-# LINE 575 "Network/Socket/Types.hsc" #-}
pattern AF_ROUTE            = Family (17)
{-# LINE 576 "Network/Socket/Types.hsc" #-}

{-# LINE 579 "Network/Socket/Types.hsc" #-}

-- | NetBios-style addresses
pattern AF_NETBIOS         :: Family

{-# LINE 583 "Network/Socket/Types.hsc" #-}
pattern AF_NETBIOS          = Family (33)
{-# LINE 584 "Network/Socket/Types.hsc" #-}

{-# LINE 587 "Network/Socket/Types.hsc" #-}

-- | Network Interface Tap
pattern AF_NIT             :: Family

{-# LINE 593 "Network/Socket/Types.hsc" #-}
pattern AF_NIT              = Family (-1)

{-# LINE 595 "Network/Socket/Types.hsc" #-}

-- | IEEE 802.2, also ISO 8802
pattern AF_802             :: Family

{-# LINE 601 "Network/Socket/Types.hsc" #-}
pattern AF_802              = Family (-1)

{-# LINE 603 "Network/Socket/Types.hsc" #-}

-- | ISO protocols
pattern AF_ISO             :: Family

{-# LINE 607 "Network/Socket/Types.hsc" #-}
pattern AF_ISO              = Family (7)
{-# LINE 608 "Network/Socket/Types.hsc" #-}

{-# LINE 611 "Network/Socket/Types.hsc" #-}

-- | umbrella of all families used by OSI
pattern AF_OSI             :: Family

{-# LINE 615 "Network/Socket/Types.hsc" #-}
pattern AF_OSI              = Family (7)
{-# LINE 616 "Network/Socket/Types.hsc" #-}

{-# LINE 619 "Network/Socket/Types.hsc" #-}

-- | DNA Network Management
pattern AF_NETMAN          :: Family

{-# LINE 625 "Network/Socket/Types.hsc" #-}
pattern AF_NETMAN           = Family (-1)

{-# LINE 627 "Network/Socket/Types.hsc" #-}

-- | CCITT X.25
pattern AF_X25             :: Family

{-# LINE 633 "Network/Socket/Types.hsc" #-}
pattern AF_X25              = Family (-1)

{-# LINE 635 "Network/Socket/Types.hsc" #-}

-- | AX25
pattern AF_AX25            :: Family

{-# LINE 641 "Network/Socket/Types.hsc" #-}
pattern AF_AX25             = Family (-1)

{-# LINE 643 "Network/Socket/Types.hsc" #-}

-- | AFI
pattern AF_OSINET          :: Family

{-# LINE 649 "Network/Socket/Types.hsc" #-}
pattern AF_OSINET           = Family (-1)

{-# LINE 651 "Network/Socket/Types.hsc" #-}

-- | US Government OSI
pattern AF_GOSSIP          :: Family

{-# LINE 657 "Network/Socket/Types.hsc" #-}
pattern AF_GOSSIP           = Family (-1)

{-# LINE 659 "Network/Socket/Types.hsc" #-}

-- | Novell Internet Protocol
pattern AF_IPX             :: Family

{-# LINE 663 "Network/Socket/Types.hsc" #-}
pattern AF_IPX              = Family (23)
{-# LINE 664 "Network/Socket/Types.hsc" #-}

{-# LINE 667 "Network/Socket/Types.hsc" #-}

-- | eXpress Transfer Protocol (no AF)
pattern Pseudo_AF_XTP      :: Family

{-# LINE 673 "Network/Socket/Types.hsc" #-}
pattern Pseudo_AF_XTP       = Family (-1)

{-# LINE 675 "Network/Socket/Types.hsc" #-}

-- | Common Trace Facility
pattern AF_CTF             :: Family

{-# LINE 681 "Network/Socket/Types.hsc" #-}
pattern AF_CTF              = Family (-1)

{-# LINE 683 "Network/Socket/Types.hsc" #-}

-- | Wide Area Network protocols
pattern AF_WAN             :: Family

{-# LINE 689 "Network/Socket/Types.hsc" #-}
pattern AF_WAN              = Family (-1)

{-# LINE 691 "Network/Socket/Types.hsc" #-}

-- | SGI Data Link for DLPI
pattern AF_SDL             :: Family

{-# LINE 697 "Network/Socket/Types.hsc" #-}
pattern AF_SDL              = Family (-1)

{-# LINE 699 "Network/Socket/Types.hsc" #-}

-- | Netware
pattern AF_NETWARE         :: Family

{-# LINE 705 "Network/Socket/Types.hsc" #-}
pattern AF_NETWARE          = Family (-1)

{-# LINE 707 "Network/Socket/Types.hsc" #-}

-- | NDD
pattern AF_NDD             :: Family

{-# LINE 713 "Network/Socket/Types.hsc" #-}
pattern AF_NDD              = Family (-1)

{-# LINE 715 "Network/Socket/Types.hsc" #-}

-- | Debugging use only
pattern AF_INTF            :: Family

{-# LINE 721 "Network/Socket/Types.hsc" #-}
pattern AF_INTF             = Family (-1)

{-# LINE 723 "Network/Socket/Types.hsc" #-}

-- | connection-oriented IP, aka ST II
pattern AF_COIP            :: Family

{-# LINE 727 "Network/Socket/Types.hsc" #-}
pattern AF_COIP             = Family (20)
{-# LINE 728 "Network/Socket/Types.hsc" #-}

{-# LINE 731 "Network/Socket/Types.hsc" #-}

-- | Computer Network Technology
pattern AF_CNT             :: Family

{-# LINE 735 "Network/Socket/Types.hsc" #-}
pattern AF_CNT              = Family (21)
{-# LINE 736 "Network/Socket/Types.hsc" #-}

{-# LINE 739 "Network/Socket/Types.hsc" #-}

-- | Help Identify RTIP packets
pattern Pseudo_AF_RTIP     :: Family

{-# LINE 745 "Network/Socket/Types.hsc" #-}
pattern Pseudo_AF_RTIP      = Family (-1)

{-# LINE 747 "Network/Socket/Types.hsc" #-}

-- | Help Identify PIP packets
pattern Pseudo_AF_PIP      :: Family

{-# LINE 753 "Network/Socket/Types.hsc" #-}
pattern Pseudo_AF_PIP       = Family (-1)

{-# LINE 755 "Network/Socket/Types.hsc" #-}

-- | Simple Internet Protocol
pattern AF_SIP             :: Family

{-# LINE 759 "Network/Socket/Types.hsc" #-}
pattern AF_SIP              = Family (24)
{-# LINE 760 "Network/Socket/Types.hsc" #-}

{-# LINE 763 "Network/Socket/Types.hsc" #-}

-- | Integrated Services Digital Network
pattern AF_ISDN            :: Family

{-# LINE 767 "Network/Socket/Types.hsc" #-}
pattern AF_ISDN             = Family (28)
{-# LINE 768 "Network/Socket/Types.hsc" #-}

{-# LINE 771 "Network/Socket/Types.hsc" #-}

-- | Internal key-management function
pattern Pseudo_AF_KEY      :: Family

{-# LINE 777 "Network/Socket/Types.hsc" #-}
pattern Pseudo_AF_KEY       = Family (-1)

{-# LINE 779 "Network/Socket/Types.hsc" #-}

-- | native ATM access
pattern AF_NATM            :: Family

{-# LINE 783 "Network/Socket/Types.hsc" #-}
pattern AF_NATM             = Family (31)
{-# LINE 784 "Network/Socket/Types.hsc" #-}

{-# LINE 787 "Network/Socket/Types.hsc" #-}

-- | ARP (RFC 826)
pattern AF_ARP             :: Family

{-# LINE 793 "Network/Socket/Types.hsc" #-}
pattern AF_ARP              = Family (-1)

{-# LINE 795 "Network/Socket/Types.hsc" #-}

-- | Used by BPF to not rewrite hdrs in iface output
pattern Pseudo_AF_HDRCMPLT :: Family

{-# LINE 801 "Network/Socket/Types.hsc" #-}
pattern Pseudo_AF_HDRCMPLT  = Family (-1)

{-# LINE 803 "Network/Socket/Types.hsc" #-}

-- | ENCAP
pattern AF_ENCAP           :: Family

{-# LINE 809 "Network/Socket/Types.hsc" #-}
pattern AF_ENCAP            = Family (-1)

{-# LINE 811 "Network/Socket/Types.hsc" #-}

-- | Link layer interface
pattern AF_LINK            :: Family

{-# LINE 815 "Network/Socket/Types.hsc" #-}
pattern AF_LINK             = Family (18)
{-# LINE 816 "Network/Socket/Types.hsc" #-}

{-# LINE 819 "Network/Socket/Types.hsc" #-}

-- | Link layer interface
pattern AF_RAW             :: Family

{-# LINE 825 "Network/Socket/Types.hsc" #-}
pattern AF_RAW              = Family (-1)

{-# LINE 827 "Network/Socket/Types.hsc" #-}

-- | raw interface
pattern AF_RIF             :: Family

{-# LINE 833 "Network/Socket/Types.hsc" #-}
pattern AF_RIF              = Family (-1)

{-# LINE 835 "Network/Socket/Types.hsc" #-}

-- | Amateur radio NetROM
pattern AF_NETROM          :: Family

{-# LINE 841 "Network/Socket/Types.hsc" #-}
pattern AF_NETROM           = Family (-1)

{-# LINE 843 "Network/Socket/Types.hsc" #-}

-- | multiprotocol bridge
pattern AF_BRIDGE          :: Family

{-# LINE 849 "Network/Socket/Types.hsc" #-}
pattern AF_BRIDGE           = Family (-1)

{-# LINE 851 "Network/Socket/Types.hsc" #-}

-- | ATM PVCs
pattern AF_ATMPVC          :: Family

{-# LINE 857 "Network/Socket/Types.hsc" #-}
pattern AF_ATMPVC           = Family (-1)

{-# LINE 859 "Network/Socket/Types.hsc" #-}

-- | Amateur Radio X.25 PLP
pattern AF_ROSE            :: Family

{-# LINE 865 "Network/Socket/Types.hsc" #-}
pattern AF_ROSE             = Family (-1)

{-# LINE 867 "Network/Socket/Types.hsc" #-}

-- | Netbeui 802.2LLC
pattern AF_NETBEUI         :: Family

{-# LINE 873 "Network/Socket/Types.hsc" #-}
pattern AF_NETBEUI          = Family (-1)

{-# LINE 875 "Network/Socket/Types.hsc" #-}

-- | Security callback pseudo AF
pattern AF_SECURITY        :: Family

{-# LINE 881 "Network/Socket/Types.hsc" #-}
pattern AF_SECURITY         = Family (-1)

{-# LINE 883 "Network/Socket/Types.hsc" #-}

-- | Packet family
pattern AF_PACKET          :: Family

{-# LINE 889 "Network/Socket/Types.hsc" #-}
pattern AF_PACKET           = Family (-1)

{-# LINE 891 "Network/Socket/Types.hsc" #-}

-- | Ash
pattern AF_ASH             :: Family

{-# LINE 897 "Network/Socket/Types.hsc" #-}
pattern AF_ASH              = Family (-1)

{-# LINE 899 "Network/Socket/Types.hsc" #-}

-- | Acorn Econet
pattern AF_ECONET          :: Family

{-# LINE 905 "Network/Socket/Types.hsc" #-}
pattern AF_ECONET           = Family (-1)

{-# LINE 907 "Network/Socket/Types.hsc" #-}

-- | ATM SVCs
pattern AF_ATMSVC          :: Family

{-# LINE 913 "Network/Socket/Types.hsc" #-}
pattern AF_ATMSVC           = Family (-1)

{-# LINE 915 "Network/Socket/Types.hsc" #-}

-- | IRDA sockets
pattern AF_IRDA            :: Family

{-# LINE 921 "Network/Socket/Types.hsc" #-}
pattern AF_IRDA             = Family (-1)

{-# LINE 923 "Network/Socket/Types.hsc" #-}

-- | PPPoX sockets
pattern AF_PPPOX           :: Family

{-# LINE 929 "Network/Socket/Types.hsc" #-}
pattern AF_PPPOX            = Family (-1)

{-# LINE 931 "Network/Socket/Types.hsc" #-}

-- | Wanpipe API sockets
pattern AF_WANPIPE         :: Family

{-# LINE 937 "Network/Socket/Types.hsc" #-}
pattern AF_WANPIPE          = Family (-1)

{-# LINE 939 "Network/Socket/Types.hsc" #-}

-- | bluetooth sockets
pattern AF_BLUETOOTH       :: Family

{-# LINE 945 "Network/Socket/Types.hsc" #-}
pattern AF_BLUETOOTH        = Family (-1)

{-# LINE 947 "Network/Socket/Types.hsc" #-}

-- | Controller Area Network
pattern AF_CAN             :: Family

{-# LINE 953 "Network/Socket/Types.hsc" #-}
pattern AF_CAN              = Family (-1)

{-# LINE 955 "Network/Socket/Types.hsc" #-}

------------------------------------------------------------------------
-- Port Numbers

-- | Port number.
--   Use the @Num@ instance (i.e. use a literal) to create a
--   @PortNumber@ value.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
-- >>> show (12345 :: PortNumber)
-- "12345"
-- >>> 50000 < (51000 :: PortNumber)
-- True
-- >>> 50000 < (52000 :: PortNumber)
-- True
-- >>> 50000 + (10000 :: PortNumber)
-- 60000
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Num, Enum, Bounded, Real, Integral)

foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
-- | Converts the from host byte order to network byte order.
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32
-- | Converts the from network byte order to host byte order.
foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
{-# DEPRECATED htonl "Use getAddrInfo instead" #-}
{-# DEPRECATED ntohl "Use getAddrInfo instead" #-}

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (0 :: Word16)
   alignment _ = alignment (0 :: Word16)
   poke p (PortNum po) = poke (castPtr p) (htons po)
   peek p = PortNum . ntohs <$> peek (castPtr p)

-- | Default port number.
--
-- >>> defaultPort
-- 0
defaultPort :: PortNumber
defaultPort = 0

------------------------------------------------------------------------

-- | The core typeclass to unify socket addresses.
class SocketAddress sa where
    sizeOfSocketAddress :: sa -> Int
    peekSocketAddress :: Ptr sa -> IO sa
    pokeSocketAddress  :: Ptr a -> sa -> IO ()

-- sizeof(struct sockaddr_storage) which has enough space to contain
-- sockaddr_in, sockaddr_in6 and sockaddr_un.
sockaddrStorageLen :: Int
sockaddrStorageLen = 128

withSocketAddress :: SocketAddress sa => sa -> (Ptr sa -> Int -> IO a) -> IO a
withSocketAddress addr f = do
    let sz = sizeOfSocketAddress addr
    if sz == 0 then
        f nullPtr 0
      else
        allocaBytes sz $ \p -> pokeSocketAddress p addr >> f (castPtr p) sz

withNewSocketAddress :: SocketAddress sa => (Ptr sa -> Int -> IO a) -> IO a
withNewSocketAddress f = allocaBytes sockaddrStorageLen $ \ptr -> do
    zeroMemory ptr $ fromIntegral sockaddrStorageLen
    f ptr sockaddrStorageLen

------------------------------------------------------------------------
-- Socket addresses

-- The scheme used for addressing sockets is somewhat quirky. The
-- calls in the BSD socket API that need to know the socket address
-- all operate in terms of struct sockaddr, a `virtual' type of
-- socket address.

-- The Internet family of sockets are addressed as struct sockaddr_in,
-- so when calling functions that operate on struct sockaddr, we have
-- to type cast the Internet socket address into a struct sockaddr.
-- Instances of the structure for different families might *not* be
-- the same size. Same casting is required of other families of
-- sockets such as Xerox NS. Similarly for UNIX-domain sockets.

-- To represent these socket addresses in Haskell-land, we do what BSD
-- didn't do, and use a union/algebraic type for the different
-- families. Currently only UNIX-domain sockets and the Internet
-- families are supported.

-- | Flow information.
type FlowInfo = Word32
-- | Scope identifier.
type ScopeID = Word32

-- | Socket addresses.
--  The existence of a constructor does not necessarily imply that
--  that socket address type is supported on your system: see
-- 'isSupportedSockAddr'.
data SockAddr
  = SockAddrInet
        !PortNumber      -- sin_port
        !HostAddress     -- sin_addr  (ditto)
  | SockAddrInet6
        !PortNumber      -- sin6_port
        !FlowInfo        -- sin6_flowinfo (ditto)
        !HostAddress6    -- sin6_addr (ditto)
        !ScopeID         -- sin6_scope_id (ditto)
  -- | The path must have fewer than 104 characters. All of these characters must have code points less than 256.
  | SockAddrUnix
        String           -- sun_path
  deriving (Eq, Ord)

instance NFData SockAddr where
  rnf (SockAddrInet _ _) = ()
  rnf (SockAddrInet6 _ _ _ _) = ()
  rnf (SockAddrUnix str) = rnf str

-- | Is the socket address type supported on this system?
isSupportedSockAddr :: SockAddr -> Bool
isSupportedSockAddr addr = case addr of
  SockAddrInet{}  -> True
  SockAddrInet6{} -> True

{-# LINE 1079 "Network/Socket/Types.hsc" #-}
  SockAddrUnix{}  -> True

{-# LINE 1083 "Network/Socket/Types.hsc" #-}

instance SocketAddress SockAddr where
    sizeOfSocketAddress = sizeOfSockAddr
    peekSocketAddress   = peekSockAddr
    pokeSocketAddress   = pokeSockAddr


{-# LINE 1092 "Network/Socket/Types.hsc" #-}
type CSaFamily = (Word8)
{-# LINE 1093 "Network/Socket/Types.hsc" #-}

{-# LINE 1096 "Network/Socket/Types.hsc" #-}

-- | Computes the storage requirements (in bytes) of the given
-- 'SockAddr'.  This function differs from 'Foreign.Storable.sizeOf'
-- in that the value of the argument /is/ used.
sizeOfSockAddr :: SockAddr -> Int

{-# LINE 1102 "Network/Socket/Types.hsc" #-}

{-# LINE 1119 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr SockAddrUnix{}  = 106
{-# LINE 1120 "Network/Socket/Types.hsc" #-}

{-# LINE 1121 "Network/Socket/Types.hsc" #-}

{-# LINE 1124 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr SockAddrInet{}  = 16
{-# LINE 1125 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr SockAddrInet6{} = 28
{-# LINE 1126 "Network/Socket/Types.hsc" #-}

-- | Use a 'SockAddr' with a function requiring a pointer to a
-- 'SockAddr' and the length of that 'SockAddr'.
withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
    let sz = sizeOfSockAddr addr
    allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

-- We cannot bind sun_paths longer than than the space in the sockaddr_un
-- structure, and attempting to do so could overflow the allocated storage
-- space.  This constant holds the maximum allowable path length.
--

{-# LINE 1139 "Network/Socket/Types.hsc" #-}
unixPathMax :: Int
unixPathMax = 104
{-# LINE 1141 "Network/Socket/Types.hsc" #-}

{-# LINE 1142 "Network/Socket/Types.hsc" #-}

-- We can't write an instance of 'Storable' for 'SockAddr' because
-- @sockaddr@ is a sum type of variable size but
-- 'Foreign.Storable.sizeOf' is required to be constant.

-- Note that on Darwin, the sockaddr structure must be zeroed before
-- use.

-- | Write the given 'SockAddr' to the given memory location.
pokeSockAddr :: Ptr a -> SockAddr -> IO ()

{-# LINE 1153 "Network/Socket/Types.hsc" #-}
pokeSockAddr p sa@(SockAddrUnix path) = do
    when (length path > unixPathMax) $ error "pokeSockAddr: path is too long"
    zeroMemory p $ fromIntegral $ sizeOfSockAddr sa

{-# LINE 1157 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((106) :: Word8)
{-# LINE 1158 "Network/Socket/Types.hsc" #-}

{-# LINE 1159 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1)) p ((1) :: CSaFamily)
{-# LINE 1160 "Network/Socket/Types.hsc" #-}
    let pathC = map castCharToCChar path
    -- the buffer is already filled with nulls.
    pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p) pathC
{-# LINE 1163 "Network/Socket/Types.hsc" #-}

{-# LINE 1166 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrInet port addr) = do
    zeroMemory p (16)
{-# LINE 1168 "Network/Socket/Types.hsc" #-}

{-# LINE 1169 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((16) :: Word8)
{-# LINE 1170 "Network/Socket/Types.hsc" #-}

{-# LINE 1171 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1)) p ((2) :: CSaFamily)
{-# LINE 1172 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p port
{-# LINE 1173 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p addr
{-# LINE 1174 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrInet6 port flow addr scope) = do
    zeroMemory p (28)
{-# LINE 1176 "Network/Socket/Types.hsc" #-}

{-# LINE 1177 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((28) :: Word8)
{-# LINE 1178 "Network/Socket/Types.hsc" #-}

{-# LINE 1179 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1)) p ((30) :: CSaFamily)
{-# LINE 1180 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p port
{-# LINE 1181 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p flow
{-# LINE 1182 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (In6Addr addr)
{-# LINE 1183 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p scope
{-# LINE 1184 "Network/Socket/Types.hsc" #-}

-- | Read a 'SockAddr' from the given memory location.
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
  family <- ((\hsc_ptr -> peekByteOff hsc_ptr 1)) p
{-# LINE 1189 "Network/Socket/Types.hsc" #-}
  case family :: CSaFamily of

{-# LINE 1191 "Network/Socket/Types.hsc" #-}
    (1) -> do
{-# LINE 1192 "Network/Socket/Types.hsc" #-}
        str <- peekCAString (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p)
{-# LINE 1193 "Network/Socket/Types.hsc" #-}
        return (SockAddrUnix str)

{-# LINE 1195 "Network/Socket/Types.hsc" #-}
    (2) -> do
{-# LINE 1196 "Network/Socket/Types.hsc" #-}
        addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 1197 "Network/Socket/Types.hsc" #-}
        port <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 1198 "Network/Socket/Types.hsc" #-}
        return (SockAddrInet port addr)
    (30) -> do
{-# LINE 1200 "Network/Socket/Types.hsc" #-}
        port <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 1201 "Network/Socket/Types.hsc" #-}
        flow <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 1202 "Network/Socket/Types.hsc" #-}
        In6Addr addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 1203 "Network/Socket/Types.hsc" #-}
        scope <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 1204 "Network/Socket/Types.hsc" #-}
        return (SockAddrInet6 port flow addr scope)
    _ -> ioError $ userError $
      "Network.Socket.Types.peekSockAddr: address family '" ++
      show family ++ "' not supported."

------------------------------------------------------------------------

-- | The raw network byte order number is read using host byte order.
-- Therefore on little-endian architectures the byte order is swapped. For
-- example @127.0.0.1@ is represented as @0x0100007f@ on little-endian hosts
-- and as @0x7f000001@ on big-endian hosts.
--
-- For direct manipulation prefer 'hostAddressToTuple' and
-- 'tupleToHostAddress'.
type HostAddress = Word32

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(0x7f, 0, 0, 1)@
-- regardless of host endianness.
--
{- -- prop> tow == hostAddressToTuple (tupleToHostAddress tow) -}
hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple ha' =
    let ha = htonl ha'
        byte i = fromIntegral (ha `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in ntohl $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'hostAddress6ToTuple' and
-- 'tupleToHostAddress6'.
type HostAddress6 = (Word32, Word32, Word32, Word32)

-- | Converts 'HostAddress6' to representation-independent IPv6 octuple.
--
{- -- prop> (w1,w2,w3,w4,w5,w6,w7,w8) == hostAddress6ToTuple (tupleToHostAddress6 (w1,w2,w3,w4,w5,w6,w7,w8)) -}
hostAddress6ToTuple :: HostAddress6 -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
hostAddress6ToTuple (w3, w2, w1, w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

-- | Converts IPv6 octuple to 'HostAddress6'.
tupleToHostAddress6 :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> HostAddress6
tupleToHostAddress6 (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in (w7 `add` w6, w5 `add` w4, w3 `add` w2, w1 `add` w0)

-- The peek32 and poke32 functions work around the fact that the RFCs
-- don't require 32-bit-wide address fields to be present.  We can
-- only portably rely on an 8-bit field, s6_addr.

s6_addr_offset :: Int
s6_addr_offset = ((0))
{-# LINE 1268 "Network/Socket/Types.hsc" #-}

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i0 = do
    let i' = i0 * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i0 a = do
    let i' = i0 * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        x `sr` i = fromIntegral (x `shiftR` i) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

-- | Private newtype proxy for the Storable instance. To avoid orphan instances.
newtype In6Addr = In6Addr HostAddress6


{-# LINE 1296 "Network/Socket/Types.hsc" #-}

instance Storable In6Addr where
    sizeOf    _ = 16
{-# LINE 1299 "Network/Socket/Types.hsc" #-}
    alignment _ = 4
{-# LINE 1300 "Network/Socket/Types.hsc" #-}

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ In6Addr (a, b, c, d)

    poke p (In6Addr (a, b, c, d)) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d

------------------------------------------------------------------------
-- Read and Show instance for pattern-based integral newtypes

socktypeBijection :: Bijection SocketType String
socktypeBijection =
    [ (UnsupportedSocketType, "UnsupportedSocketType")
    , (Stream, "Stream")
    , (Datagram, "Datagram") 
    , (Raw, "Raw")
    , (RDM, "RDM")
    , (SeqPacket, "SeqPacket")
    , (NoSocketType, "NoSocketType")
    ]

instance Show SocketType where
    showsPrec = bijectiveShow socktypeBijection def
      where
        gst = "GeneralSocketType"
        def = defShow gst packSocketType _showInt

instance Read SocketType where
    readPrec = bijectiveRead socktypeBijection def
      where
        gst = "GeneralSocketType"
        def = defRead gst unpackSocketType _readInt

familyBijection :: Bijection Family String
familyBijection =
    [ (UnsupportedFamily, "UnsupportedFamily")
    , (AF_UNSPEC, "AF_UNSPEC")
    , (AF_UNIX, "AF_UNIX")
    , (AF_INET, "AF_INET")
    , (AF_INET6, "AF_INET6")
    , (AF_IMPLINK, "AF_IMPLINK")
    , (AF_PUP, "AF_PUP")
    , (AF_CHAOS, "AF_CHAOS")
    , (AF_NS, "AF_NS")
    , (AF_NBS, "AF_NBS")
    , (AF_ECMA, "AF_ECMA")
    , (AF_DATAKIT, "AF_DATAKIT")
    , (AF_CCITT, "AF_CCITT")
    , (AF_SNA, "AF_SNA")
    , (AF_DECnet, "AF_DECnet")
    , (AF_DLI, "AF_DLI")
    , (AF_LAT, "AF_LAT")
    , (AF_HYLINK, "AF_HYLINK")
    , (AF_APPLETALK, "AF_APPLETALK")
    , (AF_ROUTE, "AF_ROUTE")
    , (AF_NETBIOS, "AF_NETBIOS")
    , (AF_NIT, "AF_NIT")
    , (AF_802, "AF_802")
    , (AF_ISO, "AF_ISO")
    , (AF_OSI, "AF_OSI")
    , (AF_NETMAN, "AF_NETMAN")
    , (AF_X25, "AF_X25")
    , (AF_AX25, "AF_AX25")
    , (AF_OSINET, "AF_OSINET")
    , (AF_GOSSIP, "AF_GOSSIP")
    , (AF_IPX, "AF_IPX")
    , (Pseudo_AF_XTP, "Pseudo_AF_XTP")
    , (AF_CTF, "AF_CTF")
    , (AF_WAN, "AF_WAN")
    , (AF_SDL, "AF_SDL")
    , (AF_NETWARE, "AF_NETWARE")
    , (AF_NDD, "AF_NDD")
    , (AF_INTF, "AF_INTF")
    , (AF_COIP, "AF_COIP")
    , (AF_CNT, "AF_CNT")
    , (Pseudo_AF_RTIP, "Pseudo_AF_RTIP")
    , (Pseudo_AF_PIP, "Pseudo_AF_PIP")
    , (AF_SIP, "AF_SIP")
    , (AF_ISDN, "AF_ISDN")
    , (Pseudo_AF_KEY, "Pseudo_AF_KEY")
    , (AF_NATM, "AF_NATM")
    , (AF_ARP, "AF_ARP")
    , (Pseudo_AF_HDRCMPLT, "Pseudo_AF_HDRCMPLT")
    , (AF_ENCAP, "AF_ENCAP")
    , (AF_LINK, "AF_LINK")
    , (AF_RAW, "AF_RAW")
    , (AF_RIF, "AF_RIF")
    , (AF_NETROM, "AF_NETROM")
    , (AF_BRIDGE, "AF_BRIDGE")
    , (AF_ATMPVC, "AF_ATMPVC")
    , (AF_ROSE, "AF_ROSE")
    , (AF_NETBEUI, "AF_NETBEUI")
    , (AF_SECURITY, "AF_SECURITY")
    , (AF_PACKET, "AF_PACKET")
    , (AF_ASH, "AF_ASH")
    , (AF_ECONET, "AF_ECONET")
    , (AF_ATMSVC, "AF_ATMSVC")
    , (AF_IRDA, "AF_IRDA")
    , (AF_PPPOX, "AF_PPPOX")
    , (AF_WANPIPE, "AF_WANPIPE")
    , (AF_BLUETOOTH, "AF_BLUETOOTH")
    , (AF_CAN, "AF_CAN")
    ]

instance Show Family where
    showsPrec = bijectiveShow familyBijection def
      where
        gf = "GeneralFamily"
        def = defShow gf packFamily _showInt

instance Read Family where
    readPrec = bijectiveRead familyBijection def
      where
        gf = "GeneralFamily"
        def = defRead gf unpackFamily _readInt

-- Print "n" instead of "PortNum n".
instance Show PortNumber where
  showsPrec p (PortNum pn) = showsPrec p pn

-- Read "n" instead of "PortNum n".
instance Read PortNumber where
  readPrec = safeInt

------------------------------------------------------------------------
-- Helper functions

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 (fromIntegral nbytes)
