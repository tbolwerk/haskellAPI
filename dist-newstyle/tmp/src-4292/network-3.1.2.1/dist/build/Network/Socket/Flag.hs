{-# LINE 1 "Network/Socket/Flag.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}



module Network.Socket.Flag where

import qualified Data.Semigroup as Sem

import Network.Socket.Imports

{-
import Network.Socket.ReadShow

import qualified Text.Read as P
-}

-- | Message flags. To combine flags, use '(<>)'.
newtype MsgFlag = MsgFlag { fromMsgFlag :: CInt }
                deriving (Show, Eq, Ord, Num, Bits)

instance Sem.Semigroup MsgFlag where
    (<>) = (.|.)

instance Monoid MsgFlag where
    mempty = MsgFlag 0

{-# LINE 31 "Network/Socket/Flag.hsc" #-}

-- | Send or receive OOB(out-of-bound) data.
pattern MSG_OOB :: MsgFlag

{-# LINE 35 "Network/Socket/Flag.hsc" #-}
pattern MSG_OOB = MsgFlag (1)
{-# LINE 36 "Network/Socket/Flag.hsc" #-}

{-# LINE 39 "Network/Socket/Flag.hsc" #-}

-- | Bypass routing table lookup.
pattern MSG_DONTROUTE :: MsgFlag

{-# LINE 43 "Network/Socket/Flag.hsc" #-}
pattern MSG_DONTROUTE = MsgFlag (4)
{-# LINE 44 "Network/Socket/Flag.hsc" #-}

{-# LINE 47 "Network/Socket/Flag.hsc" #-}

-- | Peek at incoming message without removing it from the queue.
pattern MSG_PEEK :: MsgFlag

{-# LINE 51 "Network/Socket/Flag.hsc" #-}
pattern MSG_PEEK = MsgFlag (2)
{-# LINE 52 "Network/Socket/Flag.hsc" #-}

{-# LINE 55 "Network/Socket/Flag.hsc" #-}

-- | End of record.
pattern MSG_EOR :: MsgFlag

{-# LINE 59 "Network/Socket/Flag.hsc" #-}
pattern MSG_EOR = MsgFlag (8)
{-# LINE 60 "Network/Socket/Flag.hsc" #-}

{-# LINE 63 "Network/Socket/Flag.hsc" #-}

-- | Received data is truncated. More data exist.
pattern MSG_TRUNC :: MsgFlag

{-# LINE 67 "Network/Socket/Flag.hsc" #-}
pattern MSG_TRUNC = MsgFlag (16)
{-# LINE 68 "Network/Socket/Flag.hsc" #-}

{-# LINE 71 "Network/Socket/Flag.hsc" #-}

-- | Received control message is truncated. More control message exist.
pattern MSG_CTRUNC :: MsgFlag

{-# LINE 75 "Network/Socket/Flag.hsc" #-}
pattern MSG_CTRUNC = MsgFlag (32)
{-# LINE 76 "Network/Socket/Flag.hsc" #-}

{-# LINE 79 "Network/Socket/Flag.hsc" #-}

-- | Wait until the requested number of bytes have been read.
pattern MSG_WAITALL :: MsgFlag

{-# LINE 83 "Network/Socket/Flag.hsc" #-}
pattern MSG_WAITALL = MsgFlag (64)
{-# LINE 84 "Network/Socket/Flag.hsc" #-}

{-# LINE 87 "Network/Socket/Flag.hsc" #-}

{-
msgFlagPairs :: [Pair MsgFlag String]
msgFlagPairs =
    [ (MSG_OOB, "MSG_OOB")
    , (MSG_DONTROUTE, "MSG_DONTROUTE")
    , (MSG_PEEK, "MSG_PEEK")
    , (MSG_EOR, "MSG_EOR")
    , (MSG_TRUNC, "MSG_TRUNC")
    , (MSG_CTRUNC, "MSG_CTRUNC")
    , (MSG_WAITALL, "MSG_WAITALL")
    ]
-}
