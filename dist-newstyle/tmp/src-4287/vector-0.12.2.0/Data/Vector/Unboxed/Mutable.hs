{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Vector.Unboxed.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Mutable adaptive unboxed vectors
--

module Data.Vector.Unboxed.Mutable (
  -- * Mutable vectors of primitive types
  MVector(..), IOVector, STVector, Unbox,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Zipping and unzipping
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Accessing individual elements
  read, write, modify, swap,
  unsafeRead, unsafeWrite, unsafeModify, unsafeSwap,

  -- * Modifying vectors
  nextPermutation,

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove
) where

import Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Fusion.Util ( delayed_min )
import Control.Monad.Primitive

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail,
                        zip, zip3, unzip, unzip3 )

-- don't import an unused Data.Vector.Internal.Check
#define NOT_VECTOR_MODULE
#include "vector.h"

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: Unbox a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty
null :: Unbox a => MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Unbox a
      => Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
{-# INLINE slice #-}
slice = G.slice

take :: Unbox a => Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

drop :: Unbox a => Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

splitAt :: Unbox a => Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

init :: Unbox a => MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

tail :: Unbox a => MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Unbox a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeTake :: Unbox a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: Unbox a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

unsafeInit :: Unbox a => MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: Unbox a => MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: Unbox a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Unbox a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The vector content
--   is uninitialized, which means it is filled with whatever underlying memory
--   buffer happens to contain.
--
-- @since 0.5
unsafeNew :: (PrimMonad m, Unbox a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, Unbox a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, Unbox a) => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, Unbox a)
      => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow an unboxed vector by the given number of elements. The number must be
-- non-negative. Same semantics as in `G.grow` for generic vector.
--
-- ====__Examples__
--
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> import qualified Data.Vector.Unboxed.Mutable as MVU
-- >>> mv <- VU.thaw $ VU.fromList ([('a', 10), ('b', 20), ('c', 30)] :: [(Char, Int)])
-- >>> mv' <- MVU.grow mv 2
--
-- Extra memory at the end of the newly allocated vector is initialized to 0
-- bytes, which for `Unbox` instance will usually correspond to some default
-- value for a particular type, eg. @0@ for @Int@, @False@ for @Bool@,
-- etc. However, if `unsafeGrow` was used instead this would not have been
-- guaranteed and some garbage would be there instead:
--
-- >>> VU.unsafeFreeze mv'
-- [('a',10),('b',20),('c',30),('\NUL',0),('\NUL',0)]
--
-- Having the extra space we can write new values in there:
--
-- >>> MVU.write mv' 3 ('d', 999)
-- >>> VU.unsafeFreeze mv'
-- [('a',10),('b',20),('c',30),('d',999),('\NUL',0)]
--
-- It is important to note that the source mutable vector is not affected when
-- the newly allocated one is mutated.
--
-- >>> MVU.write mv' 2 ('X', 888)
-- >>> VU.unsafeFreeze mv'
-- [('a',10),('b',20),('X',888),('d',999),('\NUL',0)]
-- >>> VU.unsafeFreeze mv
-- [('a',10),('b',20),('c',30)]
--
-- @since 0.5
grow :: (PrimMonad m, Unbox a)
              => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be non-negative but
-- this is not checked. Same semantics as in `G.unsafeGrow` for generic vector.
--
-- @since 0.5
unsafeGrow :: (PrimMonad m, Unbox a)
               => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
modify :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap


-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite
    :: (PrimMonad m, Unbox a) =>  MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap
    :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Unbox a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, Unbox a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, Unbox a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, Unbox a)
                          => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- | Compute the next (lexicographically) permutation of given vector in-place.
--   Returns False when input is the last permutation
nextPermutation :: (PrimMonad m,Ord e,Unbox e) => MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = G.nextPermutation

#define DEFINE_MUTABLE
#include "unbox-tuple-instances"
