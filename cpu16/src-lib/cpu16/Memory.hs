{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

-- |
module Memory (new, Mem (..), Reg (..), Addr (..)) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive (MutableByteArray, Prim, alignment, newAlignedPinnedByteArray, readByteArray, sizeOf, writeByteArray)
import Data.Word (Word16, Word32)
import Lens.Micro.Platform (traverseOf, traversed)
import Relude ((&), (<&>))
import Utils (pp)

-- | 8 registers (A, B, C, X, Y, Z, I, J)
data Reg where
  A :: Reg
  B :: Reg
  C :: Reg
  X :: Reg
  Y :: Reg
  Z :: Reg
  I :: Reg
  J :: Reg
  deriving (Eq, Show, Enum, Bounded)

-- | The supported addressing modes
data Addr where
  Pc :: Addr
  Sp :: Addr
  O :: Addr
  Cycles :: Addr
  Reg :: Reg -> Addr
  Ram :: Word16 -> Addr
  deriving (Eq)

instance Show Addr where
  show Pc = "Pc"
  show Sp = "Sp"
  show O = "O"
  show Cycles = "Cycles"
  show (Reg r) = show r
  show (Ram w) = pp w

class From b a where
  from :: a -> b

class (PrimMonad m) => Mem a m where
  store :: Memory (PrimState m) -> Addr -> a -> m ()
  load :: Memory (PrimState m) -> Addr -> m a

-- copied from https://github.com/jaspervdj/dcpu16-hs/blob/master/src/Memory.hs
instance From Int Addr where
  from Pc = 0x0
  from Sp = 0x1
  from O = 0x2
  from Cycles = 0x3
  from (Reg r) = 0x8 + fromEnum r
  from (Ram w) = 0x16 + fromIntegral w

data Memory s where
  Memory :: MutableByteArray s -> Memory s

instance (PrimMonad m) => Mem Word16 m where
  store (Memory mem) addr w = writeByteArray mem (from addr) w
  load (Memory mem) addr = readByteArray mem (from addr)

new :: forall w m. (PrimMonad m, Mem w m, Num w, Prim w) => m (Memory (PrimState m))
new = do
  mem <- new'
  store @w mem Pc 0x0000
  store @w mem Sp 0xffff
  store @w mem O 0x0000
  store @w mem Cycles 0x0000
  [minBound .. maxBound] & traverseOf traversed \r -> store @w mem (Reg r) 0x0000
  [minBound .. maxBound] & traverseOf traversed \r -> store @w mem (Ram r) 0x0000
  return mem
  where
    new' :: m (Memory (PrimState m))
    new' = newAlignedPinnedByteArray len (alignment @w undefined) <&> Memory

    len :: Int
    !len = 0x8 + 0x8 + 0x10000
