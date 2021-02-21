-- |
module Instr where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word16, Word8)
import Memory (Reg)
import Relude ((&))
import Utils (pp)

class Iso a b where
  encode :: a -> b
  decode :: b -> a

class Cycles a where
  cycles :: a -> Cycle

data Cycle where
  Cycle :: Int -> Cycle
  Plus :: (Int -> Int) -> Cycle

instance Semigroup Cycle where
  Cycle a <> Cycle b = a + b & Cycle
  Cycle a <> Plus f = f . (+ a) & Plus
  Plus f <> Cycle a = (+ a) . f & Plus
  Plus fa <> Plus fb = fb . fa & Plus

instance Cycles Int where
  cycles = Cycle

data Basic where
  Set :: Basic
  Add :: Basic
  Sub :: Basic
  Mul :: Basic
  Mli :: Basic
  Div :: Basic
  Dvi :: Basic
  Mod :: Basic
  Mdi :: Basic
  And :: Basic
  Bor :: Basic
  Xor :: Basic
  Shr :: Basic
  Asr :: Basic
  Shl :: Basic
  Ifb :: Basic
  Ifc :: Basic
  Ife :: Basic
  Ifn :: Basic
  Ifg :: Basic
  Ifa :: Basic
  Ifl :: Basic
  Ifu :: Basic
  Adx :: Basic
  Sbx :: Basic
  Sti :: Basic
  Std :: Basic
  deriving (Show)

data Special where
  Jsr :: Special
  Int :: Special
  Iag :: Special
  Ias :: Special
  Rfi :: Special
  Iaq :: Special
  Hwn :: Special
  Hwq :: Special
  Hwi :: Special
  deriving (Show)

data Instr a where
  Basic :: Basic -> a -> a -> Instr a
  Special :: Special -> a -> Instr a
  Unknown :: Word16 -> Instr a
  deriving (Show)

data Operand where
  OReg :: Reg -> Operand
  ORefReg :: Reg -> Operand
  ORefRegNext :: Reg -> Operand
  OPop :: Operand
  OPeek :: Operand
  OPush :: Operand
  OSp :: Operand
  OPc :: Operand
  OEx :: Operand
  ORefWord :: Operand
  ONextWord :: Operand
  OImm :: Word16 -> Operand
  deriving (Show)

instance Cycles Operand where
  cycles o =
    if
        | ORefRegNext _ <- o -> cycles @Int 1
        | ONextWord <- o -> cycles @Int 1
        | ORefWord <- o -> cycles @Int 1
        | otherwise -> cycles @Int 0

instance Cycles Basic where
  cycles = \case
    Set -> cycles @Int 1
    Add -> cycles @Int 2
    Sub -> cycles @Int 2
    Mul -> cycles @Int 2
    Mli -> cycles @Int 2
    Div -> cycles @Int 3
    Dvi -> cycles @Int 3
    Mod -> cycles @Int 3
    Mdi -> cycles @Int 3
    And -> cycles @Int 1
    Bor -> cycles @Int 1
    Xor -> cycles @Int 1
    Shr -> cycles @Int 1
    Asr -> cycles @Int 1
    Shl -> cycles @Int 1
    Ifb -> Plus (+ 2)
    Ifc -> Plus (+ 2)
    Ife -> Plus (+ 2)
    Ifn -> Plus (+ 2)
    Ifg -> Plus (+ 2)
    Ifa -> Plus (+ 2)
    Ifl -> Plus (+ 2)
    Ifu -> Plus (+ 2)
    Adx -> cycles @Int 3
    Sbx -> cycles @Int 3
    Sti -> cycles @Int 2
    Std -> cycles @Int 2

instance Cycles Special where
  cycles = \case
    Jsr -> cycles @Int 3
    Int -> cycles @Int 4
    Iag -> cycles @Int 1
    Ias -> cycles @Int 1
    Rfi -> cycles @Int 3
    Iaq -> cycles @Int 2
    Hwn -> cycles @Int 2
    Hwq -> cycles @Int 4
    Hwi -> Plus (+ 4)

instance Cycles (Instr Operand) where
  cycles (Basic op a b) = cycles a <> cycles b <> cycles op
  cycles (Special op a) = cycles a <> cycles op

instance Iso Operand Word16 where
  encode = \case
    OReg r -> unreg r
    ORefReg r -> 0x08 + unreg r
    ORefRegNext r -> 0x10 + unreg r
    OPop -> 0x18
    OPeek -> 0x19
    OPush -> 0x1a
    OSp -> 0x1b
    OPc -> 0x1c
    OEx -> 0x1d
    ORefWord -> 0x1e
    ONextWord -> 0x1f
    OImm w -> 0x20 + w
    where
      unreg = fromIntegral . fromEnum
  decode w =
    if
        | w <= 0x7 -> w & OReg . reg
        | w <= 0x0f -> w - 0x08 & ORefReg . reg
        | w <= 0x17 -> w - 0x10 & ORefRegNext . reg
        | 0x20 <= w -> w - 0x20 & OImm
        | 0x18 <- w -> OPop
        | 0x19 <- w -> OPeek
        | 0x1a <- w -> OPush
        | 0x1b <- w -> OSp
        | 0x1c <- w -> OPc
        | 0x1d <- w -> OEx
        | 0x1e <- w -> ORefWord
        | 0x1f <- w -> ONextWord
        | otherwise -> "Operand not supported: " <> pp w & error
    where
      reg = toEnum . fromIntegral

instance Iso Basic Word16 where
  encode = \case
    Set -> 0x1
    Add -> 0x2
    Sub -> 0x3
    Mul -> 0x4
    Mli -> 0x5
    Div -> 0x6
    Dvi -> 0x7
    Mod -> 0x8
    Mdi -> 0x9
    And -> 0xa
    Bor -> 0xb
    Xor -> 0xc
    Shr -> 0xd
    Asr -> 0xe
    Shl -> 0xf
    Ifb -> 0x10
    Ifc -> 0x11
    Ife -> 0x12
    Ifn -> 0x13
    Ifg -> 0x14
    Ifa -> 0x15
    Ifl -> 0x16
    Ifu -> 0x17
    Adx -> 0x1a
    Sbx -> 0x1b
    Sti -> 0x1e
    Std -> 0x1f
  decode w = case w of
    0x1 -> Set
    0x2 -> Add
    0x3 -> Sub
    0x4 -> Mul
    0x5 -> Mli
    0x6 -> Div
    0x7 -> Dvi
    0x8 -> Mod
    0x9 -> Mdi
    0xa -> And
    0xb -> Bor
    0xc -> Xor
    0xd -> Shr
    0xe -> Asr
    0xf -> Shl
    0x10 -> Ifb
    0x11 -> Ifc
    0x12 -> Ife
    0x13 -> Ifn
    0x14 -> Ifg
    0x15 -> Ifa
    0x16 -> Ifl
    0x17 -> Ifu
    0x1a -> Adx
    0x1b -> Sbx
    0x1e -> Sti
    0x1f -> Std

instance Iso Special Word16 where
  encode = \case
    Jsr -> 0x01
    Int -> 0x08
    Iag -> 0x09
    Ias -> 0x0a
    Rfi -> 0x0b
    Iaq -> 0x0c
    Hwn -> 0x10
    Hwq -> 0x11
    Hwi -> 0x12

  decode = \case
    0x01 -> Jsr
    0x08 -> Int
    0x09 -> Iag
    0x0a -> Ias
    0x0b -> Rfi
    0x0c -> Iaq
    0x10 -> Hwn
    0x11 -> Hwq
    0x12 -> Hwi

instance Iso (Instr Operand) Word16 where
  encode (Basic op a b) = (encode b `shiftL` 10) .|. (encode a `shiftL` 5) .|. encode op
  encode (Special op a) = (encode a `shiftL` 10) .|. (encode op `shiftL` 5)
  encode (Unknown w) = w
  decode w =
    if
        | 0x01 <- ooooo -> basic Set
        | 0x02 <- ooooo -> basic Add
        | 0x03 <- ooooo -> basic Sub
        | 0x04 <- ooooo -> basic Mul
        | 0x05 <- ooooo -> basic Mli
        | 0x06 <- ooooo -> basic Div
        | 0x07 <- ooooo -> basic Dvi
        | 0x08 <- ooooo -> basic Mod
        | 0x09 <- ooooo -> basic Mdi
        | 0x0a <- ooooo -> basic And
        | 0x0b <- ooooo -> basic Bor
        | 0x0c <- ooooo -> basic Xor
        | 0x0d <- ooooo -> basic Shr
        | 0x0e <- ooooo -> basic Asr
        | 0x0f <- ooooo -> basic Shl
        | 0x10 <- ooooo -> basic Ifb
        | 0x11 <- ooooo -> basic Ifc
        | 0x12 <- ooooo -> basic Ife
        | 0x13 <- ooooo -> basic Ifn
        | 0x14 <- ooooo -> basic Ifg
        | 0x15 <- ooooo -> basic Ifa
        | 0x16 <- ooooo -> basic Ifl
        | 0x17 <- ooooo -> basic Ifu
        | 0x1a <- ooooo -> basic Adx
        | 0x1b <- ooooo -> basic Sbx
        | 0x1e <- ooooo -> basic Sti
        | 0x1f <- ooooo -> basic Std
        | 0x00 <- ooooo ->
          if
              | 0x01 <- aaaaa -> special Jsr
              | 0x08 <- aaaaa -> special Int
              | 0x09 <- aaaaa -> special Iag
              | 0x0a <- aaaaa -> special Ias
              | 0x0b <- aaaaa -> special Rfi
              | 0x0c <- aaaaa -> special Iaq
              | 0x10 <- aaaaa -> special Hwn
              | 0x11 <- aaaaa -> special Hwq
              | 0x12 <- aaaaa -> special Hwi
              | otherwise -> unknown
        | otherwise -> unknown
    where
      basic :: Basic -> Instr Operand
      basic o = Basic o a b
      special :: Special -> Instr Operand
      special o = Special o b
      ooooo = w .&. 0x1f
      aaaaa = (w `shiftR` 5) .&. 0x1f
      bbbbbb = (w `shiftR` 10) .&. 0x3f
      a = decode aaaaa
      b = decode bbbbbb
      unknown = Unknown w
