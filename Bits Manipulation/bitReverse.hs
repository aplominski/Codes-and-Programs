-- bitReverseDemo.hs
-- Demonstracja bitReverse64 w Haskellu

module Main where

import Data.Bits
import Data.Word
import Numeric (showHex)

-- | Bit-reverse for 64-bit words.
bitReverse64 :: Word64 -> Word64
bitReverse64 x0 =
  let x1 = ((x0 `shiftR` 1) .&. 0x5555555555555555) .|. ((x0 .&. 0x5555555555555555) `shiftL` 1)
      x2 = ((x1 `shiftR` 2) .&. 0x3333333333333333) .|. ((x1 .&. 0x3333333333333333) `shiftL` 2)
      x3 = ((x2 `shiftR` 4) .&. 0x0F0F0F0F0F0F0F0F) .|. ((x2 .&. 0x0F0F0F0F0F0F0F0F) `shiftL` 4)
      x4 = ((x3 `shiftR` 8) .&. 0x00FF00FF00FF00FF) .|. ((x3 .&. 0x00FF00FF00FF00FF) `shiftL` 8)
      x5 = ((x4 `shiftR` 16) .&. 0x0000FFFF0000FFFF) .|. ((x4 .&. 0x0000FFFF0000FFFF) `shiftL` 16)
      x6 = (x5 `shiftR` 32) .|. (x5 `shiftL` 32)
  in x6

-- Helper to display Word64 in hex
showW64Hex :: Word64 -> String
showW64Hex w = "0x" ++ showHex w ""

-- Example usage
main :: IO ()
main = do
    let x = 0x0123456789ABCDEF
    putStrLn $ "Original:  " ++ showW64Hex x
    let rev = bitReverse64 x
    putStrLn $ "Bit-reversed: " ++ showW64Hex rev
