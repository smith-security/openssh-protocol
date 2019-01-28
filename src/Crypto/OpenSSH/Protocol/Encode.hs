-- |
-- OpenSSH protocol primitives as defined by <RFC4251 https://www.ietf.org/rfc/rfc4251.txt>.
--
module Crypto.OpenSSH.Protocol.Encode (
    text
  , string
  , uint32
  , uint64
  , mpint
  , time
  , utc
  ) where

import           Data.Bits (Bits (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Serialize
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Word (Word8, Word32, Word64)
import qualified Data.Vector.Primitive as Vector

import qualified Math.NumberTheory.Logarithms as Logarithms


text :: Text -> Serialize.Put
text =
  string . Text.encodeUtf8

string :: ByteString -> Serialize.Put
string bytes = do
  uint32 . fromIntegral . ByteString.length $ bytes
  Serialize.putByteString bytes

uint32 :: Word32 -> Serialize.Put
uint32 =
  Serialize.putWord32be

uint64 :: Word64 -> Serialize.Put
uint64 =
  Serialize.putWord64be

mpint :: Integer -> Serialize.Put
mpint n =
  string . Serialize.runPut $
    Vector.forM_ (expand n) $
      Serialize.put

time :: POSIXTime -> Serialize.Put
time =
  uint64 . floor . toRational

utc :: UTCTime -> Serialize.Put
utc =
  uint64 . floor . toRational . Time.utcTimeToPOSIXSeconds

expand :: Integer -> Vector.Vector Word8
expand n =
  let
    base = expand' n
  in
    if n == 0 then
      Vector.empty
    else if n > 0 && testBit (Vector.head base) 7 then
      Vector.cons 0 base
    else if n < 0 && not (testBit (Vector.head base) 7) then
      Vector.cons 0xff base
    else
      base

expand' :: Integer -> Vector.Vector Word8
expand' nn =
  let
    loop x n =
      let
        (n', w) = quotRem n 256
      in
        if n == 0 then x else loop (fromIntegral w : x) n'
    bytes =
      (Logarithms.integerLogBase 2 (abs nn) + 1) `quot` 8 + 1
  in
    if nn < 0 then
      expand' $ 2 ^ (8 * bytes) + nn
    else
      Vector.fromList . loop [] $ nn
