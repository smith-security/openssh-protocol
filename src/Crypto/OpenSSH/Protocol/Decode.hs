-- |
-- OpenSSH protocol primitives as defined by <RFC4251 https://www.ietf.org/rfc/rfc4251.txt>.
--
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.OpenSSH.Protocol.Decode (
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
import qualified Data.Serialize as Serialize
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import           Data.Word (Word32, Word64)
import qualified Data.Vector.Primitive as Vector


text :: Serialize.Get Text
text =
  Text.decodeUtf8 <$> string

string :: Serialize.Get ByteString
string =
  uint32 >>= Serialize.getBytes . fromIntegral

uint32 :: Serialize.Get Word32
uint32 =
  Serialize.getWord32be

uint64 :: Serialize.Get Word64
uint64 =
  Serialize.getWord64be

mpint :: Serialize.Get Integer
mpint = do
  size <- fromIntegral <$> uint32
  bytes <- Vector.replicateM size Serialize.getWord8
  let
    value = Vector.foldl' (\acc el -> (acc `shiftL` 8) + fromIntegral el) (0 :: Integer) bytes
    negative = size > 0 && testBit (Vector.head bytes) 7
  pure $
    if negative then value - 2 ^ (size * 8) else value

time :: Serialize.Get POSIXTime
time =
  fromIntegral <$> uint64

utc :: Serialize.Get UTCTime
utc =
  Time.posixSecondsToUTCTime <$> time
