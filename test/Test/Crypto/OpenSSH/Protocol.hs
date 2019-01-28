{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Crypto.OpenSSH.Protocol where

import qualified Crypto.OpenSSH.Protocol.Encode as Encode
import qualified Crypto.OpenSSH.Protocol.Decode as Decode

import qualified Data.Serialize as Serialize
import qualified Data.Time as Time

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


trip :: (Eq a, Show a) => (a -> Serialize.Put) -> Serialize.Get a -> Gen a -> PropertyT IO ()
trip put get gen = do
  a <- forAll gen
  Right a === Serialize.runGet get (Serialize.runPut $ put a)

prop_mpint :: Property
prop_mpint =
  withTests 10000 . property $ do
    trip Encode.mpint Decode.mpint $
      Gen.integral (Range.linearFrom 0 (-999999999999999999999999) 999999999999999999999999)

prop_text :: Property
prop_text =
  withTests 10000 . property $ do
    trip Encode.text Decode.text $
      Gen.text (Range.linear 0 100) Gen.alphaNum

prop_string :: Property
prop_string =
  withTests 10000 . property $ do
    trip Encode.string Decode.string $
      Gen.utf8 (Range.linear 0 100) Gen.alphaNum

prop_uint32 :: Property
prop_uint32 =
  withTests 10000 . property $ do
    trip Encode.uint32 Decode.uint32 $
      Gen.integral Range.linearBounded

prop_uint64 :: Property
prop_uint64 =
  withTests 10000 . property $ do
    trip Encode.uint64 Decode.uint64 $
      Gen.integral Range.linearBounded

prop_time :: Property
prop_time =
  withTests 10000 . property $ do
    trip Encode.time Decode.time $
      fromIntegral <$> Gen.int (Range.linear 0 maxBound)

prop_utc :: Property
prop_utc =
  withTests 10000 . property $ do
    trip Encode.utc Decode.utc $ do
      day <- Time.ModifiedJulianDay <$> Gen.integral (Range.linearFrom 50000 58500 90000)
      time <- fromInteger <$> Gen.integral (Range.constant 0 86399)
      pure $ Time.UTCTime day time

tests :: IO Bool
tests =
  checkParallel $$(discover)
