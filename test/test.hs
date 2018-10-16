{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Functions

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

stringToByteString :: Property
stringToByteString = property $ do
    xs <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
    (oldDecodeUtf8 . (oldEncodeUtf8 :: String -> B.ByteString)) xs === (newDecodeUtf8 . (newEncodeUtf8 :: String -> B.ByteString)) xs

stringToLazyByteString :: Property
stringToLazyByteString = property $ do
    xs <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
    (oldDecodeUtf8 . (oldEncodeUtf8 :: String -> LB.ByteString)) xs === (newDecodeUtf8 . (newEncodeUtf8 :: String -> LB.ByteString)) xs

tests :: IO Bool
tests =
    checkSequential $$(discover)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _ <- seq () tests

  return ()
