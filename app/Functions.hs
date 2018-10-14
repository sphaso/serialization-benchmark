{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Functions where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT


class ConvertUtf8 b where
    oldEncodeUtf8 :: String -> b
    oldDecodeUtf8 :: b -> String
    newEncodeUtf8 :: String -> b
    newDecodeUtf8 :: b -> String

instance ConvertUtf8 B.ByteString where
    oldEncodeUtf8 = BU.fromString
    oldDecodeUtf8 = BU.toString
    newEncodeUtf8 = T.encodeUtf8 . T.pack
    newDecodeUtf8 = T.unpack . T.decodeUtf8

instance ConvertUtf8 LB.ByteString where
    oldEncodeUtf8 = LBU.fromString
    oldDecodeUtf8 = LBU.toString
    newEncodeUtf8 = LT.encodeUtf8 . LT.pack
    newDecodeUtf8 = LT.unpack . LT.decodeUtf8
