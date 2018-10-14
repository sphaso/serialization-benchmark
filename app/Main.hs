module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Criterion.Main

import Cases
import Functions

correct :: Bool
correct = all (\t -> old t == new t) [test1, test2, test3]
            where
                  old = oldDecodeUtf8 . (oldEncodeUtf8 :: String -> B.ByteString)
                  new = newDecodeUtf8 . (newEncodeUtf8 :: String -> B.ByteString)

strictBenchmarkGroup :: String -> B.ByteString -> [Benchmark]
strictBenchmarkGroup t bt = [
                              bench "old encode"  $ whnf (oldEncodeUtf8 :: String -> B.ByteString) t
                            , bench "old decode"  $ whnf oldDecodeUtf8 bt
                            , bench "new encode"  $ whnf (newEncodeUtf8 :: String -> B.ByteString) t
                            , bench "new decode"  $ whnf newDecodeUtf8 bt
                            ]

lazyBenchmarkGroup :: String -> LB.ByteString -> [Benchmark]
lazyBenchmarkGroup t bt = [
                            bench "old encode"  $ whnf (oldEncodeUtf8 :: String -> LB.ByteString) t
                          , bench "old decode"  $ whnf oldDecodeUtf8 bt
                          , bench "new encode"  $ whnf (newEncodeUtf8 :: String -> LB.ByteString) t
                          , bench "new decode"  $ whnf newDecodeUtf8 bt
                          ]

makeGroup :: String -> (String -> b -> [Benchmark]) -> [b] -> Benchmark
makeGroup label function [bt1, bt2, bt3] = bgroup label [
                                                          bgroup "test 1" $ function test1 bt1
                                                        , bgroup "test 2" $ function test2 bt2
                                                        , bgroup "test 3" $ function test3 bt3
                                                        ]

main :: IO ()
main = defaultMain [
                      makeGroup "strict" strictBenchmarkGroup [btest1, btest2, btest3]
                    , makeGroup "lazy" lazyBenchmarkGroup [lbtest1, lbtest2, lbtest3]
                   ]
