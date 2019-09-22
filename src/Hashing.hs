#!/usr/bin/env stack
-- stack --resolver lts-12.26 script

{-# LANGUAGE PackageImports #-}

import           "cryptonite" Crypto.Hash             (hash, SHA256 (..), SHA3_512 (..), Digest)
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest :: Digest SHA3_512
      digest = hash bs
  putStrLn $ "SHA3_512 hash: " ++ show digest
