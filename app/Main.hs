{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text.IO qualified as TIO (getLine, putStr)
import Parser (exprParser)
import System.IO (hFlush, stdout)
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
  input <- readLine
  unless (input == ":quit") $ putParsed input >> main

readLine :: IO Text
readLine = TIO.putStr "$ " >> hFlush stdout >> TIO.getLine

putParsed :: Text -> IO ()
putParsed = parseTest exprParser
