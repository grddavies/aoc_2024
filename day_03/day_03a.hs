{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text.IO qualified as TIO

type Op = (Int, Int)

parseOp :: Parser Op
parseOp = do
  string "mul("
  a <- decimal
  char ','
  b <- decimal
  char ')'
  return (a, b)

parseOps :: Parser [Op]
parseOps = many' parseOpOrSkip
  where
    parseOpOrSkip = parseOp <|> (anyChar *> parseOpOrSkip)

main :: IO ()
main = do
  inputText <- TIO.getContents
  ops <- case parseOnly parseOps inputText of
    Left err -> error err
    Right success -> return success
  print $ foldr (\(a, b) acc -> acc + (a * b)) 0 ops
