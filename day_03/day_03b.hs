{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.Text.IO qualified as TIO

data Op = Mul (Int, Int) | Do | Dont
  deriving (Show, Eq)

parseMul :: Parser Op
parseMul = do
  "mul("
  a <- decimal
  char ','
  b <- decimal
  char ')'
  return $ Mul (a, b)

parseDo :: Parser Op
parseDo = "do()" $> Do

parseDont :: Parser Op
parseDont = "don't()" $> Dont

parseOp :: Parser Op
parseOp = parseDo <|> parseDont <|> parseMul

parseOps :: Parser [Op]
parseOps = many' parseOpOrSkip
  where
    parseOpOrSkip = parseOp <|> (anyChar *> parseOpOrSkip)

-- Interpreter state
type State = ([Int], Bool)

evalOps :: [Op] -> State
evalOps = foldl applyOp ([], True)
  where
    applyOp :: State -> Op -> State
    applyOp (acc, run) Do = (acc, True)
    applyOp (acc, run) Dont = (acc, False)
    applyOp (acc, True) (Mul (x, y)) = (x * y : acc, True)
    applyOp (acc, False) _ = (acc, False)

main :: IO ()
main = do
  inputText <- TIO.getContents
  ops <- case parseOnly parseOps inputText of
    Left err -> error err
    Right success -> return success
  print $ sum $ fst . evalOps $ ops
