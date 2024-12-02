module Main where

import Data.List (sort, sortOn)

main :: IO ()
main = interact $ show . sumDiff . map (toTuple . words) . lines
  where
    toTuple [a, b] = (read a, read b)
    toTuple _ = error "Invalid input format"

sumDiff :: [(Int, Int)] -> Int
sumDiff xs = sum $ zipWith (\(a, _) (_, b) -> abs (a - b)) (sortOn fst xs) (sortOn snd xs)
