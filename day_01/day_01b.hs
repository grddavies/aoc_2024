module Main where

import Data.MultiSet qualified as MultiSet

main :: IO ()
main = interact $ \input ->
  let (values, occurrences) = parseLists input
      ms = MultiSet.fromList occurrences
      counts = map (\x -> x * fromIntegral (MultiSet.occur x ms)) values
   in show $ sum counts

toTuple [a, b] = (read a, read b)
toTuple _ = error "Invalid input format"

parseLists :: String -> ([Int], [Int])
parseLists = unzip . map (toTuple . words) . lines
