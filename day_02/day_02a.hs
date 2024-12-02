module Main where

main :: IO ()
main = interact $ \input ->
  let reports :: [[Int]]
      reports = map (map read . words) $ lines input
   in show $ length . filter isValid $ reports

isValid :: [Int] -> Bool
isValid report =
  let diffs = zipWith (-) report (tail report)
      absDiffs = abs <$> diffs
   in maximum absDiffs <= 3
        && (all (> 0) diffs || all (< 0) diffs)
