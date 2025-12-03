{-# LANGUAGE TypeApplications #-}

module Day03 where

import Data.Char
import Data.List

type Bank = [Int]

parseBank :: String -> Bank
parseBank = map digitToInt

calcJoltage :: Int -> Bank -> Int
calcJoltage 1 xs = maximum xs
calcJoltage n xs = 10^(n-1) * k + calcJoltage (n-1) rest
  where k = maximum $ take ((length xs) - (n - 1)) xs
        rest = drop 1 $ dropWhile (< k) xs

part1 :: [Bank] -> Int
part1 = sum . map (calcJoltage 2)

part2 :: [Bank] -> Int
part2 = sum . map (calcJoltage 12)

main :: IO ()
main = do
  f <- readFile "input.txt"
  let input = map parseBank (lines f)
  print $ part1 input
