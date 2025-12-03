{-# LANGUAGE TypeApplications #-}

module Day02 where

import Data.List

type Id = String

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn k = foldr f [[]]
  where f x acc@(a:as)
          | x == k    = []:acc
          | otherwise = (x:a):as

parseRange :: String -> [Id]
parseRange s =
  let [btm, top] = map (read @Int) $ splitOn '-' s
  in map show $ [btm..top]

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Predicate for part1 rules
isInvalid :: Id -> Bool
isInvalid id =
  case halve id of
    (l, r) -> if (l == r) then True else False

-- Predicate for part2 rules
isInvalid' :: Id -> Bool
isInvalid' id =
  case halve id of
    (l, _) -> any f $ drop 1 $ inits l
  where f x = id `isPrefixOf` cycle x && (length id) `mod` (length x) == 0

part1 :: [Id] -> Int
part1 = sum . map (read @Int) . filter isInvalid

part2 :: [Id] -> Int
part2 = sum . map (read @Int) . filter isInvalid'

main :: IO ()
main = do
  f <- readFile "input.txt"
  let input = concat $ map parseRange $ splitOn ',' f
  print $ part1 input
