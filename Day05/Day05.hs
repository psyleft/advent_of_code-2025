{-# LANGUAGE TypeApplications #-}

module Day05 where

import Data.List

type Id = Int
type Range = (Id, Id)
type Inventory = ([Range], [Id])

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn k = foldr f [[]]
  where f x acc@(a:as)
          | x == k    = []:acc
          | otherwise = (x:a):as

parseInventory :: [String] -> Inventory
parseInventory rows = (parseRanges ranges, parseProducts products)
  where [ranges, products] = splitOn "" rows

parseRanges :: [String] -> [Range]
parseRanges = normalizeRanges . sort . foldr f []
  where f x acc =
          let [btm, top] = map (read @Int) $ splitOn '-' x
          in (btm, top):acc

parseProducts :: [String] -> [Id]
parseProducts = sort . map (read @Int)

normalizeRanges :: [Range] -> [Range]
normalizeRanges = foldr f []
  where f x@(x_btm, x_top) acc =
          case acc of
            []                -> [x]
            (y_btm, y_top):ys ->
              if x_top >= y_btm then
                (x_btm, max x_top y_top):ys
              else
                x:acc

inRanges :: Id -> [Range] -> ([Range], Bool)
inRanges _ [] = ([], False)
inRanges n ranges@((btm, top):rest)
  | n < btm              = (ranges, False)
  | n >= btm && n <= top = (ranges, True)
  | n > top              = n `inRanges` rest

part1 :: Inventory -> Int
part1 (fresh, items) = snd $ foldl' f (fresh, 0) items
  where f (ranges, acc) x =
          case x `inRanges` ranges of
            (r, False) -> (r, acc)
            (r, True)  -> (r, acc+1)

part2 :: Inventory -> Int
part2 (fresh, _) = foldr f 0 fresh
  where f (btm, top) acc = acc + (top - (btm - 1))

main :: IO ()
main = do
  f <- readFile "input.txt"
  let input = parseInventory (lines f)
  print $ part1 input
