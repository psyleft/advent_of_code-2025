{-# LANGUAGE TypeApplications #-}

module Day06 where

import Data.List

type Opr = (Int, Char)
type Problem = (Opr, [String])

parseProblems :: [String] -> [Problem]
parseProblems input =
  case unsnoc input of
    Nothing           -> []
    Just (nums, oprs) -> snd $ foldl' f (nums, []) (parseOprs oprs)
  where f (xs, acc) opr@(width, _) =
          let (parsed, rest) = parseNum width xs
          in (rest, (opr, parsed) : acc)

parseOprs :: String -> [Opr]
parseOprs = snd . foldr f (1, [])
  where f x (count, acc) = case x of
          ' '       -> (count+1, acc)
          otherwise -> (0, (count, x) : acc)

parseNum :: Int -> [String] -> ([String], [String])
parseNum n = unzip . map (fmap (drop 1) . splitAt n)

solveProblem :: Problem -> Int
solveProblem (opr, xs) =
  let xs' = map (read @Int) xs
  in case snd opr of
    '+' -> sum xs'
    '*' -> product xs'

solveProblem' :: Problem -> Int
solveProblem' (opr, xs) =
  let xs' = map (read @Int) $ transpose xs
  in case snd opr of
    '+' -> sum xs'
    '*' -> product xs'

part1 :: [Problem] -> Int
part1 = sum . map solveProblem

part2 :: [Problem] -> Int
part2 = sum . map solveProblem'

main :: IO ()
main = do
  f <- readFile "input.txt"
  let input = parseProblems (lines f)
  print $ part1 input
