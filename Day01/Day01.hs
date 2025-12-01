{-# LANGUAGE TypeApplications #-}

module Day01 where

import Data.Tuple

type Dial = Int
type Turn = Int

parseInput :: String -> Int
parseInput (c:cs)
  | c == 'L'  = (-1) * read @Int cs
  | c == 'R'  = read @Int cs
  | otherwise = 0

turnDial :: Dial -> Turn -> (Dial, Int)
turnDial p x = wrap (p+x)
  where wrap n
          | n > 0 = swap $ (p+x) `divMod` 100
          | n < 0 =
            let
              d = (abs n) `div` 100
              m = n `mod` 100
            in if (p == 0) then (m, d) else (m, d+1)
          | otherwise = (0, 1)

part1 :: [Turn] -> Int
part1 = snd . foldl' f (50, 0)
  where f (p, acc) x =
          let
            (p', _) = turnDial p x
          in case p' of
            0 -> (p', acc+1)
            otherwise -> (p', acc)

part2 :: [Turn] -> Int
part2 = snd . foldl' f (50, 0)
  where f (p, acc) x = (+acc) <$> turnDial p x

main :: IO ()
main = do
  f <- readFile "input.txt"
  let input = map parseInput $ lines f
  print $ part1 input
