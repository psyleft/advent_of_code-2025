{-# LANGUAGE TypeApplications #-}

module Day04 where

import Data.Maybe

import qualified Data.Map.Strict as M

type Coord = (Int, Int)
type Diagram = M.Map Coord Bool

parseRow :: Int -> String -> [(Coord, Bool)]
parseRow y = zip [(x, y) | x <- [0..]] . map f
  where f c = case c of
          '.'       -> False
          '@'       -> True
          otherwise -> undefined

makeDiagram :: [String] -> Diagram
makeDiagram = M.fromList . concat . map (uncurry parseRow) . zip [0..]

lookupAdjacent :: Coord -> Diagram -> [Bool]
lookupAdjacent (x, y) m = catMaybes $ map (($ m) . M.lookup) coords
  where coords = [(x+a, y+b) | a <- [(-1)..1], b <- [(-1)..1]]

isAccessible :: Coord -> Diagram -> Bool
isAccessible k = (< 5) . length . filter id . lookupAdjacent k

getAccessible :: Diagram -> [Coord]
getAccessible m = M.foldrWithKey f [] m
  where f k v acc =
          if v && isAccessible k m then k:acc else acc

part1 :: Diagram -> Int
part1 = length . getAccessible

part2 :: Diagram -> Int
part2 = loop 0
  where loop count m =
          let xs = getAccessible m
              m' = foldr M.delete m xs
          in case length xs of
            0 -> count
            n -> loop (count+n) m'

main :: IO ()
main = do
  f <- readFile "input.txt"
  let input = makeDiagram (lines f)
  print $ part1 input
