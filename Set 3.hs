import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = (map ((,) x) ys) ++ allPairs xs ys

data Card = Card Int String

instance Show Card where
  show (Card a b) = show a ++ b

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) ys = (map (Card x) ys) ++ allCards xs ys
