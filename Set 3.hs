import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allCombs (,)

data Card = Card Int String

instance Show Card where
  show (Card a b) = show a ++ b

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (x:xs) ys = (map (f x) ys) ++ allCombs f xs ys
