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

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 f (x:xs) ys zs = 
    concat (map (\g -> map g zs) (map (f x) ys)) ++ 
        (allCombs3 f xs ys zs)

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep (f:fs) as = map f as ++ combStep fs as

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs ys = combStep (map f xs) ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys zs = combStep (combStep (map f xs) ys) zs
