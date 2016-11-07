import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = (map ((,) x) ys) ++ allPairs xs ys
