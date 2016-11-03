import Prelude()
import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay [] = Nothing

tailMay :: [a] -> Maybe [a]
tailMay (_ : xs) = Just xs
tailMay [] = Nothing

lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay v ds = headMay $ map snd $ filter ((==) v.fst) ds

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing 
maximumMay xs = Just (foldr1 (max) xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing 
minimumMay xs = Just (foldr1 (min) xs)
