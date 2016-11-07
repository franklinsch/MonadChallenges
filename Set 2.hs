import Prelude()
import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
  (==) (Just a) (Just b) = a == b
  (==) Nothing  Nothing  = True
  (==) _ _ = False

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

addMay :: Num a => Maybe a -> Maybe a -> Maybe a
addMay Nothing _ = Nothing
addMay _ Nothing = Nothing
addMay (Just a) (Just b) = Just (a + b)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = divVal
  where 
  xs = lookupMay k d 
  tail = case xs of
    Nothing    -> Nothing
    Just xs'   -> tailMay xs'
  maxTail = case tail of
    Nothing    -> Nothing
    Just tail' -> maximumMay tail'
  Just xs' = xs
  Just head = headMay xs'
  divVal = case maxTail of
    Nothing       -> Nothing
    Just maxTail' -> divMay (fromIntegral maxTail') (fromIntegral head)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f (Just a) = f a
chain f Nothing  = Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 g s
  = link (lookupMay s g) (\xs ->
    link (headMay xs) (\head ->
    link (tailMay xs) (\tail -> 
    link (maximumMay tail) (\maxTail ->
    divMay (fromIntegral maxTail) (fromIntegral head)))))

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries sals p1 p2
  = link (lookupMay p1 sals) (\p1Sal ->
    link (lookupMay p2 sals) (\p2Sal ->
    mkMaybe (p1Sal + p2Sal)))
    
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f a b
  = link a (\a ->
    link b (\b -> 
    mkMaybe (f a b)))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 sals p1 p2 = yLink (+) (lookupMay p1 sals) (lookupMay p2 sals)

mkMaybe :: a -> Maybe a
mkMaybe = Just
