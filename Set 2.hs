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

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = divVal
  where 
  xs = lookupMay k d 
  tail = case xs of
    Nothing    -> Nothing
    Just xs'   -> tailMay xs'
  maxTail = case tail of
    Nothing    -> Nothing
    Just tail  -> maximumMay tail
  head = case xs of
    Nothing    -> Nothing
    Just xs'   -> headMay xs'
  divVal = case head of
    Nothing    -> Nothing
    Just head' -> case maxTail of 
      Nothing -> Nothing
      Just maxTail' -> divMay (fromIntegral maxTail') (fromIntegral head')

