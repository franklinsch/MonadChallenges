import Prelude()
import MCPrelude
import Set2
import Control.Arrow((&&&))

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

instance Monad Maybe where
  bind = link
  return = mkMaybe

instance Monad [] where
  bind = flip concatMap
  return = (:[])

newtype Gen t = Gen { runGen :: Seed -> (t, Seed) }

instance Monad Gen where
  bind gen func = Gen (\s -> uncurry (runGen . func) (runGen gen s))
  return a = Gen (\s -> (a, s))

evalGen :: Gen a -> Seed -> a
evalGen g s = fst (runGen g s)

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (g:gs) = bind g (\g' ->
                  bind (sequence gs) (\gs' ->
                  return (g':gs')))

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f a = bind a (\a -> return $ f a)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ga gb = bind ga (\aa -> 
                 bind gb (\bb ->
                 return $ f aa bb))

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = bind ma (\a ->
                    bind mb (\b ->
                    bind mc (\c ->
                    return $ f a b c)))

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join = flip bind id

ap :: Monad m => m (a -> b) -> m a -> m b
ap f a = (liftM2 id) f a

-- Set 1

fiveRands :: [Integer]
{-fiveRands = take 5 (tail (map fst (iterate (rand . snd) (0, mkSeed 1))))-}
fiveRands = evalGen (sequence (take 5 (repeat (Gen rand)))) (mkSeed 1)

randString3 :: String
{-randString3 = take 3 (tail (map fst (iterate (randLetter . snd) ('0', mkSeed 1))))-}
randString3 = evalGen (sequence (take 3 (repeat randLetter))) (mkSeed 1)

generalA :: (a -> b) -> Gen a -> Gen b
generalA = liftM

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

{-generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c-}
{-generalB2 f ga gb = -}
    {-genTwo ga (\aa -> -}
            {-genTwo gb (\bb ->-}
                        {-mkGen $ f aa bb))-}

{-generalPair2' :: Gen a -> Gen b -> Gen (a, b)-}
{-generalPair2' = generalB2 (,)-}

{-randPair'' = generalPair2' randLetter rand-}

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 = sequence

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = liftM2 (,)

{-repRandom :: [Gen a] -> Gen [a]-}
{-repRandom = ((.).(.)) (reverse . fst &&& snd) (flip (.) (mkGen []) . (flip (foldr (ap (flip (&&&) snd . (flip (.) fst . (flip (:) . fst))) . (flip (.) snd)))))-}
{---repRandom gens s = foldr gens-}

mkGen :: a -> Gen a
mkGen = return

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

randLetter :: Gen Char
randLetter = generalA toLetter (Gen rand)
    
randEven :: Gen Integer
randEven = generalA (*2) (Gen rand)

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) (Gen rand)

randPair = generalPair2 randLetter (Gen rand)

-- Set 2

{-headMay :: [a] -> Maybe a-}
{-headMay (x : _) = Just x-}
{-headMay [] = Nothing-}

{-tailMay :: [a] -> Maybe [a]-}
{-tailMay (_ : xs) = Just xs-}
{-tailMay [] = Nothing-}

{-lookupMay :: Eq a => a -> [(a,b)] -> Maybe b-}
{-lookupMay v ds = headMay $ map snd $ filter ((==) v.fst) ds-}

{-divMay :: (Eq a, Fractional a) => a -> a -> Maybe a-}
{-divMay _ 0 = Nothing-}
{-divMay x y = Just (x / y)-}

{-maximumMay :: Ord a => [a] -> Maybe a-}
{-maximumMay [] = Nothing -}
{-maximumMay xs = Just (foldr1 (max) xs)-}

{-minimumMay :: Ord a => [a] -> Maybe a-}
{-minimumMay [] = Nothing -}
{-minimumMay xs = Just (foldr1 (min) xs)-}

{-queryGreek d k = divVal-}
  {-where -}
  {-xs = lookupMay k d -}
  {-tail = case xs of-}
    {-Nothing    -> Nothing-}
    {-Just xs'   -> tailMay xs'-}
  {-maxTail = case tail of-}
    {-Nothing    -> Nothing-}
    {-Just tail' -> maximumMay tail'-}
  {-Just xs' = xs-}
  {-Just head = headMay xs'-}
  {-divVal = case maxTail of-}
    {-Nothing       -> Nothing-}
    {-Just maxTail' -> divMay (fromIntegral maxTail') (fromIntegral head)-}

{-chain :: (a -> Maybe b) -> Maybe a -> Maybe b-}
{-chain = (=<<)-}

{-link :: Maybe a -> (a -> Maybe b) -> Maybe b-}
{-link = bind-}

{-queryGreek2 :: GreekData -> String -> Maybe Double-}
{-queryGreek2 g s-}
  {-= link (lookupMay s g) (\xs ->-}
    {-link (headMay xs) (\head ->-}
    {-link (tailMay xs) (\tail -> -}
    {-link (maximumMay tail) (\maxTail ->-}
    {-divMay (fromIntegral maxTail) (fromIntegral head)))))-}

{-addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer-}
{-addSalaries sals p1 p2-}
  {-= link (lookupMay p1 sals) (\p1Sal ->-}
    {-link (lookupMay p2 sals) (\p2Sal ->-}
    {-mkMaybe (p1Sal + p2Sal)))-}
    
{-yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c-}
{-yLink = liftM2-}

{-addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer-}
{-addSalaries2 sals p1 p2 = yLink (+) (lookupMay p1 sals) (lookupMay p2 sals)-}

{-mkMaybe :: a -> Maybe a-}
{-mkMaybe = Just-}

{-tailProd :: Num a => [a] -> Maybe a-}
{-tailProd-}
  {-= (transMaybe product) . tailMay-}


{-tailSum :: Num a => [a] -> Maybe a-}
{-tailSum-}
  {-= (transMaybe sum) . tailMay-}

{-transMaybe :: (a -> b) -> Maybe a -> Maybe b-}
{-transMaybe = liftM-}

{-tailMax :: Ord a => [a] -> Maybe (Maybe a)-}
{-tailMax-}
  {-= (transMaybe maximumMay) . tailMay-}

{-tailMin :: Ord a => [a] -> Maybe (Maybe a)-}
{-tailMin-}
  {-= (transMaybe minimumMay) . tailMay-}

{-combine :: Maybe (Maybe a) -> Maybe a-}
{-combine Nothing = Nothing-}

-- Set 3

data Card = Card Int String

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs = return f `ap` as `ap` bs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = return f `ap` as `ap` bs `ap` cs

combStep :: [a -> b] -> [a] -> [b]
combStep = ap
