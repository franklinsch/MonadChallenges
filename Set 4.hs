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
generalA f g = (=<<) (return . f) g

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
