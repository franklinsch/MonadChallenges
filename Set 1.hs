import MCPrelude
import Control.Arrow
import Control.Monad

type Gen t = Seed -> (t, Seed)

fiveRands :: [Integer]
fiveRands = take 5 (tail (map fst (iterate (rand . snd) (0, mkSeed 1))))

randString3 :: String
randString3 = take 3 (tail (map fst (iterate (randLetter . snd) ('0', mkSeed 1))))

generalA :: (a -> b) -> Gen a -> Gen b
generalA = (.) . (flip (&&&) snd) . (flip (.) fst)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = flip (.) ((flip (.) (((&&&) fst) . (flip (.) snd))) . (flip (.))) . (.) . (.) . morph

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

repRandom :: [Gen a] -> Gen [a]
repRandom = ((.).(.)) (reverse . fst &&& snd) (flip (.) (mkGen []) . (flip (foldr (ap (flip (&&&) snd . (flip (.) fst . (flip (:) . fst))) . (flip (.) snd)))))
--repRandom gens s = foldr gens

mkGen :: a -> Gen a
mkGen = (,) 

genTwo :: Gen a -> (a -> Gen b) -> Gen b
--genTwo gen func seed = uncurry func (gen seed)
genTwo = flip (.) uncurry . (flip (.)) 

randLetter :: Gen Char
randLetter = generalA toLetter rand
    
randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

randPair = generalPair2 randLetter rand

morph :: (a -> b -> t) -> (a, (b, c)) -> (t, c)
morph = uncurry . ((.) ((.) (flip (&&&) snd)) ((.) (flip (.) fst)))
