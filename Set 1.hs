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

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = 
    genTwo ga (\aa -> 
            genTwo gb (\bb ->
                        mkGen $ f aa bb))

generalPair2' :: Gen a -> Gen b -> Gen (a, b)
generalPair2' = generalB2 (,)

randPair'' = generalPair2' randLetter rand

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 [] = mkGen []
repRandom2 (g:gs) = genTwo g (\g' ->
    genTwo (repRandom2 gs) (\gs' ->
      mkGen (g':gs')))

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
