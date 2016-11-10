import Prelude()
import MCPrelude
import Set2

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
