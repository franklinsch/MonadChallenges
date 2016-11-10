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
