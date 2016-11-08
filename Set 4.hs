import MCPrelude

generalB, yLink :: (a -> b -> c) -> m a -> m b -> m c
genTwo, link :: m a -> (a -> m b) -> m b
