m = (->) r
(>>=) :: m a -> (a -> m b) -> m b
h >>= f = \w -> f (h w) w

(r -> a) -> (a -> (r -> b)) -> (r -> b)
|   h    |         f        |
