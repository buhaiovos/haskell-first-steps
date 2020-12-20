-- Functor f
fmap :: (a -> b) -> f a -> f b

-- Functor f => Applicative f
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- Semigroup a
(<>) :: a -> a -> a

-- Semigroup a => Monoid a
mempty :: a
mappend :: a -> a -> a
mconcat :: [a] -> a

-- Applicative m => Monad m
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
(>>) :: m a -> m b -> m b
