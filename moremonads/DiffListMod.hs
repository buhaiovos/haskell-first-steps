module DiffListMod
(
DiffList
) where

import           Control.Monad.Writer
import           Data.Monoid
import           Data.Semigroup

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  mappend = (<>)

-- mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x - 1)
  tell (toDiffList [show x])

-- mapM_ putStrLn . snd . runWriter $ finalCountDown 500000
finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x - 1)
  tell [show x]
