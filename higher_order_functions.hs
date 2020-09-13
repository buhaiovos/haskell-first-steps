multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs


quicksortV2 :: (Ord a) => [a] -> [a]
quicksortV2 [] = []
quicksortV2 (x:xs) =
  let smallerSorted = quicksortV2 (filter (<=x) xs)
      biggerSorted = quicksortV2 (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
  where p x = (x `mod` 3829) == 0


-- Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain(n `div` 2)
  | odd n  = n:chain(n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- lambdas
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x


flip''' :: (a -> b -> c) -> (b -> a -> c)
flip''' f = g
    where g x y = f y x
