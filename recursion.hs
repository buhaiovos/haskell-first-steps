quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (aHead:aTail) =
  let
    left = quicksort [ element | element <- aTail, element <= aHead ]
    right = quicksort [ element | element <- aTail, element > aHead ]
  in left ++ [aHead] ++ right

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Вов, полегше!"
maximum' [single] = single
maximum' (first:others) = max first (maximum' others)

replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' quantity element
  | quantity <= 0 = []
  | otherwise = element:(replicate' (quantity - 1) element)

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)
