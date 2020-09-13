lucky :: (Integral a) => a -> String
lucky 7 = "You're a lucky one!"
lucky x = "Better luck next time!"

patternFactorial :: (Integral a) => a -> a
patternFactorial 0 = 1
patternFactorial n = n * patternFactorial (n - 1)

-- pattern matching

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Empty list is illegal"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:aTail) = 1 + length' aTail

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- patterns

capital :: String -> String
capital "" = "Empty String!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

-- where

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where
      bmi = weight / height ^ 2

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where
      bmi = weight / height ^ 2
      (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where
      (f:_) = firstName
      (l:_) = lastName

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis weightHeights = [bmi weight height | (weight, height) <- weightHeights]
    where bmi weight height = weight / height ^ 2
-- calcBmis weightHeights = [ weight / height ^ 2 | (weight, height) <- weightHeights]

-- let

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let
    sideArea = 2 * pi * r * h
    topArea = pi * r ^ 2
  in sideArea + 2 * topArea


-- case

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
  [] -> "Empty"
  [_] -> "Singleton list"
  otherwise -> "List of size 2+"

-- same stuff but with where
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where
    what [] = "empty"
    what [_] = "a singleton list"
    what xs = "of size 2+"
