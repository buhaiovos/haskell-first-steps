removeNonUppercase :: [Char] -> [Char]
removeNonUppercase xs = [ x | x <- xs, x `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree one two three = one + two + three

circumference :: Double -> Double
circumference r = 2 * pi * r
