doubleUs x y = doubleMe x + doubleMe y  
doubleMe x = x + x

doubleSmallNubmer x = if x > 100
                        then x
                        else x * 2

factorial n = if n == 0
                then 1
                else n * factorial (n-1)

sumOfList list = if null list
                   then 0
                   else head list + sumOfList (tail list)

getAllElementsDivisibleBy :: [Int] -> Int -> [Int]
getAllElementsDivisibleBy list divisor = 
    if null list
        then []
        else if head list `mod` divisor == 0
            then head list : getAllElementsDivisibleBy (tail list) divisor
            else getAllElementsDivisibleBy (tail list) divisor

boomBang xs = [ if x < 10 then "BOOM" else "BANG" | x <- xs, odd x]

length' xs = sum [ 1 | _ <- xs ]