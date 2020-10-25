data Section = Section {
  getA::Int,
  getB::Int,
  getC::Int
} deriving (Show)

data Label = A | B | C deriving (Show)

type RoadSystem = [Section]

type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [
                    Section 50 10 30,
                    Section 5 90 20,
                    Section 40 2 25,
                    Section 10 8 0
                  ]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPathToA = priceA + a
      crossPathToA = priceB + b + c
      forwardPathToB = priceB + b
      crossPathToB = priceA + a + c
      newPathToA = if forwardPathToA <= crossPathToA
                      then (A, a):pathA
                      else (C, c):(B, b):pathB
      newPathToB = if forwardPathToB <= crossPathToB
                      then (B, b):pathB
                      else (C, c):(A, a):pathA
  in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath rs =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) rs
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
         then reverse bestAPath
         else reverse bestBPath


groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main :: IO ()
main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice
