data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int
} deriving (Show, Eq)

jane :: Person
jane = Person "Jane" "Doe" 23

john :: Person
john = Person "John" "Doe" 25

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type AssocList k v = [(k, v)]

findAssoc :: (Eq k) => k -> AssocList k v -> Maybe v
findAssoc key = foldl (\acc (k, v) -> if k == key then Just v else acc) Nothing


-- recursive data structures

infixr 5 :-:
data List a = Empty | a :-: (List a)
  deriving (Show, Read, Ord, Eq)
