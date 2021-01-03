import           Control.Monad

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Right (left + n, right)
  | otherwise                    = failure (left + n) right

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Right (left, right + n)
  | otherwise                    = failure (left + n) right

banana :: Pole -> Either String Pole
banana _ = Left "banana ;)"

failure :: Birds -> Birds -> Either String b
failure left right = Left $
                        "Right - "  `mappend`
                        (show right)`mappend`
                        " Left - "  `mappend`
                        (show left)

x -: f = f x

-- Finding Knight path
type KnightPos = (Int, Int)
type KnightRoute = [KnightPos]
type Knight = (KnightPos, KnightRoute)

moveKnight :: Knight -> [Knight]
moveKnight ((c, r), moves) = do
  (c', r') <- [(c+2, r-1), (c+2, r+1)
              ,(c-2, r-1), (c-2, r+1)
              ,(c+1, r-2), (c+1, r+2)
              ,(c-1, r-2), (c-1, r+2)
              ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return ((c', r'), moves ++ [(c', r')])

in3 :: Knight -> [Knight]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

inMany :: Int -> Knight -> [Knight]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn3 :: KnightPos -> KnightPos -> Maybe KnightRoute
canReachIn3 start end = end `lookup` in3 (start, [])

canReachIn :: Int -> KnightPos -> KnightPos -> Maybe KnightRoute
canReachIn x start end = lookup end $ inMany x (start, [])
