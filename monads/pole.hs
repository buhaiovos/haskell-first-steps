import Control.Monad

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

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

canReachIn3 :: KnightPos -> KnightPos -> Maybe KnightRoute
canReachIn3 start end = end `lookup` in3 (start, [])
