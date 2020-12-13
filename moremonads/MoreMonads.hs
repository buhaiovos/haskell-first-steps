module MoreMonads
(
mapplyLog
) where

isBigGang :: Int -> Bool
isBigGang x = x > 9

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "Comparing to gang of 9")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog' (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

mapplyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
mapplyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)
