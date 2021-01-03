import           Control.Monad
import           Data.List

solveRPN :: String -> Maybe Float
solveRPN input = do
  [result] <- foldM foldingFunction [] (words input)
  return result

foldingFunction :: [Float] -> String -> Maybe [Float]
foldingFunction (x:y:ys) "*"       = return $ (x * y):ys
foldingFunction (x:y:ys) "+"       = return $ (x + y):ys
foldingFunction (x:y:ys) "-"       = return $ (y - x):ys
foldingFunction (x:y:ys) "/"       = return $ (y / x):ys
foldingFunction (x:y:ys) "^"       = return $ (y ** x):ys
foldingFunction (x:xs)   "ln"      = return $ (log x):xs
foldingFunction xs       "sum"     = return $ [sum xs]
foldingFunction xs       numberStr = liftM (:xs) $ readMaybe numberStr

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _        -> Nothing
