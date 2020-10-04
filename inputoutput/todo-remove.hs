import System.IO
import System.Directory
import Data.List

main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "Current tasks:"
  putStr $ unlines numberedTasks
  putStrLn "Which one should be deleted?"
  taskIndexStr <- getLine
  let index = read taskIndexStr
      newTodoItems = delete (todoTasks !! index) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
