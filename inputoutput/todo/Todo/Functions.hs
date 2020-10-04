module Todo.Functions
(
  dispatch
)
where

import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [
            ("add", add)
           ,("view", view)
           ,("remove", remove)
           ]

makeLine :: String -> String
makeLine = (++"\n")

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName $ makeLine todoItem

view :: [String] -> IO ()
view [fileName] = do
  content <- readFile fileName
  let tasks = lines content
      tasksWithNumbers = zipWith (\n task -> show n ++ " - " ++ task) [0..] tasks
  putStr $ unlines tasksWithNumbers

remove :: [String] -> IO ()
remove [fileName, indexStr] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let index = read indexStr
      todos = lines contents
      newTodos = delete (todos !! index) todos
  hPutStr tempHandle $ unlines newTodos
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
