import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn . message . show . length . lines $ contents

message :: String -> String
message body = "The file has " ++ body ++ " lines."

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just path -> putStrLn $ "The file " ++ path ++ " does not exist!"
      Nothing -> putStrLn "The file does not exist so much that I even don't know its name!"
  | otherwise = ioError e
