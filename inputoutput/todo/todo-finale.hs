import qualified Todo.Functions as TdF
import System.Environment

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command TdF.dispatch
  action args
