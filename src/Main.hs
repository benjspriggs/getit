{-# LANGUAGE QuasiQuotes #-}
import Data.DateTime
import Data.Time.Format
import Control.Monad
import System.Environment
import System.Console.Docopt
import System.Exit
import Item
import Tasks
import Menu

patterns :: Docopt
patterns = [docoptFile|src/USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

storeCommand args = do
  fn <- getArgOrExit args (longOption "it")
  putStrLn "store"
  store fn sample
  
  putStrLn ("Saved to " ++ fn)

newCommand args = do
  putStrLn "new task"
  fn <- getArgOrExit args (longOption "it")

  name <- getArgOrExit args (argument "name")
  taskDueBy <- getArgOrExit args (argument "due-by")
  fmt <- getArgOrExit args (longOption "format")
  putStrLn $ "Formatting '" ++ taskDueBy ++ "' according to " ++ fmt

  let desc = getArgWithDefault args "" (argument "description")
  let parsedDueBy = parseTimeOrError True defaultTimeLocale fmt taskDueBy


  tasks <- getTasks fn
  let newTasks = tasks ++ [Todo parsedDueBy False name desc]
  store fn $! newTasks

  return ()

listCommand args = do
  fn <- getArgOrExit args (longOption "it")
  putStrLn "list"
  
  tasks <- getTasks fn
  putStrLn $ unlines $ map show tasks

doneCommand args = do
  putStrLn "done"
  fn <- getArgOrExit args (longOption "it")
  name <- getArgOrExit args (argument "name")

  tasks <- getTasks fn
  let newTasks = finish name tasks

  store fn $! newTasks

main :: IO()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "store")) $ storeCommand args
  when (args `isPresent` (command "new")) $ newCommand args
  when (args `isPresent` (command "list")) $ listCommand args
  when (args `isPresent` (command "done")) $ doneCommand args
  when (args `isPresent` (command "menu")) $ void $ getitMenuAction
