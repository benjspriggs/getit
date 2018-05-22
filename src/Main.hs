{-# LANGUAGE QuasiQuotes #-}
import Data.DateTime
import Data.Time.Format
import Control.Monad
import Control.Monad.State
import System.Environment
import System.Console.Docopt
import System.Exit
import Item
import Tasks

patterns :: Docopt
patterns = [docoptFile|src/USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

withGetitFile :: String -> State Tasks Tasks -> IO ()
withGetitFile fn action = do
  putStrLn $ "Retrieving from " ++ fn
  tasks <- getTasks fn

  let onTasks  = runState action tasks
  let newTasks = fst onTasks

  store fn $! newTasks
  putStrLn $ "Saved to " ++ fn

  return ()

main :: IO()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  fn <- getArgOrExit args (longOption "it")

  when (args `isPresent` (command "store")) $ do
    putStrLn "store"
    withGetitFile fn $ do
      return sample

  when (args `isPresent` (command "new")) $ do
    putStrLn "new task"

    name <- getArgOrExit args (argument "name")
    taskDueBy <- getArgOrExit args (argument "due-by")
    fmt <- getArgOrExit args (longOption "format")
    putStrLn $ "Formatting '" ++ taskDueBy ++ "' according to " ++ fmt

    let desc = getArgWithDefault args "" (argument "description")
    let parsedDueBy = parseTimeOrError True defaultTimeLocale fmt taskDueBy

    withGetitFile fn $ addTodo $ Todo (Just parsedDueBy) False name desc

  when (args `isPresent` (command "list")) $ do
    putStrLn "list"
    tasks <- getTasks fn
    putStrLn $ unlines $ map show tasks

  when (args `isPresent` (command "done")) $ do
    putStrLn "done"
    name <- getArgOrExit args (argument "name")

    withGetitFile fn $ finishTodo name
