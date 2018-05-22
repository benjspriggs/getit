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

_couldBeADate args fmt _parse option = do
  let mightBeDate = getArg args (argument option)
  putStrLn $ "Formatting '" ++ (show mightBeDate) ++ "' according to " ++ fmt

  let parsedDate = fmap _parse mightBeDate
  return parsedDate

main :: IO()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  -- get the user's tasks and events
  fn <- getArgOrExit args (longOption "it")
  -- the date format string for everything passed
  fmt <- getArgOrExit args (longOption "format")

  let parseWithFormatString = parseTimeOrError True defaultTimeLocale fmt
  let couldBeADate = _couldBeADate args fmt parseWithFormatString

  -- store some sample tasks and events
  when (args `isPresent` (command "store")) $ do
    withGetitFile fn $ do
      return sample

  -- create a new thing
  when (args `isPresent` (command "new")) $ do
    name <- getArgOrExit args (argument "name")
    let desc = getArg args (argument "description")

    -- new task
    when (args `isPresent` (command "task")) $ do
      parsedDueBy <- couldBeADate "due-by"

      withGetitFile fn $ addTodo $ Todo parsedDueBy False name desc

    -- new event
    when (args `isPresent` (command "event")) $ do
      parsedStart <- couldBeADate "start"
      parsedEnd <- couldBeADate "end"

      withGetitFile fn $ addTodo $ Event parsedStart parsedEnd False name desc

  when (args `isPresent` (command "list")) $ do
    tasks <- getTasks fn
    putStr $ unlines $ map show tasks

  when (args `isPresent` (command "done")) $ do
    name <- getArgOrExit args (argument "name")

    withGetitFile fn $ finishTodo name
