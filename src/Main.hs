{-# LANGUAGE QuasiQuotes #-}
import Data.Time
import Data.Time.Format
import Data.Time.Clock
import Data.Maybe
import Control.Monad
import Control.Monad.State
import System.Environment
import System.Console.Docopt
import System.IO
import System.Exit
import Item
import Tasks
import Menu

patterns :: Docopt
patterns = [docoptFile|src/USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

withGetitFile :: String -> State Tasks Tasks -> IO ()
withGetitFile fn action = do
  putStrLn $ "Retrieving tasks from " ++ fn
  tasks <- getTasks fn

  -- notify of tasks due now
  ct <- getCurrentTime
  let dueSoon = filter (fromMaybe False . dueBy ct) tasks

  when (length dueSoon > 0) $ do
    putStrLn "The following tasks are due NOW!"
    putStr $ unlines $ map show dueSoon

  let onTasks  = runState action tasks
  let newTasks = fst onTasks
  let changed = tasks /= newTasks
  let save = do
          store fn $! newTasks
          putStrLn $ "Saved tasks to " ++ fn

  when changed $ do
    let added = length tasks < length newTasks
    when added $ do
      when (any (\t -> any (overlap t) newTasks) newTasks) $ do
        putStr "Overlaping todos found, continue saving? (y/n/q) "
        hFlush stdout
        confirm <- getChar
        when (confirm == 'y') $ do
          save
    when (not added) $ do
      save

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

      let newTodo = Todo parsedDueBy False name desc

      withGetitFile fn $ addTodo newTodo

    -- new event
    when (args `isPresent` (command "event")) $ do
      parsedStart <- couldBeADate "start"
      parsedEnd <- couldBeADate "end"

      let newDate = Event parsedStart parsedEnd False name desc

      withGetitFile fn $ addTodo newDate

  when (args `isPresent` (command "list")) $ do
    tasks <- getTasks fn
    ct <- getCurrentTime
    let dueSoon = filter (fromMaybe False . dueBy ct) tasks

    when (length dueSoon > 0) $ do
      putStrLn "The following things are due by RIGHT NOW!"
      putStr $ unlines $ map show dueSoon

    putStrLn $ pretty "due" (dueBy ct) tasks

  when (args `isPresent` (command "done")) $ do
    name <- getArgOrExit args (argument "name")

    withGetitFile fn $ finishTodo name

  when (args `isPresent` (command "menu")) $ void $ getitMenuAction

  when (args `isPresent` (command "soon")) $ do
    ct <- getCurrentTime
    parsedWaterMark <- couldBeADate "watermark"
    let waterMark = fromMaybe (addUTCTime nominalDay ct) parsedWaterMark

    tasks <- getTasks fn
    let soonTasks = filter (\t -> 0 >= (fromMaybe 0 $ remaining waterMark t)) tasks

    putStrLn $ pretty "done" (\t -> Just $ done t) soonTasks

  when (args `isPresent` (command "clean")) $ do
    withGetitFile fn $ removeFinishedTodos
