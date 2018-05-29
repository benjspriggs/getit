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

patterns :: Docopt
patterns = [docoptFile|src/USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  -- get the user's tasks and events
  fn <- getArgOrExit args (longOption "it")
  -- the date format string for everything passed
  fmt <- getArgOrExit args (longOption "format")

  let parseWithFormatString = parseTimeOrError True defaultTimeLocale fmt
  let couldBeADate = couldBeDateFromArgs args fmt parseWithFormatString

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
    putStrLn $ pretty "due" (dueBy ct) tasks

  when (args `isPresent` (command "done")) $ do
    name <- getArgOrExit args (argument "name")

    withGetitFile fn $ finishTodo name

  when (args `isPresent` (command "soon")) $ do
    ct <- getCurrentTime
    parsedWaterMark <- couldBeADate "watermark"
    let waterMark = fromMaybe (addUTCTime nominalDay ct) parsedWaterMark

    tasks <- getTasks fn
    let soonTasks = filter (\t -> 0 >= (fromMaybe 0 $ remaining waterMark t)) tasks

    putStrLn $ pretty "done" (\t -> Just $ done t) soonTasks

  when (args `isPresent` (command "clean")) $ do
    withGetitFile fn $ removeFinishedTodos
