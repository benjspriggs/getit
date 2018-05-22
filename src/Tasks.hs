{-# LANGUAGE ScopedTypeVariables #-}
module Tasks where
import Prelude hiding (readFile)
import Control.Exception(handle, IOException)
import Control.Monad.State
import Data.Maybe
import Data.DateTime
import Data.Tuple.HT
import Text.Read(readMaybe)
import System.IO.Strict(readFile)
import Item

type Tasks = [TodoItem]

-- gets the current status of
-- a number of todo items
status :: Tasks -> IO [(TodoItem, Bool)]
status ts = do
    ct <- getCurrentTime
    return $ zip ts (catMaybes $ map (dueBy ct) ts)

-- sample task list
sample :: Tasks
sample = map todo $ map (uncurry3 fromGregorian') [(2017, 2, 3), (2018, 3,2), (2019, 3,2)]

-- stores tasks in file
store :: String -> Tasks -> IO ()
store path tasks = writeFile path serialized
  where serialized = unlines $ map show tasks

-- retrieves tasks from file
retrieve :: String -> IO (Maybe Tasks)
retrieve path = do
  handle (\(e :: IOException) -> return Nothing) $ do
    content <- readFile path 
    let tasks = lines content
    let readTasks = map readMaybe tasks :: [Maybe TodoItem] 
    return (sequence readTasks)

-- retrieves tasks from file, returning
-- the empty list if that file doesn't exist
-- or something else happened along the way
getTasks :: String -> IO Tasks
getTasks path = do
  maybeTasks <- retrieve path
  return $ fromMaybe [] maybeTasks

finish :: String -> Tasks -> Tasks
finish finished [] = []
finish finished (t:ts) = if name t == finished then (completeTodo t):ts else (t:ts)

addTodo :: TodoItem -> State Tasks Tasks
addTodo item = do
  ts <- get
  return (item:ts)

finishTodo :: String -> State Tasks Tasks
finishTodo n = do
  ts <- get
  return $ finish n ts

removeFinishedTodos :: State Tasks Tasks
removeFinishedTodos = do
  ts <- get
  return $ filter (not . done) ts

within :: DateTime -> (Maybe DateTime, Maybe DateTime) -> Bool
within t range = case range of
  (Nothing, Nothing) -> False
  (Just _startDate, Just _endDate) -> t `after` _startDate && t `before` _endDate
  (Just _startDate, Nothing) ->  t `after`_startDate
  (Nothing, Just _endDate) -> t `before` _endDate
