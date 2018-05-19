module Tasks where
import Data.Maybe
import Data.DateTime
import Data.Tuple.HT
import Text.Read(readMaybe)
import Item

type Tasks = [TodoItem]

-- gets the current status of
-- a number of todo items
status :: Tasks -> IO [Bool]
status ts = do
    ct <- getCurrentTime
    return $ map (\t -> dueBy t ct) ts

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
  content <- readFile path 
  let tasks = lines content
  let readTasks = map readMaybe tasks :: [Maybe TodoItem]
  return (sequence readTasks)

getTasks :: String -> IO Tasks
getTasks path = do
  maybeTasks <- retrieve path
  let tasks = fromMaybe [] maybeTasks
  return tasks

