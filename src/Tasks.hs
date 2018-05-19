module Tasks where
import Data.DateTime
import Data.Tuple.HT
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
store = error "todo"

-- retrieves tasks from file
retrieve :: String -> Maybe (IO Tasks)
retrieve = error "todo"
