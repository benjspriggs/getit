module Item where
import Data.DateTime

data TodoItem = Todo DateTime Bool
  deriving (Eq, Show, Ord)

-- returns if this TODO is due by the
-- current time
due :: TodoItem -> IO Bool
due (Todo d _) = do
    ct <- getCurrentTime
    return (d <= ct)

-- returns if this TODO is due at a
-- certain time
dueBy :: TodoItem -> DateTime -> Bool
dueBy (Todo d _) t = d <= t

-- initializes an empty todo
todo :: DateTime -> TodoItem
todo t = Todo t False
