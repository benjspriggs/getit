module Item where
import Data.DateTime

data TodoItem = Todo { date :: DateTime
                     , done :: Bool
                     , description :: String }
  deriving (Eq, Show, Ord)

-- returns if this TODO is due by the
-- current time
due :: TodoItem -> IO Bool
due (Todo d _ _) = do
    ct <- getCurrentTime
    return (d <= ct)

-- returns if this TODO is due at a
-- certain time
dueBy :: TodoItem -> DateTime -> Bool
dueBy (Todo d _ _) t = d <= t

todo :: DateTime -> TodoItem
todo d = Todo d False ""
