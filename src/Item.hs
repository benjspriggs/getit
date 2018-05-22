module Item where
import Data.DateTime

data TodoItem = Todo { date :: Maybe DateTime
                     , done :: Bool
                     , name :: String
                     , description :: String } |
                Event { startDate :: Maybe DateTime
                     , endDate   :: Maybe DateTime
                     , done      :: Bool
                     , name      :: String
                     , description :: String }
  deriving (Eq, Read, Show, Ord)

-- returns if this TODO is due by the
-- current time
due :: TodoItem -> IO (Maybe Bool)
due (Todo d _ _ _) = do
    ct <- getCurrentTime
    return $ fmap (<=ct) d

-- returns if this TODO is due at a
-- certain time
dueBy :: DateTime -> TodoItem -> Maybe Bool
dueBy t (Todo d _ _ _) = fmap (<=t) d

todo :: DateTime -> TodoItem
todo d = Todo (Just d) False "" ""

completeTodo :: TodoItem -> TodoItem
completeTodo (Todo _date _done _name _desc) = 
  Todo _date True _name _desc
completeTodo (Event _startDate _endDate _done _name _desc) = 
  Event _startDate _endDate True _name _desc
