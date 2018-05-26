module Item where
import Data.Time

data TodoItem = Todo { date :: Maybe UTCTime
                     , done :: Bool
                     , name :: String
                     , description :: Maybe String } |
                Event { startDate :: Maybe UTCTime
                     , endDate   :: Maybe UTCTime
                     , done      :: Bool
                     , name      :: String
                     , description :: Maybe String }
  deriving (Eq, Read, Show, Ord)

before :: UTCTime -> UTCTime -> Bool
(before) = (<=)

after :: UTCTime -> UTCTime -> Bool
(after) = (>=)

-- returns if this TODO is due by the
-- current time
due :: TodoItem -> IO (Maybe Bool)
due (Todo d _ _ _) = do
    ct <- getCurrentTime
    return $ fmap (<=ct) d

-- returns if this TODO is due at a
-- certain time
dueBy :: UTCTime -> TodoItem -> Maybe Bool
dueBy t (Todo d _ _ _) = fmap (<=t) d
dueBy t (Event _ endDate _ _ _) = fmap (<=t) endDate

todo :: Day -> TodoItem
todo d = Todo (Just $ UTCTime d $ secondsToDiffTime 0) False "" Nothing

-- returns an identical todo that's done
completeTodo :: TodoItem -> TodoItem
completeTodo (Todo _date _done _name _desc) = 
  Todo _date True _name _desc
completeTodo (Event _startDate _endDate _done _name _desc) = 
  Event _startDate _endDate True _name _desc

prettyWithFormatting :: String -> String -> (Maybe Bool, TodoItem) -> String
prettyWithFormatting ding nope (maybeDone, todoItem) = case maybeDone of
    Nothing     -> "\t" ++ (show todoItem)
    Just isDone -> (if isDone then ding else nope) ++ "\t" ++ (show todoItem)

pretty = prettyWithFormatting "✔" "✗"

remaining :: UTCTime -> TodoItem -> Maybe NominalDiffTime
remaining t (Todo d _ _ _) = (flip diffUTCTime) t <$> d
remaining t (Event _ d _ _ _) = (flip diffUTCTime) t <$> d
