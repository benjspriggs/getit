module Item where
import Data.Time
import Data.List(intercalate)

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

data Formatter = TodoFormat { heading :: String
                 , ding :: String
                 , nope :: String
                 , isIt :: TodoItem -> Maybe Bool }

conditionalColumns :: [Formatter] -> [TodoItem] -> String
conditionalColumns fmts ts = (separated "\t" headers) ++ "item\n" ++ intercalate "\n" rows
  where separated ch xs = (intercalate ch xs) ++ ch
        headers = map heading fmts
        rows = map columnFor ts
        columnFor t = separated "\t" $ map (formatColumn t) fmts
        formatColumn t fmt = pre ++ "\t" ++ show t
            where pre = case (isIt fmt t) of
                          Nothing -> ""
                          Just success -> if success then ding fmt else nope fmt

prettyWithFormatting :: String -> String -> (Maybe Bool, TodoItem) -> String
prettyWithFormatting ding nope (maybeSuccess, todoItem) = case maybeSuccess of
    Nothing     -> "\t" ++ (show todoItem)
    Just isSuccess -> (if isSuccess then ding else nope) ++ "\t" ++ (show todoItem)

pretty :: String -> (TodoItem -> Maybe Bool) -> [TodoItem] -> String
pretty name f = conditionalColumns [TodoFormat name "✔" "✗" f]

remaining :: UTCTime -> TodoItem -> Maybe NominalDiffTime
remaining t (Todo d _ _ _) = (flip diffUTCTime) t <$> d
remaining t (Event _ d _ _ _) = (flip diffUTCTime) t <$> d

overlap :: TodoItem -> TodoItem -> Bool
overlap (Event a b _ _ _) (Event c d _ _ _)  = not $ a >= d && b >= c
overlap _ _ = False
