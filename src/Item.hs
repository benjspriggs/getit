module Item where
import Data.DateTime

data TodoItem = Todo DateTime

due :: TodoItem -> IO Bool
due (Todo d) = do
                ct <- getCurrentTime
                return (d <= ct)
