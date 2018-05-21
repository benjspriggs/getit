module Menu where
import Data.Text(Text, pack)
import System.Console.Byline


-- interactive menu to add, remove, etc
-- existing TODOs
-- getitMenu :: IO ()
getitMenu :: Menu Text
getitMenu = menu choices stylize
  where choices = map pack ["a", "b", "c"] :: [Text]
        stylize :: Text -> Stylized
        stylize t = text t

getitMenuAction :: IO (Maybe (Choice Text))
getitMenuAction = do
  choice <- runByline $ askWithMenu getitMenu (text (pack "getit"))
  return choice
