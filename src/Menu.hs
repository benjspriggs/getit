module Menu where
import Data.Text(Text, unpack)
import System.Console.Byline


-- interactive menu to add, remove, etc
-- existing TODOs
-- getitMenu :: IO ()
getitMenu :: Menu Text
getitMenu = menu choices stylize
  where choices = map unpack ["a", "b", "c"] :: [Text]
        stylize :: Text -> Stylized
        stylize t = text (pack t) <> (" (choice)" <> fg blue)

getitMenuAction :: IO Text
getitMenuAction = do
  return $ runByline $ askWithMenu getitMenu "getit"
