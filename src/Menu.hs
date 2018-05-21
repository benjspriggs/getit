{-# LANGUAGE OverloadedStrings #-}
module Menu where
import Data.Text(Text, pack)
import System.Console.Byline


-- interactive menu to add, remove, etc
-- existing TODOs
-- getitMenu :: IO ()
getitMenu :: Menu Text
getitMenu = menu choices text
  where choices = map pack ["a", "b", "c"]

getitMenuAction :: IO (Maybe (Choice Text))
getitMenuAction = do
  let prompt = "Which action? "

  choice <- runByline $ askWithMenu getitMenu prompt
  return choice
