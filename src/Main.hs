{-# LANGUAGE QuasiQuotes #-}
import Data.DateTime
import Control.Monad
import System.Environment
import System.Console.Docopt
import Item
import Tasks

patterns :: Docopt
patterns = [docoptFile|src/USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  fn <- getArgOrExit args (longOption "it")
  when (args `isPresent` (command "store")) $ do
    putStrLn "store"
    store fn sample
    
    putStrLn ("Saved to " ++ fn)

  when (args `isPresent` (command "new")) $ do
    putStrLn "new"
  when (args `isPresent` (command "list")) $ do
    putStrLn "list"
    
    tasks <- getTasks fn
    putStrLn (show tasks)
  when (args `isPresent` (command "done")) $ do
    putStrLn "done"
