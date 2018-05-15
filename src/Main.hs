{-# LANGUAGE QuasiQuotes #-}
import Data.DateTime
import Control.Monad
import System.Environment
import System.Console.Docopt
import Item

patterns :: Docopt
patterns = [docoptFile|src/USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  when (args `isPresent` (command "new")) $ do
    putStrLn "new"
  when (args `isPresent` (command "list")) $ do
    putStrLn "list"
  when (args `isPresent` (command "done")) $ do
    putStrLn "done"
