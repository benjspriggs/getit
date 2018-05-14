import Data.DateTime

main = do
	ct <- getCurrentTime
	putStrLn (show ct)
	putStrLn "Hello world!"
