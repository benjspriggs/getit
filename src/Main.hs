import Data.DateTime
import Item

main = do
	ct <- return (fromGregorian' 2019 2 3)
	isDue <- due (Todo ct)
	putStrLn (show ct)
	putStrLn (show isDue)
	putStrLn "Hello world!"
