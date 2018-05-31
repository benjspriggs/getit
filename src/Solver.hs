module Solver where
import Data.List(permutations, concat, sortBy)
import Data.Time(UTCTime)
import Data.Maybe(Maybe, fromMaybe)
import Item(TodoItem)

data OpenConstraint a = Open a (Maybe UTCTime) (Maybe UTCTime)
data SolvedConstraint a = Solved a UTCTime UTCTime deriving (Eq)
type Solution a = ([SolvedConstraint a], [OpenConstraint a])

overlap :: SolvedConstraint a -> SolvedConstraint a -> Bool
overlap (Solved _ a b) (Solved _ c d)  = not $ a >= d && b >= c

valid :: Eq a => Solution a -> Bool
valid (solved, _) = (not . and) $ map (\(a, b) -> overlap a b) [(a,b) | a <- solved, b <- solved, a /= b]

solveSchedule :: Eq a => UTCTime -> UTCTime -> [OpenConstraint a] -> [Solution a]
solveSchedule start end constraints = [ solution | solution <- possibleSolutions start end constraints, valid solution ]

subsets       :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- Sorts OpenConstraint.
sortPredicate :: OpenConstraint a -> OpenConstraint a -> Ordering
sortPredicate (Open _ mStartA _) (Open _ mStartB _) = case (mStartA, mStartB) of
  (Nothing, Nothing) -> EQ
  (Nothing, _) -> LT
  (_, Nothing) -> GT
  (Just a, Just b) -> compare a b

-- Gives a time to an open constraint.
-- Defaults to the first two provided dates.
giveTime :: UTCTime -> UTCTime -> OpenConstraint a -> SolvedConstraint a
giveTime s e (Open item mStart mEnd) = Solved item newStart newEnd
  where newStart = fromMaybe s mStart
        newEnd = fromMaybe e mEnd

allPossibleSplits :: [OpenConstraint a] -> [([OpenConstraint a], [OpenConstraint a])]
allPossibleSplits cs = [splitAt n cs | n <- [0..(length cs)]]

asSolution :: UTCTime -> UTCTime -> ([OpenConstraint a], [OpenConstraint a]) -> Solution a
asSolution s e (toSolve, toOpen) = (map (giveTime s e) toSolve, toOpen)

-- Given a start and ending window, and a list of 
-- OpenConstraint, return a list of solution sets
-- that may or may not be valid.
possibleSolutions :: UTCTime -> UTCTime -> [OpenConstraint a] -> [Solution a]
possibleSolutions start end constraints = [ asSolution start end possible | possible <- allPossibleSplits $ concat $ permutations constraints ]
  where sortedConstraints = sortBy sortPredicate constraints
