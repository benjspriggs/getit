module SolverSpec(spec) where
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Time
import Solver

instance Arbitrary NominalDiffTime where
  arbitrary = do
    v <- arbitrary `suchThat` (>=0) -- we only want times in the future
    return $ realToFrac (v :: Integer)

instance Arbitrary UTCTime where
  arbitrary = do
    randomDay <- choose (0, 1000)
    randomSeconds <- choose (0, 500)
    let timeFromMidnight = secondsToDiffTime randomSeconds
    let day = ModifiedJulianDay randomDay
    return $ UTCTime day timeFromMidnight

instance Arbitrary a => Arbitrary (SolvedConstraint a) where
  arbitrary = do
    val <- arbitrary
    start <- arbitrary
    endDiff <- arbitrary
    return $ solvedConstraint val start (addUTCTime endDiff start)

spec :: Spec
spec = do
  let randDate d diff = UTCTime (ModifiedJulianDay $ d + diff) (secondsToDiffTime $ d * 10)

  describe "overlap" $ do
    it "distinct events commute" $ 
      property $ \a b -> a /= b ==> 
        overlap (a :: SolvedConstraint Int) (b :: SolvedConstraint Int) == overlap b a

    it "nondistinct events commute" $ 
      property $ \a -> 
        overlap (a :: SolvedConstraint Int) a == overlap a a

    it "says the same event overlaps" $ do
      let pickedStart = randDate 0 0
      let pickedEnd = randDate 10 100
      let randSolvedDate = solvedConstraint 0 pickedStart pickedEnd
      overlap randSolvedDate randSolvedDate `shouldBe` True

  describe "valid" $ do
    let zero = UTCTime (ModifiedJulianDay 0) 0

    it "accepts empty solutions" $ do
      valid (([] :: [SolvedConstraint Int]),[]) `shouldBe` True

    it "accepts valid solutions" $ do
      let firstSolved = [solvedConstraint d (randDate d 0) (randDate d 10) | d <- [1..1000]]
      valid (firstSolved, []) `shouldBe` True

    it "rejects solved constraints that are all the same" $ do
      let allSame = [solvedConstraint d zero zero | d <- [1..1000]]
      valid (allSame, []) `shouldBe` False

    it "rejects solved constraints that are all overlapping" $ do
      let pickedStart = randDate 0 0
      let pickedEnd = randDate 10 100
      let allOverlapping = replicate 1000 $ solvedConstraint 0 pickedStart pickedEnd
      valid (allOverlapping, []) `shouldBe` False

  describe "solveSchedule" $ do
    let pickedStart = randDate 0 0
    let pickedEnd = randDate 100 0

    it "all un-assigned dates give solutions with all open constraints" $ do
      let nothing = Open 0 Nothing Nothing
      let nothings = replicate 5 nothing
      let response = solveSchedule pickedStart pickedEnd nothings
      putStrLn $ show response
      length response `shouldBe` 1
      
