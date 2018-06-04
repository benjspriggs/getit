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
  describe "overlap" $ do
    it "distinct events don't commute" $ 
      property $ \a b -> a /= b ==> 
        overlap (a :: SolvedConstraint Int) (b :: SolvedConstraint Int) /= overlap b a

  describe "solved" $ do
    let randDate d diff = UTCTime (ModifiedJulianDay $ d + diff) (secondsToDiffTime $ d * 10)

    it "validates empty solutions" $ do
      valid (([] :: [SolvedConstraint Int]),[]) `shouldBe` True

    it "validates valid solutions" $ do
      let firstSolved = [solvedConstraint d (randDate d 0) (randDate d 10) | d <- [1..1000]]
      valid (firstSolved, []) `shouldBe` True

    it "invalidates invalid solutions" $ do
      let backwards = [solvedConstraint d (randDate d 0) (randDate d 10) | d <- [1..(-1000)]]
      valid (backwards, []) `shouldBe` False
      
