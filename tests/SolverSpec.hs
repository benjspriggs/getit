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
    it "distinct events commute" $ 
      property $ \a b -> a /= b ==> 
        overlap (a :: SolvedConstraint Int) (b :: SolvedConstraint Int) == overlap b a

    it "nondistinct events commute" $ 
      property $ \a -> 
        overlap (a :: SolvedConstraint Int) a == overlap a a
    

  describe "valid" $ do
    let randDate d diff = UTCTime (ModifiedJulianDay $ d + diff) (secondsToDiffTime $ d * 10)
    let zero = UTCTime (ModifiedJulianDay 0) 0

    it "accepts empty solutions" $ do
      valid (([] :: [SolvedConstraint Int]),[]) `shouldBe` True

    it "accepts valid solutions" $ do
      let firstSolved = [solvedConstraint d (randDate d 0) (randDate d 10) | d <- [1..1000]]
      valid (firstSolved, []) `shouldBe` True

    it "rejects invalid solutions" $ do
      let backwards = [solvedConstraint d zero zero | d <- [1..1000]]
      valid (backwards, []) `shouldBe` False

      let pickedStart = randDate 0 100
      let pickedEnd = randDate 0 1000
      let allOverlapping = replicate 1000 $ solvedConstraint 0 pickedStart pickedEnd
      valid (allOverlapping, []) `shouldBe` False
