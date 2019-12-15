module MPMatrixSpec where

import Test.Hspec
import MPMatrix

main :: IO ()
main = hspec $ do
  describe "Matrix Functions" $ do
    it "returns the scalar multiple of a given Matrix" $
      scalarMult 2 (Matrix [Row [1,1], Row [1,1]]) `shouldBe` (Matrix [Row [2,2], Row [2,2]])

    it "returns the addition of two Matricies" $
      mAdd (Matrix [Row [1,1], Row [1,1]]) (Matrix [Row [1,1], Row [1,1]]) `shouldBe` (Matrix [Row [2,2], Row [2,2]])

    it "returns the subtraction of two Matricies" $
      mSub (Matrix [Row [1,1], Row [1,1]]) (Matrix [Row [1,1], Row [1,1]]) `shouldBe` (Matrix [Row [0,0], Row [0,0]])