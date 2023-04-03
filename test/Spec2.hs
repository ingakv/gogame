import Test.QuickCheck(property)
import Test.Hspec(Spec, describe, shouldBe, it, hspec)

import Lib (distanceTuple, sqrt', makeTuple)
import Logic (replace)


main :: IO ()
main = do
    hspec testFunc

testFunc :: Spec
testFunc = do
    describe "Testing Lib module" $ do
        describe "distanceTuple" $ do
            it "works for (1, 1) (1, 2)" $ do
                distanceTuple (1,1) (1,2) `shouldBe` (1, 1, 2)
            it "Works for (1, 2) (1, 1)" $ do
                distanceTuple (1,2) (1,1) `shouldBe` (1, 1, 1)
        describe "sqrt'" $ do
            it "works for 5" $ do
                sqrt' 5 `shouldBe` 2
            it "works for 12" $ do
                sqrt' 12 `shouldBe` 3
        describe "makeTuple" $ do
            it "works for 3 (10, 10) (0, 0)" $ do
                makeTuple 3 (10, 10) (0, 0) `shouldBe` (9, 9)
    
    describe "Testing Logic module" $ do
        describe "replace" $ do
            it "works for (1) (5) [1,2,3]" $ do
                replace 1 5 [1,2,3] `shouldBe` [1,5,3]
            it "works for 0 (5) [1,2,3]" $ do
                replace 0 5 [1,2,3] `shouldBe` [5,2,3] 
