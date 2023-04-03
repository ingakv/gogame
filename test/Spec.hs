
import Test.QuickCheck(property)
import Test.Hspec(Spec, describe, shouldBe, it, hspec)

import GameLogic (isEmpty, replace, insertEveryN, insertAt)
import DataTypes (Slot(Empty))


main :: IO ()
main = do
    hspec testFunc

testFunc :: Spec
testFunc = do
    describe "Testing GameLogic module" $ do
        describe "isEmpty" $ do
            it "works for if it is Empty" $ do
                isEmpty Empty `shouldBe` True
        describe "replace" $ do
            it "works for 2 5 [1,2,3]" $ do
                replace 2 5 [1,2,3] `shouldBe` [1,2,5]
        describe "insertEveryN" $ do
            it "works for 3 2 'x' Hello world" $ do
                insertEveryN 3 2 'x' "Hello world" `shouldBe` "Helxxlo xxworxxld"
        describe "insertAt" $ do
            it "works for 9 1 [1,2,3]" $ do
                insertAt 9 1 [1,2,3] `shouldBe` [1,9,2,3]
