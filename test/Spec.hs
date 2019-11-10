import Lib (snakecase)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "snakecase" $ do
      it "returns the all-caps snakecase version of the given string" $ do
        snakecase "some-name" `shouldBe` "SOME_NAME"
