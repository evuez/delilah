import Lib (snakecase)
import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "snakecase" $
      it "returns the all-caps snakecase version of the given string" $
        snakecase "some-name" `shouldBe` "SOME_NAME"
