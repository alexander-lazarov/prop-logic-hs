import Test.Hspec
import Control.Exception (evaluate)
import PropositionalLogic

main :: IO ()
main = hspec $ do

  describe "varValue" $ do
    it "returns false if var does not exist" $ do
      (varValue env "p") `shouldBe` False
    it "returns value if var exists" $ do
      (varValue env "x") `shouldBe` True
    it "returns value if var exists 2" $ do
      (varValue env "y") `shouldBe` False

  describe "evalWithEnv" $ do
    describe "Const" $ do
      it "t" $ do
        (evalWithEnv emptyEnv $ Const True) `shouldBe` True
      it "f" $ do
        (evalWithEnv emptyEnv $ Const False ) `shouldBe` False

    describe "Var" $ do
      it "t" $ do
        (evalWithEnv env $ Var "x") `shouldBe` True
      it "f" $ do
        (evalWithEnv env $ Var "y") `shouldBe` False

    describe "Not" $ do
      it "t" $ do
        (evalWithEnv env $ Not $ ct) `shouldBe` False
      it "f" $ do
        (evalWithEnv env $ Not $ cf) `shouldBe` True

    describe "And" $ do
      it "t t" $ do
        (evalWithEnv env $ ct `And` ct) `shouldBe` True
      it "t f" $ do
        (evalWithEnv env $ ct `And` cf) `shouldBe` False
      it "f t" $ do
        (evalWithEnv env $ cf `And` ct) `shouldBe` False
      it "f f" $ do
        (evalWithEnv env $ cf `And` cf) `shouldBe` False

    describe "Or" $ do
      it "t t" $ do
        (evalWithEnv env $ ct `Or` ct) `shouldBe` True
      it "t f" $ do
        (evalWithEnv env $ ct `Or` cf) `shouldBe` True
      it "f t" $ do
        (evalWithEnv env $ cf `Or` ct) `shouldBe` True
      it "f f" $ do
        (evalWithEnv env $ cf `Or` cf) `shouldBe` False

    describe "Implies" $ do
      it "t t" $ do
        (evalWithEnv env $ ct `Implies` ct) `shouldBe` True
      it "t f" $ do
        (evalWithEnv env $ ct `Implies` cf) `shouldBe` False
      it "f t" $ do
        (evalWithEnv env $ cf `Implies` ct) `shouldBe` True
      it "f f" $ do
        (evalWithEnv env $ cf `Implies` cf) `shouldBe` True

  describe "allVars" $ do
    it "returns an empty list when no vars" $ do
      (allVars $ ct ) `shouldBe` []

    it "handles the example case" $ do
      (allVars $ Var "x" `And` Var "y" `And` Const True) `shouldBe` ["x", "y"]

  describe "bind" $ do
    it "handles empty list" $ do
      (bind [] []) `shouldBe` []

    it "handles the example case" $ do
      (bind ["x", "y"] [True, False]) `shouldBe` [("x", True), ("y", False)]

  where
    ct = Const True
    cf = Const False
    emptyEnv = []
    env = [("x", True), ("y", False), ("z", True)]
