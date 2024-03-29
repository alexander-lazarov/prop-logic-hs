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

  describe "allBools" $ do
    it "0" $ do
      (allBools 0) `shouldBe` []

    it "1" $ do
      (allBools 1) `shouldBe` [[False], [True]]

    it "2" $ do
      (allBools 2) `shouldBe` [[False, False], [False, True], [True, False], [True, True]]

  describe "allEnvs" $ do
    it "handles 0 vars" $ do
      (allEnvs []) `shouldBe` []

    it "handles 1 var" $ do
      (allEnvs ["x"]) `shouldBe` [ [("x", False)]
                                 , [("x", True)]
                                 ]

    it "handles 2 vars" $ do
      (allEnvs ["x", "y"]) `shouldBe` [ [("x", False), ("y", False)]
                                      , [("x", False), ("y", True)]
                                      , [("x", True), ("y", False)]
                                      , [("x", True), ("y", True)]
                                      ]

  describe "isTautology" $ do
    it "example 1" $ do
      (isTautology $ x) `shouldBe` False

    it "example 2" $ do
      (isTautology $ x `Or` nx) `shouldBe` True

    it "example 3" $ do
      (isTautology $ x `Or` y) `shouldBe` False

  describe "isSatisfiable" $ do
    it "example 1" $ do
      (isSatisfiable $ x) `shouldBe` True

    it "example 2" $ do
      (isSatisfiable $ x `Implies` nx) `shouldBe` True

    it "example 3" $ do
      (isSatisfiable $ x `And` nx) `shouldBe` False

  describe "isContradiction" $ do
    it "example 1" $ do
      (isContradiction $ x) `shouldBe` False

    it "example 2" $ do
      (isContradiction $ x `And` nx) `shouldBe` True

    it "example 3" $ do
      (isContradiction $ x `Or` nx) `shouldBe` False

  describe "sematicallyImplies" $ do
    it "example 1" $ do
      (semanticallyImplies x x) `shouldBe` True

    it "example 2" $ do
      (semanticallyImplies x y) `shouldBe` False

    it "example 3" $ do
      (semanticallyImplies (x `Or` nx) x) `shouldBe` False

    it "example 4" $ do
      (semanticallyImplies x (x `Or` y)) `shouldBe` True

    it "example 5" $ do
      (semanticallyImplies (x `And` nx) y) `shouldBe` True

    it "example 6" $ do
      (semanticallyImplies y (x `Implies` x)) `shouldBe` True

  describe "semanticallyEquivalent" $ do
    it "example 1" $ do
      (semanticallyEquivalent x x) `shouldBe` True

    it "example 2" $ do
      (semanticallyEquivalent x y) `shouldBe` False

    it "example 3" $ do
      (semanticallyEquivalent (x `And` y) (y `And` x)) `shouldBe` True

  describe "isAxiom" $ do
    it "not an axiom 1" $ do
      (isAxiom $ x `Implies` y `Implies` z) `shouldBe` False
    it "not an axiom 2" $ do
      (isAxiom nax2) `shouldBe` False
    it "not an axiom 3" $ do
      (isAxiom nax3) `shouldBe` False
    it "THEN-1 - case 1" $ do
      (isAxiom $ x `Implies` y `Implies` x) `shouldBe` True
    it "THEN-1 - case 2" $ do
      (isAxiom $ y `Implies` x `Implies` y) `shouldBe` True
    it "THEN-1 - case 3" $ do
      (isAxiom $ xiy `Implies` y `Implies` xiy) `shouldBe` True
    it "THEN-2 - case 1" $ do
      (isAxiom $ then2) `shouldBe` True
    it "AND-1 - case 1" $ do
      (isAxiom $ x `And` y `Implies` x) `shouldBe` True
    it "AND-2 - case 1" $ do
      (isAxiom $ x `And` y `Implies` y) `shouldBe` True
    it "AND-3 - case 1" $ do
      (isAxiom $ y `Implies` (x `Implies` (y `And` x))) `shouldBe` True
    it "AND-3 - case 2" $ do
      (isAxiom $ y `Implies` (x `Implies` (x `And` y))) `shouldBe` True
    it "OR-1 - case 1" $ do
      (isAxiom $ x `Implies` (x `Or` y)) `shouldBe` True
    it "OR-2 - case 1" $ do
      (isAxiom $ x `Implies` (y `Or` x)) `shouldBe` True
    it "OR-3 - case 1" $ do
      (isAxiom or3_1) `shouldBe` True
    it "OR-3 - case 2" $ do
      (isAxiom or3_2) `shouldBe` True
    it "NOT-1 - case 1" $ do
      (isAxiom not1) `shouldBe` True
    it "NOT-2 - case 1" $ do
      (isAxiom $ x `Implies` (nx`Implies` y)) `shouldBe` True
    it "NOT-3 - case 1" $ do
      (isAxiom $ x `Or` nx) `shouldBe` True
  describe "modusPonens" $ do
    it "case 1" $ do
      (modusPonens x (x `Implies` y) y) `shouldBe` True
    it "case 2" $ do
      (modusPonens x (x `Implies` y) x) `shouldBe` False
    it "case 1" $ do
      (modusPonens (x `And` y) (x `Implies` y) y) `shouldBe` False
  describe "proofFrom" $ do
    it "case 1" $ do
      (proofFrom hypSymAnd symAnd) `shouldBe` True
    it "case 2" $ do
      (proofFrom [] impl) `shouldBe` True
    it "case 3" $ do
      (proofFrom [] [x `And` y]) `shouldBe` False
  describe "proove" $ do
    it "case 1" $ do
      (proove [] $ Not x) `shouldBe` Nothing
    it "case 2" $ do
      (proove [] $ x `Or` Not x) `shouldBe` Just []
    it "case 3" $ do
      (proove [x `And` y] $ y `And` x) `shouldBe` Just [x `And` y]
    it "case 4" $ do
      (proove [x `And` y] $ x) `shouldBe` Just [x `And` y]
    it "case 5" $ do
      (proove [x `And` y, y `And` z] $ x) `shouldBe` Just [x `And` y]
    it "case 6" $ do
      (proove [x `And` y] $ y `Implies` x `Implies` y `And` x) `shouldBe` Just []


  where
    ct = Const True
    cf = Const False

    emptyEnv = []
    env      = [("x", True), ("y", False), ("z", True)]

    x = Var "x"
    y = Var "y"
    z = Var "z"

    xiy = x `Implies` y
    xiz = x `Implies` z
    yiz = y `Implies` z
    xay = xiy `And` xiy
    nx  = Not x
    ny  = Not y

    nax2  = xiy `Implies` xiy `Implies` x
    nax3  = xiy `Implies` xiy `Implies` x
    then2 = (x `Implies` yiz) `Implies` (xiy `Implies` xiz)
    or3_1 = xiz `Implies` (yiz `Implies` ((x `Or` y) `Implies` z))
    or3_2 = xiz `Implies` (yiz `Implies` ((y `Or` x) `Implies` z))
    not1  = (x `Implies` y) `Implies` ((x `Implies` ny) `Implies` nx)

    hypSymAnd = [x `And` y]
    symAnd    = [ x `And` y `Implies` x
                , x `And` y `Implies` y
                , x `And` y
                , y
                , x
                , y `Implies` x `Implies` y `And` x
                , x `Implies` y `And` x
                , y `And` x]
    impl     = [ ((x `Implies` ((y `Implies` x) `Implies` x)) `Implies` ((x `Implies` (y `Implies` x)) `Implies` (x `Implies` x)))
               , (x `Implies` ((y `Implies` x) `Implies` x))
               , ((x `Implies` (y `Implies` x)) `Implies` (x `Implies` x))
               , (x `Implies` (y `Implies` x))
               , (x `Implies` x)
               ]

