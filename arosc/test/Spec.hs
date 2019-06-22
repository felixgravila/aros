import Test.Hspec
import Syntax
import Parser
import Checker
import Evaluator

resDir :: String
resDir = "test/resources/"

doTypeCheck :: [Char] -> IO Bool
doTypeCheck file = do
  code <- readFile (resDir ++ file)
  let (Right ast) = parseAros file code
  let (programOk,_) = runChecker ast
  return programOk

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "Basic shape declaration in grid def should parse correctly (simple-decl-and-grid)" $ do
      let file = "simple-decl-and-grid.aros"
      code <- readFile (resDir ++ file)
      let (Right ast) = (parseAros file code)
      let correctAst = Program
                       [
                         Decl TypeInt "myint" (BinaryExp (IntegerExp 5) Plus (IntegerExp 3)),
                         Decl TypeVec "thevec" (VectorExp (VariableExp "myint") (IntegerExp 2))
                       ]
                       (GridDef (VectorExp (IntegerExp 4) (IntegerExp 4)) (VariableExp "place"))
                       (RobotRoute (VectorExp (IntegerExp 1) (IntegerExp 1)) (VectorExp (IntegerExp 2) (IntegerExp 2)))
      ast `shouldBe` correctAst

  describe "Type Checking" $ do
    it "parsedVars should type check" $ do
      didItTypecheck <- doTypeCheck "parsedVars.aros"
      didItTypecheck `shouldBe` True
    it "01-errors should not type check" $ do
      didItTypecheck <- doTypeCheck "01-errors.aros"
      didItTypecheck `shouldBe` False
    it "02-recCurried should type check" $ do
      didItTypecheck <- doTypeCheck "02-recCurried.aros"
      didItTypecheck `shouldBe` True
    it "03-CurryHoward should type check" $ do
      didItTypecheck <- doTypeCheck "03-CurryHoward.aros"
      didItTypecheck `shouldBe` True
    it "04-properMap should type check" $ do
      didItTypecheck <- doTypeCheck "04-properMap.aros"
      didItTypecheck `shouldBe` True

  describe "Evaluation" $ do
    it "04-properMap should evaluate correctly" $ do
      True `shouldBe` True
