module SpecLevel2 where

import Data.Int (Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Level1
import Level2
import SpecLevel1 ()
import Test.Prelude

tests :: NamedTests
tests = nameTests 2 [testOrd, testCycle, testCycling, testShow, testParse, testPairShape, testNested]

testOrd :: Test
testOrd = TestList
  [ TestCase $ do
      assertEqual "листья" EQ (compare IntLeaf IntLeaf)
      assertEqual "лист меньше" LT (compare IntLeaf (n 1))
      assertEqual "узел больше" GT (compare (n 1) IntLeaf)
  , TestCase $ assertEqual "метка игнорируется" EQ $
      compare (IntNode IntLeaf (1, 3) IntLeaf) (IntNode IntLeaf (9, 3) IntLeaf)
  , TestCase $ assertEqual "левое поддерево раньше значения" LT $
      compare (IntNode (n 1) (0, 9) IntLeaf) (IntNode (n 2) (0, 0) IntLeaf)
  , TestCase $ assertEqual "значение раньше правого поддерева" LT $
      compare (IntNode IntLeaf (0, 1) (n 99)) (IntNode IntLeaf (0, 2) IntLeaf)
  , TestCase $ assertEqual "правое поддерево участвует" LT $
      compare (IntNode IntLeaf (0, 1) IntLeaf) (IntNode IntLeaf (0, 1) (n 0))
  , lawsToTest $ ordLaws (Proxy @IntTree) ]
  where n x = IntNode IntLeaf (0, x) IntLeaf

testCycle :: Test
testCycle = TestList
  [ TestCase $ assertEqual "Bool succ" [True, False] (map cycSucc [False, True])
  , TestCase $ assertEqual "Bool pred" [True, False] (map cycPred [False, True])
  , TestCase $ do
      assertEqual "Int8 max" (minBound :: Int8) (cycSucc maxBound)
      assertEqual "Int8 min" (maxBound :: Int8) (cycPred minBound)
  , propertyToTest "обычные значения и обратимость Int8" $ \(x :: Int8) -> conjoin
      [ cycPred (cycSucc x) === x
      , cycSucc (cycPred x) === x
      , cycSucc x === if x == maxBound then minBound else succ x
      , cycPred x === if x == minBound then maxBound else pred x ] ]

-- Собственного CycleEnum нет: слишком сильный контекст Cycling не соберётся.
data Three = One | Two | Three
  deriving (Eq, Show, Enum, Bounded)

testCycling :: Test
testCycling = TestList
  [ TestCase $ assertEqual "обычный шаг" (Cycling Two) (cycSucc (Cycling One))
  , TestCase $ assertEqual "верхняя граница" (Cycling One) (cycSucc (Cycling Three))
  , TestCase $ assertEqual "нижняя граница" (Cycling Three) (cycPred (Cycling One)) ]

testShow :: Test
testShow = TestList
  [ TestCase $ assertEqual "пустая" "EMPTY" (show (Matrix [] :: Matrix Int))
  , TestCase $ assertEqual "без завершающего перевода строки" "[1,2]\n[3,4]" $
      show (Matrix [1 :| [2], 3 :| [4]] :: Matrix Int)
  , TestCase $ assertEqual "символы" "\"ab\"\n\"cd\"" $ show (Matrix ['a' :| "b", 'c' :| "d"])
  , TestCase $ assertEqual "отрицательные" "[-1,0]" $ show (Matrix [(-1) :| [0]] :: Matrix Int) ]

testParse :: Test
testParse = TestList
  [ TestCase $ do
      assertEqual "EMPTY" (Just (Matrix []) :: Maybe (Matrix Int)) (parseMatrix "EMPTY")
      assertEqual "EMPTY с переводом строки" (Just (Matrix []) :: Maybe (Matrix Int)) (parseMatrix "EMPTY\n")
  , TestCase $ assertEqual "две строки" (Just (Matrix [1 :| [2], 3 :| [4]]) :: Maybe (Matrix Int)) $
      parseMatrix "[1,2]\n[3,4]\n"
  , TestCase $ assertEqual "символы" (Just (Matrix ['a' :| "b"])) (parseMatrix "\"ab\"")
  , TestCase $ mapM_ (\s -> assertEqual ("ошибка: " ++ show s) (Nothing :: Maybe (Matrix Int)) (parseMatrix s))
      ["", "[]", "[1]\n[]", "[1]\nwrong", "[1] junk", "\n", "EMPTY\n[1]", "[1]\n\n[2]"]
  , propertyToTest "roundtrip Int" $ forAll (resize 5 (listOf (listOf1 arbitrary))) $ \(rows :: [[Int]]) ->
      let m = Matrix (map NE.fromList rows) in parseMatrix (show m) === Just m
  , propertyToTest "roundtrip Char" $ forAll (resize 5 (listOf (listOf1 arbitrary))) $ \(rows :: [[Char]]) ->
      let m = Matrix (map NE.fromList rows) in parseMatrix (show m) === Just m ]

testPairShape :: Test
testPairShape = TestList
  [ TestCase $ do
      assertBool "фиксированная метка" (("key", 1 :: Int) ==# ("key", True))
      assertBool "метки различаются" (not (("a", True) ==# ("b", False)))
  , propertyToTest "форма пары — фиксированная компонента" $
      \(x :: [Int]) (y :: [Int]) (a :: Bool) (b :: Char) ->
        ((x, a) ==# (y, b)) === (x == y) ]

observe :: PairReaderList Bool Int a -> [[(Int, a)]]
observe = map (\f -> map f [False, True]) . getPairReaderList

testNested :: Test
testNested = TestList
  [ TestCase $ assertEqual "все функции и входы, неизменные метки"
      [[(1, "10"), (1, "20")], [(2, "30"), (3, "40")]] $ observe (fmap show sample)
  , TestCase $ assertEqual "пустой список" ([] :: [[(Int, String)]]) $
      observe (fmap show (PairReaderList [] :: PairReaderList Bool Int Int))
  , propertyToTest "Identity и Composition" $
      \(fs :: [Fun Bool (Int, Int)], Fun _ f :: Fun Int Char, Fun _ g :: Fun Char Bool) ->
        let x = PairReaderList (map applyFun fs) in conjoin
          [ observe (fmap id x) === observe x
          , observe (fmap (g . f) x) === observe (fmap g (fmap f x)) ] ]
  where
    sample :: PairReaderList Bool Int Int
    sample = PairReaderList
      [ \b -> (1, if b then 20 else 10)
      , \b -> if b then (3, 40) else (2, 30) ]
