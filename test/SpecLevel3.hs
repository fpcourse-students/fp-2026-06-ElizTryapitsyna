module SpecLevel3 where

import Control.Exception (SomeException, evaluate, try)
import Level3
import Test.Prelude

tests :: NamedTests
tests = nameTests 3 [testChurch, testArithmetic, testDictionary]

testChurch :: Test
testChurch = TestList
  [ propertyToTest "Show" $ forAll (chooseInteger (0, 100)) $ \n ->
      show (toChurch n) === "Church " ++ show n
  , propertyToTest "Eq" $ forAll (chooseInteger (0, 100)) $ \n ->
      forAll (chooseInteger (0, 100)) $ \m -> (toChurch n == toChurch m) === (n == m)
  , TestCase $ assertBool "разные кодировки одного числа" $ toChurch 2 == Church (\s z -> s (s z)) ]

assertNegative :: String -> Church -> IO ()
assertNegative label n = do
  result <- try @SomeException (evaluate (fromChurch n))
  case result of
    Left _ -> pure ()
    Right _ -> assertFailure (label ++ ": отрицательное число принято")

testArithmetic :: Test
testArithmetic = TestList
  [ propertyToTest "арифметика натуральных" $ forAll (chooseInteger (0, 12)) $ \n ->
      forAll (chooseInteger (0, 12)) $ \m ->
        let x = toChurch n; y = toChurch m in conjoin
          [ fromChurch (succ x) === n + 1
          , fromChurch (pred x) === max 0 (n - 1)
          , fromChurch (x + y) === n + m
          , fromChurch (x * y) === n * m
          , fromChurch (x - y) === max 0 (n - m)
          , fromChurch (abs x) === n
          , fromChurch (signum x) === signum n
          , fromChurch (fromInteger n) === n
          , fromEnum x === fromInteger n
          , fromChurch (toEnum (fromInteger n)) === n ]
  , TestCase $ assertEqual "от 4 до 14" [4, 6 .. 14] (map fromChurch from4To14)
  , TestCase $ do
      -- Сначала проверяем положительный вход: todo не считается корректной ошибкой.
      assertEqual "fromInteger 1" 1 (fromChurch (fromInteger 1))
      assertNegative "fromInteger" (fromInteger (-1))
  , TestCase $ do
      assertEqual "toEnum 1" 1 (fromChurch (toEnum 1))
      assertNegative "toEnum" (toEnum (-1)) ]

testDictionary :: Test
testDictionary = TestList
  [ TestCase $ assertEqual "список Int" [1, 4, 9, 16, 25, 36, 49, 64, 81, 100] case1
  , TestCase $ assertEqual "список String" (map show [1 .. 10 :: Int]) case2
  , TestCase $ assertEqual "Maybe String" (Just "yx") case3
  , TestCase $ assertEqual "один словарь, два типа" ([1, 4, 9, 16, 25, 36, 49, 64, 81, 100], ["ya"]) case4
  , TestCase $ assertEqual "Nothing" (Nothing :: Maybe Bool) (myFmap myMaybeFmap not Nothing)
  , propertyToTest "словарь списка" $ \(xs :: [Int], Fun _ f :: Fun Int Char) ->
      myFmap myListFmap f xs === map f xs ]
