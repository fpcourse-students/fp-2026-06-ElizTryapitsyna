-- | Домашка 6. Необязательные челленджи.
module Level3 where

import MetaUtils (todo, todoImpl)


-- 3.1. Снова числа Чёрча — теперь с классами типов.
-- В Church находится натуральное число: шаг выполняется n раз.
-- Определите Show (точный формат "Church N") и Eq (сравнение значений n).
-- Напишите оба инстанса целиком.
-- toChurch/fromChurch даны, чтобы не повторять прошлые домашки.
-- Используйте Integer при декодировании, чтобы не переполнить Int.

newtype Church = Church (forall a. (a -> a) -> a -> a)

toChurch :: Integer -> Church
toChurch n
  | n < 0 = error "Church: negative input"
  | otherwise = Church $ \s z -> go n s z
  where
    go 0 _ z = z
    go k s z = s (go (k - 1) s z)

fromChurch :: Church -> Integer
fromChurch (Church n) = n (+ 1) 0

todoImpl ''Show ''Church
todoImpl ''Eq ''Church


-- 3.2. Арифметика и перечисление чисел Чёрча.
-- Реализуйте Enum и Num. pred 0 == 0, вычитание насыщается нулём,
-- abs сохраняет число, signum возвращает 0 или 1.
-- Напишите инстансы целиком, самостоятельно
-- выбрав минимально необходимый набор методов.
-- toEnum/fromInteger для отрицательных чисел должны сообщать ошибку.
-- fromEnum нужен только для чисел, помещающихся в Int.
-- Для succ, pred, +, * и - работайте с кодировкой (не декодируйте в Integer).
-- Это учебный Num: из-за отсутствия отрицательных чисел он НЕ удовлетворяет
-- всем законам Num. Не переносите его в код, ожидающий кольцо.
-- Напишите список [4,6,8,10,12,14] с помощью синтаксиса Enum и Num.

todoImpl ''Enum ''Church
todoImpl ''Num ''Church

from4To14 :: [Church]
from4To14 = todo "3.2 range"


-- 3.3. Явный словарь Functor.
-- f остаётся выбранным при создании словаря, а a и b выбираются заново при
-- каждом вызове myFmap. Поэтому forall стоит ВНУТРИ типа поля словаря.
-- RankNTypes уже включено в шаблоне. Закончите два словаря и case4.
-- case4 применяет один и тот же переданный словарь к двум разным типам.
-- Сначала проверьте case1–3, затем прочитайте тип go и реализуйте case4.

newtype MyFunctor f = MkMyFunctor
  { myFmap :: forall a b. (a -> b) -> f a -> f b }

myListFmap :: MyFunctor []
myListFmap = todo "3.3 list"

myMaybeFmap :: MyFunctor Maybe
myMaybeFmap = todo "3.3 maybe"

case1 :: [Int]
case1 = myFmap myListFmap (^ (2 :: Int)) [1 .. 10]

case2 :: [String]
case2 = myFmap myListFmap show ([1 .. 10] :: [Int])

case3 :: Maybe String
case3 = myFmap myMaybeFmap ('y' :) (Just "x")

case4 :: ([Int], [String])
case4 = go myListFmap
  where
    go :: MyFunctor [] -> ([Int], [String])
    go = todo "3.3 two types"
