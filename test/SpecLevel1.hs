module SpecLevel1 where

import Examples qualified as E
import Level1
import Test.Prelude

tests :: NamedTests
tests = nameTests 1 [testRead, testEq, testFunctor, testShape]

testRead :: Test
testRead = TestList
  [ TestCase $ assertEqual "представление Warning" (E.render E.Warning) predictedShow
  , TestCase $ assertEqual "метка не участвует в равенстве"
      (E.Labelled "old" 7 == E.Labelled "new" 7) predictedEq
  , TestCase $ assertEqual "fmap сохраняет Just" (fmap not (Just True)) predictedFmap ]

instance Arbitrary IntTree where
  arbitrary = sized gen
    where
      gen 0 = pure IntLeaf
      gen n = frequency
        [ (1, pure IntLeaf)
        , (3, IntNode <$> gen (n `div` 3) <*> arbitrary <*> gen (n `div` 3)) ]

key :: IntTree -> (String, [Int])
key t = (shape t, values t)
  where
    shape IntLeaf = "L"
    shape (IntNode l _ r) = "N(" ++ shape l ++ "," ++ shape r ++ ")"
    values IntLeaf = []
    values (IntNode l (_, x) r) = values l ++ [x] ++ values r

testEq :: Test
testEq = TestList
  [ TestCase $ assertBool "служебная метка игнорируется" $
      IntNode IntLeaf (42, 2) IntLeaf == IntNode IntLeaf (24, 2) IntLeaf
  , TestCase $ assertBool "значение участвует" $
      IntNode IntLeaf (0, 2) IntLeaf /= IntNode IntLeaf (0, 3) IntLeaf
  , TestCase $ assertBool "оба поддерева участвуют" $
      IntNode (IntNode IntLeaf (0, 1) IntLeaf) (0, 2) IntLeaf /=
      IntNode IntLeaf (0, 2) (IntNode IntLeaf (0, 1) IntLeaf)
  , propertyToTest "форма и значения" $ \a b -> (a == b) === (key a == key b)
  , lawsToTest $ eqLaws (Proxy @IntTree) ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen
    where
      gen 0 = oneof [pure Leaf, Node <$> arbitrary <*> pure []]
      gen n = frequency
        [ (1, pure Leaf)
        , (3, do
            k <- chooseInt (0, 3)
            Node <$> arbitrary <*> vectorOf k (gen (n `div` 3))) ]

referenceMap :: (a -> b) -> Tree a -> Tree b
referenceMap _ Leaf = Leaf
referenceMap f (Node x cs) = Node (f x) (map (referenceMap f) cs)

testFunctor :: Test
testFunctor = TestList
  [ TestCase $ assertEqual "узел без детей хранит значение" (Node False []) $ fmap not (Node True [])
  , TestCase $ assertEqual "разный тип, порядок, пустые дети"
      (Node "1" [Leaf, Node "2" [Node "3" [], Leaf]]) $
      fmap show (Node (1 :: Int) [Leaf, Node 2 [Node 3 [], Leaf]])
  , propertyToTest "все узлы" $ \(t :: Tree Int, Fun _ f :: Fun Int Char) ->
      fmap f t === referenceMap f t
  , lawsToTest $ functorLaws (Proxy @Tree) ]

treeShape :: Tree a -> [Int]
treeShape Leaf = [-1]
treeShape (Node _ children) = length children : concatMap treeShape children

testShape :: Test
testShape = TestList
  [ TestCase $ do
      assertBool "пустые списки разных типов" (([] :: [Int]) ==# ([] :: [Bool]))
      assertBool "списки разных типов" ([1, 2 :: Int] ==# [True, False])
      assertBool "лишний элемент" (not ([1 :: Int] ==# [True, False]))
      assertBool "пустой и непустой" (not (([] :: [Int]) ==# [True]))
  , propertyToTest "форма списка — длина" $ \(xs :: [Int]) (ys :: [Bool]) ->
      (xs ==# ys) === (length xs == length ys)
  , TestCase $ do
      assertBool "листья разных типов" ((Leaf :: Tree Int) ==# (Leaf :: Tree Bool))
      assertBool "те же дети" (Node (1 :: Int) [Leaf, Node 2 []] ==# Node True [Leaf, Node False []])
      assertBool "Leaf и Node" (not (Leaf ==# Node True []))
      assertBool "число детей" (not (Node True [Leaf] ==# Node True []))
      assertBool "порядок детей" (not (Node True [Leaf, Node True []] ==# Node True [Node True [], Leaf]))
      assertBool "глубина" (not (Node True [Node True []] ==# Node True [Node True [Leaf]]))
  , propertyToTest "форма дерева, элементы разных типов" $ \(x :: Tree Int) (y :: Tree Bool) ->
      (x ==# y) === (treeShape x == treeShape y) ]
