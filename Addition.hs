-- Addition.hs

module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multiplies :: (Eq a, Num a) => a -> a -> a
multiplies a b = mulrec a b 0
  where mulrec a' b' acc
          | (b' == 0) = acc
          | otherwise = mulrec a' (b' - 1) (acc + a')

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2,2,2,2,2, 3]

genBool :: Gen Bool
genBool = choose (False, True)
genBool' :: Gen Bool
genBool' = elements [False, True]
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 + 1) :: Integer) > 1 `shouldBe` True
    it "2+2 is equal to 4" $ do
      ((2 + 2) :: Integer) `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      (dividedBy 15 3 :: (Integer, Integer)) `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      (dividedBy 22 5 :: (Integer, Integer)) `shouldBe` (4, 2)
    it "2 multiplied by 3 is 6" $ do
      (multiplies 2 3 :: Integer) `shouldBe` 6
    it "2 multiplied by 2 is 4" $ do
      (multiplies 2 2 :: Integer) `shouldBe` 4
    it "2 multiplied by 1 is 2" $ do
      (multiplies 2 1 :: Integer) `shouldBe` 2
    it "2 multiplied by 0 is 0" $ do
      (multiplies 2 0 :: Integer) `shouldBe` 0
    it "x + 1 is always greater than x" $ do
      property $ \x -> x+1 > (x :: Int)
