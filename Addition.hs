-- Addition.hs

module Addition where

import Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multiplies :: (Eq a, Num a) => a -> a -> a
multiplies a b = mulrec a b 0
  where mulrec a b acc
          | (b == 0) = acc
          | otherwise = mulrec a (b - 1) (acc + a)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 + 1) :: Integer) > 1 `shouldBe` True
    it "2+2 is equal to 4" $ do
      ((2 + 2) :: Integer) `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "2 multiplied by 3 is 6" $ do
      multiplies 2 3 `shouldBe` 6
    it "2 multiplie by 2 is 4" $ do
      multiplies 2 2 `shouldBe` 4
    it "2 multiplie by 1 is 2" $ do
      multiplies 2 1 `shouldBe` 2
    it "2 multiplie by 0 is 0" $ do
      multiplies 2 0 `shouldBe` 0
