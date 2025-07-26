module MVectorSpec where

import Control.Monad
import qualified Data.Vector.HIP.Mutable as MHV
import qualified Data.Vector.Storable as VS
import Test.Hspec

spec :: Spec
spec = do
  describe "creation" $ do
    it "new" $ do
      void (MHV.new 4 :: IO (MHV.IOVector Float))

    it "replicate" $ do
      v <- MHV.replicate 4 100 :: IO (MHV.IOVector Float)
      w <- VS.freeze =<< MHV.copyToHost v
      VS.toList w `shouldBe` [100, 100, 100, 100]

    it "generate" $ do
      v <- MHV.generate 4 id :: IO (MHV.IOVector Int)
      w <- VS.freeze =<< MHV.copyToHost v
      VS.toList w `shouldBe` [0, 1, 2, 3]

  describe "read/write" $ do
    it "read" $ do
      v <- MHV.generate 4 (\i -> i * i) :: IO (MHV.IOVector Int)
      forM_ [0 .. 3] $ \i -> do
        x <- MHV.read v i
        x `shouldBe` (i * i)

      x <- MHV.readMaybe v 2
      x `shouldBe` Just 4

      y <- MHV.readMaybe v 4
      y `shouldBe` Nothing

  describe "misc" $ do
    it "length" $ do
      v <- MHV.new 99 :: IO (MHV.IOVector Int)
      MHV.length v `shouldBe` 99

    it "null" $ do
      v <- MHV.new 0 :: IO (MHV.IOVector Int)
      MHV.length v `shouldBe` 0
      MHV.null v `shouldBe` True

    it "split" $ do
      v <- MHV.generate 5 (\i -> i * i) :: IO (MHV.IOVector Int)
      let (v1, v2) = MHV.splitAt 3 v
      w1 <- VS.freeze =<< MHV.copyToHost v1
      w2 <- VS.freeze =<< MHV.copyToHost v2

      VS.toList w1 `shouldBe` [0, 1, 4]
      VS.toList w2 `shouldBe` [9, 16]

    it "take" $ do
      v <- MHV.generate 4 (\i -> i * 2) :: IO (MHV.IOVector Int)

      let vars = [(0, []), (3, [0, 2, 4]), (4, [0, 2, 4, 6]), (7, [0, 2, 4, 6])]

      forM_ vars $ \(n, o) -> do
        let v1 = MHV.take n v
        w1 <- VS.freeze =<< MHV.copyToHost v1
        VS.toList w1 `shouldBe` o

    it "drop" $ do
      v <- MHV.generate 4 (\i -> i * 2) :: IO (MHV.IOVector Int)

      let vars = [(0, [0, 2, 4, 6]), (3, [6]), (4, []), (7, [])]

      forM_ vars $ \(n, o) -> do
        let v1 = MHV.drop n v
        w1 <- VS.freeze =<< MHV.copyToHost v1
        VS.toList w1 `shouldBe` o

    it "slice" $ do
      v <- MHV.generate 5 (\i -> i * 2) :: IO (MHV.IOVector Int)
      let v1 = MHV.slice 2 2 v
      w1 <- VS.freeze =<< MHV.copyToHost v1
      VS.toList w1 `shouldBe` [4, 6]

    it "init" $ do
      v <- MHV.generate 5 (\i -> i * 2) :: IO (MHV.IOVector Int)
      let v1 = MHV.init v
      w1 <- VS.freeze =<< MHV.copyToHost v1
      VS.toList w1 `shouldBe` [0, 2, 4, 6]

    it "tail" $ do
      v <- MHV.generate 5 (\i -> i * 2) :: IO (MHV.IOVector Int)
      let v1 = MHV.tail v
      w1 <- VS.freeze =<< MHV.copyToHost v1
      VS.toList w1 `shouldBe` [2, 4, 6, 8]
