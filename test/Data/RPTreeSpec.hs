{-# options_ghc -Wno-unused-imports #-}
module Data.RPTreeSpec where

import Control.Monad (replicateM)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.RPTree (Gen, evalGen, GenT, evalGenT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR, sparse, RPTree, build, levels, points, Inner(..), innerSD, innerSS, metricSSL2, metricSDL2, SVector, fromList)

spec :: Spec
spec =
  describe "Data.RPTree" $ do
    it "build : max number of tree levels should be bounded" $ do
      let
        maxLevs = 20
        tt :: RPTree Double [SVector Double]
        tt = evalGen 1337 $ build maxLevs 1.0 2 (gaussMix 200 2)
      levels tt `shouldSatisfy` (<= maxLevs)

-- test data

normalSv :: Double -> Double -> Int -> Gen (SVector Double)
normalSv mu sig dim = sparse 0.5 dim (normal mu sig)

tt0 :: RPTree Double [SVector Double]
tt0 = evalGen 1337 $ build 20 1.0 2 (gaussMix 200 2)

-- genDataset :: Int -> Int -> [P Double]
-- genDataset m d = evalGen 1234 $ replicateM m (genP d)

-- genP :: Int -> Gen (P Double)
-- genP d = P <$> VG.replicateM d stdNormal

gaussMix :: Int -> Int -> [SVector Double]
gaussMix m dim = evalGen 1234 $ replicateM m (genGaussMix dim)

genGaussMix :: Int -> Gen (SVector Double)
genGaussMix dim = do
  b <- bernoulli 0.5
  if b
    then normalSv 0 1 dim
    else normalSv 3 2 dim

-- normalP :: Double -> Double -> Int -> Gen (P Double)
-- normalP mu sig d = P <$> VG.replicateM d (normal mu sig)

-- mkP :: (VU.Unbox a) => [a] -> P a
-- mkP = P . VU.fromList

-- newtype P a = P (VU.Vector a) deriving (Eq, Show)
-- instance InnerS P where
--   innerS sv1 (P v) = innerSD sv1 v
