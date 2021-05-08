{-# options_ghc -Wno-unused-imports #-}
module Data.RPTreeSpec where

import Control.Monad (replicateM)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import System.Random.SplitMix.Distributions (Gen, sample, GenT, sampleT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR)
import Data.RPTree (sparse, dense,  RPTree, getLeaf, levels, points, Inner(..), innerSD, innerSS, metricSSL2, metricSDL2, SVector, fromListSv, DVector, fromListDv)

spec :: Spec
spec =
  describe "Data.RPTree" $ do
    it "WIP" $ do
      1 `shouldBe` 1
  
    -- it "build : max number of tree levels should be bounded" $ do
    --   let
    --     maxLevs = 20
    --     n = 100
    --     dim = 2
    --     dats = evalGen 1234 $ replicateM n (genGaussMix dim)
    --     -- tt :: RPTree Double [DVector Double]
    --     tt = evalGen 1337 $ tree maxLevs 1.0 2 dats
    --   levels tt `shouldSatisfy` (<= maxLevs)
    -- it "nearest : counting search" $ do
    --   let
    --     maxLevs = 20
    --     n = 1000
    --     ntrees = 10
    --     thr = 3 -- voting threshold
    --     dim = 2 -- vector dimension
    --     q = fromListDv [0.1, (- 0.7)] -- query
    --     dats = evalGen 1234 $ replicateM n (genGaussMix dim) -- data
    --     tts = evalGen 1337 $ forest ntrees maxLevs 1.0 2 dats -- forest
    --     hits = nearest thr tts q
    --   print hits -- DEBUG
    --   hits `shouldSatisfy` (not . null)

-- test data

normalSv :: Double -> Double -> Int -> Gen (SVector Double)
normalSv mu sig dim = sparse 0.5 dim (normal mu sig)

normalDv :: Double -> Double -> Int -> Gen (DVector Double)
normalDv mu sig dim = dense dim (normal mu sig)

-- gaussMix :: Int -> Int -> [SVector Double]
-- gaussMix m dim = evalGen 1234 $ replicateM m (genGaussMix dim)

genGaussMix :: Int -> Gen (DVector Double)
genGaussMix dim = do
  b <- bernoulli 0.5
  if b
    then normalDv 0 1 dim
    else normalDv 3 2 dim

-- normalP :: Double -> Double -> Int -> Gen (P Double)
-- normalP mu sig d = P <$> VG.replicateM d (normal mu sig)

-- mkP :: (VU.Unbox a) => [a] -> P a
-- mkP = P . VU.fromList

-- newtype P a = P (VU.Vector a) deriving (Eq, Show)
-- instance InnerS P where
--   innerS sv1 (P v) = innerSD sv1 v

-- tt0 :: RPTree Double [SVector Double]
-- tt0 = evalGen 1337 $ build 20 1.0 2 (gaussMix 200 2)

-- genDataset :: Int -> Int -> [P Double]
-- genDataset m d = evalGen 1234 $ replicateM m (genP d)

-- genP :: Int -> Gen (P Double)
-- genP d = P <$> VG.replicateM d stdNormal
