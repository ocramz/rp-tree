{-# options_ghc -Wno-unused-imports #-}
module Data.RPTreeSpec where

import Control.Monad (replicateM)

-- conduit
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, last, scanl, print, foldl)
-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import System.Random.SplitMix.Distributions (Gen, sample, GenT, sampleT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR)
import Data.RPTree (forest, knn, sparse, dense,  RPTree, candidates, levels, points, Inner(..), SVector, fromListSv, DVector, fromListDv, dataSource, Embed(..))

spec :: Spec
spec =
  describe "Data.RPTree" $ do

    it "knn : results should be close to the query" $ do
      let
        maxLevs = 20
        n = 1000
        ntrees = 10
        minLeaf = 20
        nchunk = 50
        k = 5
        dim = 2 -- vector dimension
        q = fromListDv [0.1, (- 0.7)] -- query
        dats = dataSource n (genGaussMix dim)  .| C.map (\ x -> Embed x ()) -- data
      tts <- sampleT 1234 $ forest 1234 maxLevs minLeaf ntrees nchunk 1.0 2 dats -- forest
      let
        hits = knn metricL2 k tts q
      -- let
      --   hits = foldMap (`candidates` q) tts -- candidates tts q
      -- print hits -- DEBUG
      hits `shouldSatisfy` (not . null)

-- test data

normalSv :: (Monad m) => Double -> Double -> Int -> GenT m (SVector Double)
normalSv mu sig dim = sparse 0.5 dim (normal mu sig)

normalDv :: (Monad m) => Double -> Double -> Int -> GenT m (DVector Double)
normalDv mu sig dim = dense dim (normal mu sig)

-- gaussMix :: Int -> Int -> [SVector Double]
-- gaussMix m dim = evalGen 1234 $ replicateM m (genGaussMix dim)

-- genGaussMix :: Int -> Gen (DVector Double) --
genGaussMix :: (Monad m) => Int -> GenT m (DVector Double)
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
