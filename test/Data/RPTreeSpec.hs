{-# options_ghc -Wno-unused-imports #-}
module Data.RPTreeSpec where

import Control.Monad (replicateM)
import Data.Foldable (toList)
import GHC.Word (Word64)

-- conduit
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, last, scanl, print, foldl)
-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, runIO)

import System.Random.SplitMix.Distributions (Gen, sample, GenT, sampleT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR)
import Data.RPTree (forest, knn, sparse, dense,  RPTree, rpTreeCfg, RPTreeConfig(..), candidates, levels, treeSize, points, Inner(..), SVector, fromListSv, DVector, fromListDv, dataSource, Embed(..), randSeed, circle2d)

spec :: Spec
spec = do
  describe "Data.RPTree.Internal : vector space operations" $ do
    let
      vs0, vs1 :: SVector Double
      vs0 = fromListSv 5 [(1, 3.4), (4, 2.1)]
      vs1 = fromListSv 5 [(0, 6.7), (3, 5.5)]
      v1 :: DVector Double
      v1 = fromListDv [1,2,3,4,5]
    it "(^+^) : sparse * dense" $ do
      let
        vsum = vs0 ^+^ v1 :: DVector Double
        vexpect = fromListDv [1, 5.4, 3, 4, 7.1]
      vsum `shouldBe` vexpect
    it "(^-^) : sparse * dense" $ do
      let
        vdiff = vs0 ^-^ v1 :: DVector Double
        vexpect = fromListDv [(- 1), 1.4, (- 3), (- 4), (- 2.9)]
      vdiff `shouldBe` vexpect
    it "inner : sparse * sparse" $ do
      let
        x = vs0 `inner` vs1
      x `shouldBe` 0
    it "inner : sparse * dense" $ do
      let
        x = vs0 `inner` v1
      x `shouldBe` 17.3

  describe "Data.RPTree" $ do
    s <- runIO randSeed
    let
      maxLevs = 10
      n = 10000
      ntrees = 10
      minLeaf = 20
      nchunk = 50
      k = 5
      dim = 2 -- vector dimension
      q = fromListDv [0, 0] -- query
      dats = dataSource n circle2d2  .|
             C.map (\ x -> Embed x ()) -- data
    tts <- sampleT s $ forest s maxLevs minLeaf ntrees nchunk 1.0 2 dats -- forest
    let
        (RPCfg maxLevs' nchunk' _) = rpTreeCfg minLeaf n dim
    tts' <- sampleT s $ forest s maxLevs' minLeaf ntrees nchunk' 1.0 2 dats -- forest
    it "forest : all data points should appear in every tree" $ do
      all (\t -> treeSize t == n) tts `shouldBe` True
    it "knn : results should be close to the query (hand picked params)" $ do
      let
        hits = knn metricL2 k tts q
        dists = map fst $ toList hits
      print hits
      maximum dists `shouldSatisfy` (< 1)
    it "knn : results should be close to the query (rpTreeCfg params)" $ do
      let
        hits = knn metricL2 k tts' q
        dists = map fst $ toList hits
      print hits
      maximum dists `shouldSatisfy` (< 1)


-- test data

-- binary mixture of two non-overlapping circles
circle2d2 :: (Monad m) => GenT m (DVector Double)
circle2d2 = do
  let
    d = fromListDv [2, 3]
    r = 1
  b <- bernoulli 0.5
  if b
    then circle2d r
    else (^+^ d) <$> circle2d r

normalSv :: (Monad m) => Double -> Double -> Int -> GenT m (SVector Double)
normalSv mu sig dim = sparse 0.5 dim (normal mu sig)

normalDv :: (Monad m) => Double -> Double -> Int -> GenT m (DVector Double)
normalDv mu sig dim = dense dim (normal mu sig)

-- gaussMix :: Int -> Int -> [SVector Double]
-- gaussMix m dim = evalGen 1234 $ replicateM m (genGaussMix dim)

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
