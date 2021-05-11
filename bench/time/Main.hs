{-# LANGUAGE FlexibleContexts #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Word (Word8, Word64)

-- benchpress
import Test.BenchPress (Stats(..), benchmark, printDetailedStats)
-- conduit
import Conduit (runResourceT, MonadResource)
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await, transPipe)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- mnist-idx-conduit
import Data.IDX.Conduit (sourceIdxSparse, sBufSize, sNzComponents)

-- mtl
import Control.Monad.Trans.Class (MonadTrans(..))
-- splitmix
import System.Random.SplitMix (initSMGen, unseedSMGen)
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Unboxed as VU (Unbox, Vector, map)

import Data.RPTree (tree, forest, knn, fromVectorSv, fromListSv, RPForest, RPTree, SVector, Inner(..))

main :: IO ()
main = do -- putStrLn "hello!"
  let
    mnistCfgs = benchConfigs 784
    mnfpath = "assets/mnist/train-images-idx3-ubyte"
  forM_ mnistCfgs $ \cfg -> do
    -- stats <- mnistBench mnfpath mnistV0 cfg
    -- stats <- mnistFBench mnfpath cfg
    stats <- mnistTBench mnfpath cfg
    print cfg
    printDetailedStats stats

benchConfigs :: Int -> [BenchConfig]
benchConfigs pdim = [ BenchConfig maxd minl nt chunk nzd pdim
                    | maxd <- [5, 10],
                      minl <- [100],
                      nt <- [3, 5],
                      chunk <- [1000],
                      nzd <- [0.2, 0.5, 0.8]
                    ]

data BenchConfig = BenchConfig {
  bcMaxTreeDepth :: Int
  , bcMinLeafSize :: Int
  , bcNumTrees :: Int
  , bcChunkSize :: Int
  , bcNZDensity :: Double
  , bcProjDim :: Int
                               } deriving (Show)

-- build and query index
mnistFBQBench :: Inner SVector v =>
                 FilePath -> v Double -> BenchConfig -> IO Stats
mnistFBQBench fp q = forestBench (mnist fp) act 3
  where
    act x = do
      tt <- runResourceT x
      pure $! knn metricL2 10 tt q

-- only build index
mnistFBench :: FilePath -> BenchConfig -> IO Stats
mnistFBench fp = forestBench (mnist fp) act 3
  where
    act x = runResourceT x >> pure ()

mnistTBench :: FilePath -> BenchConfig -> IO Stats
mnistTBench fp = treeBench (mnist fp) act 3
  where
    act x = runResourceT x >> pure ()


mnist :: MonadResource m =>
         FilePath -- path to uncompressed MNIST IDX data file
      -> C.ConduitT a (SVector Double) m ()
mnist fp = sourceIdxSparse fp .|
  C.map (\r -> fromVectorSv (sBufSize r) (VU.map f $ sNzComponents r))
  where
    f (i, x) = (i, toUnitRange x)

toUnitRange :: Word8 -> Double
toUnitRange w8 = fromIntegral w8 / 255


-- | runs a benchmark on a newly created RPForest initialized with a random seed
forestBench ::
  (MonadThrow m, Inner SVector v) =>
  C.ConduitT () (v Double) m ()
  -> (m (RPForest Double (V.Vector (v Double))) -> IO c) -- ^ allows for both deterministic and random data sources
  -> Int -- ^ number of replicates
  -> BenchConfig
  -> IO Stats -- ^ wall-clock time measurement only
forestBench src go n cfg = do
  (_, wct) <- benchmark n setup (const $ pure ()) go
  pure wct
  where
    setup = do
      s <- randSeed
      -- let src' = C.transPipe lift src
      pure $ growForest s cfg src

treeBench :: (Monad m, Inner SVector v) =>
             C.ConduitT () (v Double) m ()
          -> (m (RPTree Double (V.Vector (v Double))) -> IO c)
          -> Int
          -> BenchConfig
          -> IO Stats
treeBench src go n cfg = do
    (_, wct) <- benchmark n setup (const $ pure ()) go
    pure wct
      where
        setup = do
          s <- randSeed
          -- let src' = C.transPipe lift src
          pure $ growTree s cfg src

growTree :: (Monad m, Inner SVector v) =>
            Word64
         -> BenchConfig
         -> C.ConduitT () (v Double) m ()
         -> m (RPTree Double (V.Vector (v Double)))
growTree seed (BenchConfig maxd minl _ chunksize pnz pdim) =
  tree seed maxd minl chunksize pnz pdim

growForest :: (MonadThrow m, Inner SVector v) =>
              Word64
           -> BenchConfig
           -> C.ConduitT () (v Double) m ()
           -> m (RPForest Double (V.Vector (v Double)))
growForest seed (BenchConfig maxd minl ntrees chunksize pnz pdim) =
  forest seed maxd minl ntrees chunksize pnz pdim

randSeed :: MonadIO m => m Word64
randSeed = liftIO (fst . unseedSMGen <$> initSMGen)




mnistV0 :: SVector Double
mnistV0 = fromListSv 784 (map (\(i, x) -> (i, toUnitRange x)) cs)
  where
    cs = [(208,55),(209,148),(210,210),(211,253),(212,253),(213,113),(214,87),(215,148),(216,55),(235,87),(236,232),(237,252),(238,253),(239,189),(240,210),(241,252),(242,252),(243,253),(244,168),(261,4),(262,57),(263,242),(264,252),(265,190),(266,65),(267,5),(268,12),(269,182),(270,252),(271,253),(272,116),(289,96),(290,252),(291,252),(292,183),(293,14),(296,92),(297,252),(298,252),(299,225),(300,21),(316,132),(317,253),(318,252),(319,146),(320,14),(324,215),(325,252),(326,252),(327,79),(343,126),(344,253),(345,247),(346,176),(347,9),(350,8),(351,78),(352,245),(353,253),(354,129),(370,16),(371,232),(372,252),(373,176),(377,36),(378,201),(379,252),(380,252),(381,169),(382,11),(398,22),(399,252),(400,252),(401,30),(402,22),(403,119),(404,197),(405,241),(406,253),(407,252),(408,251),(409,77),(426,16),(427,231),(428,252),(429,253),(430,252),(431,252),(432,252),(433,226),(434,227),(435,252),(436,231),(455,55),(456,235),(457,253),(458,217),(459,138),(460,42),(461,24),(462,192),(463,252),(464,143),(489,62),(490,255),(491,253),(492,109),(517,71),(518,253),(519,252),(520,21),(546,253),(547,252),(548,21),(573,71),(574,253),(575,252),(576,21),(601,106),(602,253),(603,252),(604,21),(629,45),(630,255),(631,253),(632,21),(658,218),(659,252),(660,56),(686,96),(687,252),(688,189),(689,42),(714,14),(715,184),(716,252),(717,170),(718,11),(743,14),(744,147),(745,252),(746,42)]
