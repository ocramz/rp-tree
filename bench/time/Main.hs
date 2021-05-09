{-# LANGUAGE FlexibleContexts #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Control.Monad.IO.Class (MonadIO(..))
import GHC.Word (Word64)

-- benchpress
import Test.BenchPress (Stats(..), benchmark)
-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await, transPipe)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- mtl
import Control.Monad.Trans.Class (MonadTrans(..))
-- splitmix
import System.Random.SplitMix (initSMGen, unseedSMGen)
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)

import Data.RPTree (forest, RPForest, RPTree, SVector, Inner(..))

main :: IO ()
main = putStrLn "hello!"

data BenchConfig = BenchConfig {
  bcMaxTreeDepth :: Int
  , bcMinLeafSize :: Int
  , bcNumTrees :: Int
  , bcChunkSize :: Int
  , bcNZDensity :: Double
  , bcProjDim :: Int
                               }

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

growForest :: (MonadThrow m, Inner SVector v) =>
              Word64
           -> BenchConfig
           -> C.ConduitT () (v Double) m ()
           -> m (RPForest Double (V.Vector (v Double)))
growForest seed (BenchConfig maxd minl ntrees chunksize pnz pdim) =
  forest seed maxd minl ntrees chunksize pnz pdim

randSeed :: MonadIO m => m Word64
randSeed = liftIO (fst . unseedSMGen <$> initSMGen)
