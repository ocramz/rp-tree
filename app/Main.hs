{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Control.Monad (replicateM)
import Data.Bitraversable (Bitraversable(..))
import Data.Foldable (fold, toList)
import Data.Functor (void)
import GHC.Stack (HasCallStack)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await, transPipe)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print, sinkVector, sinkList)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM)
-- containers
import qualified Data.IntMap as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey)
-- -- exceptions
-- import Control.Monad.Catch (MonadThrow(..))
-- -- mnist-idx-conduit
-- import Data.IDX.Conduit (sourceIdxSparse, sBufSize, sNzComponents)
-- splitmix-distributions
import System.Random.SplitMix.Distributions (Gen, GenT, sample, sampleT, bernoulli, normal)
-- transformers
import Control.Monad.Trans.State.Strict (State, get, put, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, toList, fromList, replicate, zip, zipWith)

import Control.Monad (replicateM)
import Data.RPTree (knn, candidates, rpTreeCfg, RPTreeConfig(..), Embed(..), Inner(..), RPTree, RPForest, SVector, fromListSv, DVector, fromListDv, dense, writeCsv, tree, forest, dataSource, sparse, normal2, normalSparse2, datS, datD, circle2d, leaves, levels, treeSize, leafSizes, writeDot)
-- import Data.RPTree.Internal.Testing (datS, datD)

main :: IO ()
main = do
  let
    n = 10000
    maxd = 5
    minl = 10
    chunk = 100
    dim = 2
    -- cfg = rpTreeCfg n dim
    cfg = RPCfg maxd minl 3 chunk 1.0
  csvTree0 n cfg
  tree0dot n cfg



tree0dot :: Int -> RPTreeConfig -> IO ()
tree0dot n (RPCfg maxd minl _ chunk _) =
  writeDot f fpath "tree0" $ tree0 n maxd minl chunk
  where
    f = show . length
    fpath = "tree0.dot"

csvTree0 :: Int -> RPTreeConfig -> IO ()
csvTree0 n (RPCfg maxd minl _ chunk _) = do
  let
    tt = tree0 n maxd minl chunk
    ttlab = prep tt
  writeCsv "r/scatter_data_2.csv" ttlab

prep :: (Traversable t) => t (V.Vector (Embed v e a)) -> t (V.Vector (v e, Pal5))
prep = flip evalState A . traverse labeled

labeled :: (Enum b) =>
             V.Vector (Embed v e a)
          -> State b (V.Vector (v e, b))
labeled xs = do
  i <- get
  put (succ i)
  let
    n = length xs
    f (Embed x _) ii = (x, ii)
  pure $ V.zipWith f xs (V.replicate n i)

data Pal5 = A | B | C | D | E deriving (Eq, Show)
instance Enum Pal5 where
  toEnum = \case
    0 -> A
    1 -> B
    2 -> C
    3 -> D
    4 -> E
    x -> toEnum (x `mod` 5)
  fromEnum = \case
    A -> 0
    B -> 1
    C -> 2
    D -> 3
    E -> 4



tree0 :: Int -- ^ dataset size
      -> Int -- ^ max tree depth
      -> Int -- ^ min leaf size
      -> Int -- ^ chunk size
      -> RPTree Double () (V.Vector (Embed DVector Double ()))
tree0 n maxd minl chunk = sample s $ tree s maxd minl chunk 1.0 2 (srcC n .| embedC)
  where
    s = 1235137

dataset :: Int -> V.Vector (DVector Double)
dataset n = V.fromList $ sample 1234 $ replicateM n (dense 2 $ normal 0 1)

datasetCircles :: Int -> V.Vector (DVector Double)
datasetCircles n = V.fromList $ sample 1234 $ C.runConduit $ srcCircles n .| C.sinkList



srcC :: Monad m => Int -> C.ConduitT i (DVector Double) (GenT m) ()
srcC n = dataSource n normal2

srcCircles :: Monad m =>
              Int -> C.ConduitT i (DVector Double) (GenT m) ()
srcCircles n = dataSource n circle2d2

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





-- main :: IO ()
-- main = do -- putStrLn "hello!"
--   let
--     n = 1000
--     maxd = 3
--     minl = 10
--     ntree = 10
--     d = 100
--     pnz = 0.3
--     chunk = 20
--     src = datS n d pnz .| C.map (\ x -> Embed x ())
--     -- src = srcCircles n
--     seed = 1234
--   (q, tts) <- sampleT seed $ do
--     tts <- C.runConduit $
--            forest seed maxd minl ntree chunk pnz d (liftC src)
--     q <- sparse 0.3 d (normal 0.1 0.6)
--     pure (q, tts)
--   let
--     res = knn (flip metricL2) 1 tts q
--   print res


-- liftC = C.transPipe lift

embedC :: Monad m => C.ConduitT (v e) (Embed v e ()) m ()
embedC = C.map (\ x -> Embed x ())



-- -- renderTree1 :: Int -> IO ()
-- renderTree1 tt = do
--   let
--     -- csvrows :: [(DVector Double, Pal5)]
--     csvrows = fold $ flip evalState A $ traverse labeledV tt -- (tree1 n)
--   writeCsv "r/scatter_data_rt2.csv" $ V.toList csvrows
