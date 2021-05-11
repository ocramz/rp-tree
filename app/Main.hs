{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Data.Foldable (fold)
-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM)
-- containers
import qualified Data.IntMap as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- splitmix-distributions
import System.Random.SplitMix.Distributions (Gen, GenT, sample, sampleT, bernoulli, normal)
-- transformers
import Control.Monad.Trans.State.Lazy (State, get, put, evalState)
-- vector
import qualified Data.Vector as V (Vector, toList, fromList, replicate, zip)

import Control.Monad (replicateM)
import Data.RPTree (knn, candidates, Inner(..), RPTree, RPForest, leaves, SVector, DVector, fromListDv, dense, writeCsv, forest, dataSource, normal2)

main :: IO ()
main = do
  let
    n = 10000
  -- renderTree0 n
  -- renderTree1 n
  undefined -- FIXME


-- renderTree0 :: Int -> IO ()
renderTree0 tt = do
  let csvrows = V.toList $ fold $ flip evalState A $ traverse labeledV tt -- (tree0 n)
  writeCsv "r/scatter_data.csv" csvrows

-- renderTree1 :: Int -> IO ()
renderTree1 tt = do
  let
    -- csvrows :: [(DVector Double, Pal5)]
    csvrows = fold $ flip evalState A $ traverse labeledV tt -- (tree1 n)
  writeCsv "r/scatter_data_rt2.csv" $ V.toList csvrows

labeled :: (Enum i) =>
           [a] -> State i [(a, i)]
labeled xs = do
  i <- get
  put (succ i)
  let n = length xs
  pure $ zip xs (replicate n i)


labeledV :: Enum i => V.Vector a -> State i (V.Vector (a, i))
labeledV xs = do
  i <- get
  put (succ i)
  let n = length xs
  pure $ V.zip xs (V.replicate n i)

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

-- tree0 :: Int -> RPTree Double (V.Vector (DVector Double))
-- tree0 n = evalGen 1234 $ tree 10 1.0 2 (dataset n)

-- tree1 :: Int -> RT SVector Double (V.Vector (DVector Double))
-- tree1 n = evalGen 1234 $ treeRT 10 20 1.0 2 (dataset n)

dataset :: Int -> V.Vector (DVector Double)
dataset n = V.fromList $ sample 1234 $ replicateM n (dense 2 $ normal 0 1)


-- treeC0 :: MonadThrow m =>
--           Int -> GenT m (RPTree Double (V.Vector (DVector Double)))
-- treeC0 n = treeSink 1234 10 20 100 1.0 2 (srcC n)

{-
λ> nn0 10000 (fromListDv [0,0])
[(0.13092191004810114,DV [-8.771274989760332e-2,9.71957819868927e-2]),(0.14722273682679538,DV [-4.767722969780902e-2,0.13928896584839093]),(0.1626065099556818,DV [-4.57842765697381e-2,0.15602780873598454]),(0.22082909577433263,DV [-3.62336905451185e-2,0.21783619811681887]),(0.22085935710897311,DV [0.21196201255823421,-6.2056110535964756e-2]),(0.2636139991233282,DV [-0.24290511334764195,0.10241799862994452]),(0.3869415454995779,DV [-0.3658837577279577,0.12590804368455188]),(0.3951528583078011,DV [-0.3543713488257354,0.1748334308999686]),(0.6174219338196472,DV [-0.4952807707701239,0.3686553979897009]),(0.6968774335522048,DV [-0.6408548616154526,0.2737575638007956])]
-}
nn0 :: (Inner SVector v, Inner DVector v) =>
       Int -> v Double -> V.Vector (Double, DVector Double)
nn0 n q = case ttsm of
  Just tts -> knn metricL2 10 tts q -- FIXME voting search size ?!
  -- Nothing -> mempty
  where
    ttsm = sampleT 1234 $ forestC0 n

cs0 n q = case sampleT 1234 $ forestC0 n of
  Just tts -> (`candidates` q) <$> tts

forestC0 :: MonadThrow m =>
            Int
         -> GenT
            m
            (IM.IntMap (RPTree Double (V.Vector (DVector Double))))
forestC0 n = forest 1234 10 20 10 100 1.0 2 (srcC n)

srcC :: Monad m => Int -> C.ConduitT i (DVector Double) (GenT m) ()
srcC n = dataSource n normal2


