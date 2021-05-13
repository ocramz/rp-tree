{-# language DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language BangPatterns #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-top-binds #-}
module Data.RPTree.Conduit
  (tree, 
  forest
  -- ** helpers
  , dataSource
  )
where

import Control.Monad (replicateM)
import Data.Functor (void)
import GHC.Word (Word64)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, last, scanl, print, foldl)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM, mapAccum)
-- containers
import qualified Data.IntMap.Strict as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey, intersectionWith)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- splitmix-distributions
import System.Random.SplitMix.Distributions (Gen, sample, GenT, sampleT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!), map, freeze, thaw, take, drop, unzip)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

import Data.RPTree.Gen (sparse, dense)
import Data.RPTree.Internal (RPTree(..), RPForest, RPT(..), levels, points, Inner(..), innerSD, innerSS, metricSSL2, metricSDL2, SVector(..), fromListSv, DVector(..), fromListDv, partitionAtMedian, RPTError(..))


-- | Populate a tree from a data stream
--
-- Assumptions on the data source:
--
-- * non-empty : contains at least one value
--
-- * stationary : each chunk is representative of the whole dataset
--
-- * bounded : we wait until the end of the stream to produce a result
--
-- Throws 'EmptyResult' if the conduit is empty
tree :: (Monad m, Inner SVector v) =>
        Word64 -- ^ random seed
     -> Int -- ^ max tree depth
     -> Int -- ^ min leaf size
     -> Int -- ^ data chunk size
     -> Double -- ^ nonzero density of projection vectors
     -> Int -- ^ dimension of projection vectors
     -> C.ConduitT () (v Double) m () -- ^ data source
     -> m (RPTree Double (V.Vector (v Double)))
tree seed maxDepth minLeaf n pnz dim src = do
  let
    rvs = sample seed $ V.replicateM maxDepth (sparse pnz dim stdNormal)
  t <- C.runConduit $ src .|
                      insertC maxDepth minLeaf n rvs
  pure $ RPTree rvs t


-- tree' seed maxDepth minLeaf n pnz dim = do
--   let
--     rvs = sample seed $ V.replicateM maxDepth (sparse pnz dim stdNormal)
--   t <- insertC maxDepth minLeaf n rvs
--   pure $ RPTree rvs t


-- | Incrementally build a tree
insertC :: (Monad m, Inner u v, Ord d, VU.Unbox d, Fractional d) =>
           Int -- ^ max tree depth
        -> Int -- ^ min leaf size
        -> Int -- ^ data chunk size
        -> V.Vector (u d) -- ^ random projection vectors
        -> C.ConduitT (v d) o m (RPT d (V.Vector (v d))) 
insertC maxDepth minLeaf n rvs = chunkedAccum n z (insert maxDepth minLeaf rvs)
  where
    z = Tip mempty



-- | Populate a forest from a data stream
--
-- Assumptions on the data source:
--
-- * non-empty : contains at least one value
--
-- * stationary : each chunk is representative of the whole dataset
--
-- * bounded : we wait until the end of the stream to produce a result
--
-- Throws 'EmptyResult' if the conduit is empty
forest :: (Monad m, Inner SVector v) =>
          Word64 -- ^ random seed
       -> Int -- ^ max tree depth
       -> Int -- ^ min leaf size
       -> Int -- ^ number of trees
       -> Int -- ^ data chunk size
       -> Double -- ^ nonzero density of projection vectors
       -> Int -- ^ dimension of projection vectors
       -> C.ConduitT () (v Double) m () -- ^ data source
       -> m (RPForest Double (V.Vector (v Double)))
forest seed maxd minl ntrees chunksize pnz dim src = do
  let
    rvss = sample seed $ do
      rvs <- replicateM ntrees $ V.replicateM maxd (sparse pnz dim stdNormal)
      pure $ IM.fromList $ zip [0 .. ] rvs
  ts <- C.runConduit $ src .|
                       insertMultiC maxd minl chunksize rvss
  pure $ IM.intersectionWith RPTree rvss ts



insertMultiC :: (Monad m, Ord d, Inner u v, VU.Unbox d, Fractional d, VG.Vector v1 (u d)) =>
                Int  -- ^ max tree depth
             -> Int -- ^ min leaf size
             -> Int -- ^ chunk size
             -> IM.IntMap (v1 (u d)) -- one entry per tree
             -> C.ConduitT
                (v d)
                o
                m
                (IM.IntMap (RPT d (V.Vector (v d))))
insertMultiC maxd minl n rvss = chunkedAccum n im0 (insertMulti maxd minl rvss)
  where
    im0 = IM.map (const z) rvss
    z = Tip mempty


{-# SCC insertMulti #-}
insertMulti :: (Ord d, Inner u v, VU.Unbox d, Fractional d, VG.Vector v1 (u d)) =>
               Int
            -> Int
            -> IM.IntMap (v1 (u d)) -- ^ projection vectors
            -> IM.IntMap (RPT d (V.Vector (v d))) -- ^ accumulator of subtrees
            -> V.Vector (v d) -- ^ data chunk
            -> IM.IntMap (RPT d (V.Vector (v d)))
insertMulti maxd minl rvss tacc xs =
  flip IM.mapWithKey tacc $ \ !i !t -> case IM.lookup i rvss of
                                      Just !rvs -> insert maxd minl rvs t xs
                                      _        -> t

{-# SCC insert #-}
insert :: (VG.Vector v1 (u d), Ord d, Inner u v, VU.Unbox d, Fractional d) =>
          Int -- ^ max tree depth
       -> Int -- ^ min leaf size
       -> v1 (u d) -- ^ projection vectors
       -> RPT d (V.Vector (v d)) -- ^ accumulator
       -> V.Vector (v d) -- ^ data chunk
       -> RPT d (V.Vector (v d))
insert maxDepth minLeaf rvs = loop 0
  where
    z = Tip mempty
    loop ixLev !tt xs =
      let
        r = rvs VG.! ixLev
      in
        case tt of

          b@(Bin thr0 margin0 tl0 tr0) ->
            if ixLev >= maxDepth || length xs <= minLeaf
              then b -- return current subtree
              else
                let
                  (thr, margin, ll, rr) =
                    partitionAtMedian r xs
                  margin' = margin0 <> margin
                  thr' = (thr0 + thr) / 2
                  tl = loop (ixLev + 1) tl0 ll
                  tr = loop (ixLev + 1) tr0 rr
                in Bin thr' margin' tl tr

          Tip xs0 -> do
            let xs' = xs <> xs0
            if ixLev >= maxDepth || length xs <= minLeaf
              then Tip xs' -- concat data in leaf
              else
                let
                  (thr, margin, ll, rr) = partitionAtMedian r xs'
                  tl = loop (ixLev + 1) z ll
                  tr = loop (ixLev + 1) z rr
                in Bin thr margin tl tr


-- | Aggregate the input stream in chunks of a given size (semantics of 'C.chunksOf'), and fold over the resulting stream building up an accumulator structure (e.g. a tree)
chunkedAccum :: (Monad m) =>
                Int -- ^ chunk size
             -> t -- ^ initial accumulator state
             -> (t -> V.Vector a -> t)
             -> C.ConduitT a o m t
chunkedAccum n z f = C.chunksOf n .|
                     C.map V.fromList .|
                     C.foldl f z

-- | Source of random data points
dataSource :: (Monad m) =>
              Int -- ^ number of vectors to generate
           -> GenT m a -- ^ random generator for the vector components
           -> C.ConduitT i a (GenT m) ()
dataSource n gg = flip C.unfoldM 0 $ \i -> do
  if i == n
    then pure Nothing
    else do
      x <- gg
      pure $ Just (x, i + 1)



