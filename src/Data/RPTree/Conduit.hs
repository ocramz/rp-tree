{-# language DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Conduit (
  treeSink, forestSink
  -- ** helpers
  , syntheticData
  ) where

import Control.Exception (Exception(..))
import Control.Monad (replicateM)
import Data.Typeable (Typeable)
import GHC.Word (Word64)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM)
-- containers
import qualified Data.IntMap as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!), map, freeze, thaw, take, drop, unzip)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

import Data.RPTree.Gen (Gen, evalGen, GenT, evalGenT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR, sparse, dense)
import Data.RPTree.Internal (RPTree(..), RPT(..), levels, points, Inner(..), innerSD, innerSS, metricSSL2, metricSDL2, SVector(..), fromListSv, DVector(..), fromListDv, partitionAtMedian)

normal2 :: (Monad m) => GenT m (DVector Double)
normal2 = do
  b <- bernoulli 0.2
  if b
    then dense 2 $ normal 0 0.1
    else dense 2 $ normal 10 0.1


-- | Source of random data points
syntheticData :: (Monad m) =>
                 Int -- ^ number of vectors to generate
              -> GenT m a -- ^ random generator for the vector components
              -> C.ConduitT i a (GenT m) ()
syntheticData n gg = flip C.unfoldM 0 $ \i -> do
  if i == n
    then pure Nothing
    else do
      x <- gg
      pure $ Just (x, i + 1)

-- | Populate a tree from a data stream
--
-- Assumptions on the data source:
--
-- * non-empty : contains at least one value
--
-- * stationary : each chunk is representative of the whole dataset
--
-- * bounded : we wait until the end of the stream to produce a result
treeSink :: (Monad m, Inner SVector v) =>
            Word64 -- ^ random seed
         -> Int -- ^ max tree depth
         -> Int -- ^ data chunk size
         -> Double -- ^ nonzero density of projection vectors
         -> Int -- ^ dimension of projection vectors
         -> C.ConduitT () (v Double) m () -- ^ data source
         -> m (Maybe (RPTree Double (V.Vector (v Double))))
treeSink seed maxDepth n pnz dim src = do
  let
    rvs = evalGen seed $ V.replicateM maxDepth (sparse pnz dim stdNormal)
  tm <- C.runConduit $ src .| insertC maxDepth n rvs .| C.last
  case tm of
    Just t -> pure $ Just $ RPTree rvs t
    _ -> pure Nothing

-- | Incrementally build a tree
insertC :: (Monad m, Inner u v, Ord d, VU.Unbox d, Num d) =>
          Int -- ^ max tree depth
       -> Int -- ^ data chunk size
       -> V.Vector (u d) -- ^ random projection vectors
       -> C.ConduitT (v d) (RPT d (V.Vector (v d))) m ()
insertC maxDepth n rvs = chunked n z (insert maxDepth rvs)
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
forestSink :: (Monad m, Inner SVector v) =>
                 Word64 -- ^ random seed
              -> Int -- ^ max tree depth
              -> Int -- ^ number of trees
              -> Int -- ^ data chunk size
              -> Double -- ^ nonzero density of projection vectors
              -> Int -- ^ dimension of projection vectors
              -> C.ConduitT () (v Double) m () -- ^ data source
              -> m (IM.IntMap (RPTree Double (V.Vector (v Double))))
forestSink seed maxd ntrees chunksize pnz dim src = do
  let
    rvss = evalGen seed $ do
      rvs <- replicateM ntrees $ V.replicateM maxd (sparse pnz dim stdNormal)
      pure $ IM.fromList $ zip [0 .. ] rvs
  tm <- C.runConduit $ src .|
                       insertMultiC maxd chunksize rvss .|
                       C.last
  case tm of
    Just ts -> do
      let
        res = IM.foldlWithKey (\acc i t -> case IM.lookup i rvss of
                                  Just rvs -> IM.insert i (RPTree rvs t) acc
                                  Nothing -> acc) mempty ts
      pure $ res
    _ -> pure mempty


insertMultiC :: (Monad m, Ord d, Inner u v, VU.Unbox d, Num d, VG.Vector v1 (u d)) =>
                Int
             -> Int
             -> IM.IntMap (v1 (u d)) -- one entry per tree
             -> C.ConduitT
                (v d)
                (IM.IntMap (RPT d (V.Vector (v d))))
                m
                ()
insertMultiC maxd n rvss = chunked n mempty (insertMulti maxd rvss)


insertMulti :: (Ord d, Inner u v, VU.Unbox d, Num d, VG.Vector v1 (u d)) =>
               Int
            -> IM.IntMap (v1 (u d))
            -> IM.IntMap (RPT d (V.Vector (v d)))
            -> V.Vector (v d)
            -> IM.IntMap (RPT d (V.Vector (v d)))
insertMulti maxd rvss tacc xs =
  flip IM.mapWithKey tacc $ \i t -> case IM.lookup i rvss of
                                      Just rvs -> insert maxd rvs t xs
                                      _        -> t

insert :: (VG.Vector v1 (u d), Ord d, Inner u v, VU.Unbox d, Num d) =>
          Int
       -> v1 (u d) -- ^ projection vectors
       -> RPT d (V.Vector (v d)) -- ^ accumulator
       -> V.Vector (v d) -- ^ data chunk
       -> RPT d (V.Vector (v d))
insert maxDepth rvs = loop 0
  where
    z = Tip mempty
    loop ixLev tt xs =
      let
        r = rvs VG.! ixLev
      in
        case tt of

          b@(Bin thr tl0 tr0) ->
            if ixLev >= maxDepth || null xs
              then b -- do nothing
              else
                let
                  (_, ll, rr) = partitionAtMedian r xs -- ignore new threshold (?)
                  tl = loop (ixLev + 1) tl0 ll
                  tr = loop (ixLev + 1) tr0 rr
                in Bin thr tl tr

          Tip xs0 -> do
            let xs' = xs <> xs0
            if ixLev >= maxDepth || length xs <= 1
              then Tip xs' -- concat data in leaf
              else
                let
                  (thr, ll, rr) = partitionAtMedian r xs'
                  tl = loop (ixLev + 1) z ll
                  tr = loop (ixLev + 1) z rr
                in Bin thr tl tr

chunked :: (Monad m) =>
           Int -- ^ chunk size
        -> t -- ^ initial tree
        -> (t -> V.Vector a -> t)
        -> C.ConduitT a t m ()
chunked n z f = do C.chunksOf n .|
                     C.map V.fromList .|
                     C.scanl f z -- .|
         -- C.last
  -- case xsm of
  --   Nothing -> throwM $ CNoDataInConduit
  --   Just xs -> pure xs

data CException =
  CNoDataInConduit deriving (Eq, Show, Typeable)
instance Exception CException
