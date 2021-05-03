{-# LANGUAGE FlexibleContexts #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Conduit (
  treeSink
  -- ** helpers
  , syntheticData
  ) where

import GHC.Word (Word64)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanlM, last, print)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM)
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

-- | Source of random data points
syntheticData :: (Monad m, VG.Vector VU.Vector a) =>
                 Int -- ^ number of vectors to generate
              -> Int -- ^ vector  dimension
              -> GenT m a -- ^ random generator for the vector components
              -> C.ConduitT i (DVector a) (GenT m) ()
syntheticData n dim gg = flip C.unfoldM 0 $ \i -> do
  if i == n
    then pure Nothing
    else do
      x <- dense dim gg
      pure $ Just (x, i + 1)

-- | Populate a tree from a data stream
--
-- Assumptions on the data source:
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

chunked :: Monad m =>
           Int
        -> t
        -> (t -> V.Vector a -> m t)
        -> C.ConduitT a t m ()
chunked n z f = C.chunksOf n .| C.map V.fromList .| C.scanlM f z

-- | Incrementally build a tree
--

insertC :: (Monad m, Inner u v, Ord d, VU.Unbox d, Num d) =>
          Int
       -> Int -- ^ data chunk size
       -> V.Vector (u d) -- ^ random projection vectors
       -> C.ConduitT (v d) (RPT d (V.Vector (v d))) m ()
insertC maxDepth n rvs = chunked n z $ \tree0 xss -> do
  loop 0 tree0 xss
  where
    z = Tip mempty
    loop ixLev tt xs = case tt of

      b@(Bin thr tl0 tr0) -> do
        if ixLev >= maxDepth || null xs
          then pure b -- do nothing
          else
          do
            let
              r = rvs VG.! ixLev
              (_, ll, rr) = partitionAtMedian r xs -- ignore new threshold (?)
            tl <- loop (ixLev + 1) tl0 ll
            tr <- loop (ixLev + 1) tr0 rr
            pure $ Bin thr tl tr

      Tip xs0 -> do
        let xs' = xs <> xs0
        if ixLev >= maxDepth || length xs <= 1
          then pure $ Tip xs' -- concat data in leaf
          else
          do
            let
              r = rvs VG.! ixLev
              (thr, ll, rr) = partitionAtMedian r xs'
            tl <- loop (ixLev + 1) z ll
            tr <- loop (ixLev + 1) z rr
            pure $ Bin thr tl tr
