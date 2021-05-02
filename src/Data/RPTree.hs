{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
-- {-# language MultiParamTypeClasses #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree (
  -- * Construction
  build
  -- * Query
  , nearest
  -- * Access
  , levels, points
  -- * Types
  , RPTree
  -- *
  , SVector, fromList
  -- * inner product
  , Inner(..)
    -- ** helpers for implementing Inner instances
    -- *** inner product
  , innerSS, innerSD
    -- *** L2 distance
  , metricSSL2, metricSDL2
  -- * Random generation
  -- ** pure
  , Gen, evalGen
  -- ** monadic
  , GenT, evalGenT
  -- ** scalar distributions
  , bernoulli, normal, stdNormal, uniformR, stdUniform, exponential
  -- ** multivariate
  , sparse
  ) where

import Control.Monad (replicateM)
import Data.Foldable (Foldable(..), maximumBy, minimumBy)
import Data.List (partition, sortBy)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import GHC.Word (Word64)

-- containers
-- import Data.Sequence (Seq, (|>))
import qualified Data.Set as S (Set, fromList, intersection)
-- deepseq
import Control.DeepSeq (NFData(..))
-- erf
import Data.Number.Erf (InvErf(..))
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- splitmix
import System.Random.SplitMix (SMGen, mkSMGen, nextInt, nextInteger, nextDouble)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- ulid
import qualified Data.ULID as UU (ULID, getULID)
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM)
import qualified Data.Vector.Generic as VG ((!))
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

import Data.RPTree.Gen (Gen, evalGen, GenT, evalGenT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR, sparse)
import Data.RPTree.Internal (RPTree(..), RPT(..), levels, points, Inner(..), innerSD, innerSS, metricSSL2, metricSDL2, SVector(..), fromList)

import Data.RPTree.Draw (draw)

-- ^ recall-at-k
recall :: (Inner SVector v, Inner u v, VU.Unbox a, Ord a, Ord (u a), Floating a) =>
          RPTree a [u a]
       -> Int -- ^ k : number of nearest neighbors to consider
       -> v a  -- ^ query point
       -> Double
recall = recallWith metricL2


recallWith :: (Fractional a1, Inner SVector v, VU.Unbox d, Ord d, Ord a3,
               Ord a2, Num d) =>
              (a2 -> v d -> a3) -> RPTree d [a2] -> Int -> v d -> a1
recallWith distf tt k q = fromIntegral (length aintk) / fromIntegral k
  where
    xs = points tt
    dists = sortBy (comparing snd) $ map (\x -> (x, x `distf` q)) xs
    kk = S.fromList $ map fst $ take k dists
    aa = S.fromList $ nearest tt q
    aintk = aa `S.intersection` kk


-- | Build a random projection tree
--
-- Optimization: instead of sampling one random vector per branch, we sample one per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
build :: (Inner SVector v) =>
         Int -- ^ maximum tree depth
      -> Double -- ^ nonzero density of sparse projection vectors
      -> Int -- ^ dimension of projection vectors
      -> [v Double] -- ^ dataset
      -> Gen (RPTree Double [v Double])
build maxDepth pnz dim xss = do
  -- sample all projection vectors
  rvs <- V.replicateM maxDepth (sparse pnz dim stdNormal)
  let
    loop xs = do
      ixLev <- get
      if ixLev >= maxDepth || length xs <= 1
        then
        pure $ Tip xs
        else
        do
          let
            r = rvs VG.! ixLev
            -- project the dataset
            projs = map (\x -> (x, r `inner` x)) xs
            hi = snd $ maximumBy (comparing snd) projs
            lo = snd $ minimumBy (comparing snd) projs
          -- sample a threshold
          thr <- lift $ uniformR lo hi
          let
            (ll, rr) = partition (\xp -> snd xp < thr) projs
          put (ixLev + 1)
          treel <- loop (map fst ll)
          treer <- loop (map fst rr)
          pure $ Bin thr treel treer
  (rpt, _) <- flip runStateT 0 $ loop xss
  pure $ RPTree rvs rpt


-- | Retrieve points nearest to the query
nearest :: (Inner SVector v, Ord d, VU.Unbox d, Num d) =>
           RPTree d xs
        -> v d -- ^ query point
        -> xs
nearest (RPTree rvs tt) x = flip evalState 0 $ go tt
  where
    go (Tip xs) = pure xs
    go (Bin thr ll rr) = do
      ixLev <- get
      let
        r = rvs VG.! ixLev
      put (ixLev + 1)
      if r `inner` x < thr
        then go ll
        else go rr


-- build maxDepth pnz dim xss = do
--   (rpt, (_, rs)) <- flip runStateT (0, mempty) $ loop xss
--   pure $ RPTree rs rpt
--   where
--     loop xs = do
--       (ixLev, rAcc) <- get
--       if ixLev >= maxDepth
--         then
--         pure $ Tip xs
--         else
--         do
--           -- sample a projection vector
--           r <- lift $ sparse pnz dim stdNormal
--           let
--             -- project the dataset
--             projs = map (\x -> (x, innerS r x)) xs
--             hi = snd $ maximumBy (comparing snd) projs
--             lo = snd $ minimumBy (comparing snd) projs
--           -- sample a threshold
--           thr <- lift $ uniformR lo hi
--           let
--             (ll, rr) = partition (\xp -> snd xp < thr) projs
--             rAcc' = rAcc |> r
--           put (ixLev + 1, rAcc')
--           tl <- loop $ map fst ll
--           tr <- loop $ map fst rr
--           pure $ Bin thr tl tr




