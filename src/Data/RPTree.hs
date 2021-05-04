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
  tree
  , tree' 
  , forest
  -- * Query
  , nearest
  -- * Validation
  , recall
  -- * Access
  , levels, points, leaves, getLeaf
  -- * Types
  , RPTree
  -- *
  , SVector, fromListSv
  , DVector, fromListDv
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
  , sparse, dense
  -- * Rendering
  , draw
  -- * CSV
  , writeCsv
  ) where

import Control.Monad (replicateM)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (Foldable(..), maximumBy, minimumBy)
import Data.Functor.Identity (Identity(..))
import Data.List (partition, sortBy)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import GHC.Generics (Generic)
import GHC.Word (Word64)

-- containers
-- import Data.Sequence (Seq, (|>))
import qualified Data.Map as M (Map, fromList, toList, foldrWithKey, insert, insertWith)
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
-- -- ulid
-- import qualified Data.ULID as UU (ULID, getULID)
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!), map, freeze, thaw, take, drop, unzip)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sortBy)

import Data.RPTree.Gen (Gen, evalGen, GenT, evalGenT, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR, sparse, dense)
import Data.RPTree.Internal (RPTree(..), RPT(..), levels, points, leaves, Inner(..), innerSD, innerSS, metricSSL2, metricSDL2, SVector(..), fromListSv, DVector(..), fromListDv, partitionAtMedian)

import Data.RPTree.Draw (draw, writeCsv)


newtype Counts a = Counts {
  unCounts :: M.Map a (Sum Int) } deriving (Eq, Show, Semigroup, Monoid)
keepCounts :: Int -- ^ keep entry iff counts are larger than this value
           -> Counts a
           -> [(a, Int)]
keepCounts thr cs = M.foldrWithKey insf mempty c
  where
    insf k v acc
      | v >= thr = (k, v) : acc
      | otherwise = acc
    c = getSum `fmap` unCounts cs
counts :: (Foldable t, Ord a) => t a -> Counts a
counts = foldl count mempty
count :: Ord a => Counts a -> a -> Counts a
count (Counts mm) x = Counts $ M.insertWith mappend x (Sum 1) mm

-- | Approximate the set of points nearest to the query by voting search
--
-- A point is retained if it appears in more than k trees
nearest :: (Inner SVector v, Ord d, VU.Unbox d, Num d, Ord a,
             Foldable f, Functor f, Foldable t) =>
           Int -- ^ counting threshold k
        -> f (RPTree d (t a)) -- ^ forest
        -> v d -- ^ query
        -> [(a, Int)]
nearest thr tts q = keepCounts thr ks
  where
    bkts = fmap (`getLeaf` q) tts
    ks = foldMap counts bkts




-- | average recall-at-k, computed over a set of trees
recall :: (Foldable t, Functor t, Inner u v, Inner SVector v,
            VU.Unbox a, Ord a, Ord (u a), Floating a) =>
          t (RPTree a [u a])
       -> Int -- ^ k : number of nearest neighbors to consider
       -> v a -- ^ query point
       -> Double
recall tt k q = sum rs / fromIntegral n
  where
    rs = fmap (\t -> recall1 t k q) tt
    n = length tt

recall1 :: (Inner SVector v, Inner u v, VU.Unbox a, Ord a, Ord (u a), Floating a) =>
          RPTree a [u a]
       -> Int -- ^ k : number of nearest neighbors to consider
       -> v a  -- ^ query point
       -> Double
recall1 = recallWith metricL2


recallWith :: (Fractional a1, Inner SVector v, VU.Unbox d, Ord d, Ord a3,
               Ord a2, Num d) =>
              (a2 -> v d -> a3) -> RPTree d [a2] -> Int -> v d -> a1
recallWith distf tt k q = fromIntegral (length aintk) / fromIntegral k
  where
    xs = points tt
    dists = sortBy (comparing snd) $ map (\x -> (x, x `distf` q)) xs
    kk = S.fromList $ map fst $ take k dists
    aa = S.fromList $ getLeaf tt q
    aintk = aa `S.intersection` kk

type RPForest d a = [RPTree d a]

forest :: Inner SVector v =>
          Int -- ^ # of trees
       -> Int -- ^ maximum tree height
       -> Double -- ^ nonzero density of sparse projection vectors
       -> Int -- ^ dimension of projection vectors
       -> [v Double] -- ^ dataset
       -> Gen [RPTree Double [v Double]]
forest nt maxDepth pnz dim xss =
  replicateM nt (tree maxDepth pnz dim xss)

-- | Build a random projection tree
--
-- points are partitioned by _sampling_ a threshold uniformly between the minimum and maximum of the inner products
--
-- Optimization: instead of sampling one random vector per branch, we sample one per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
tree :: (Inner SVector v) =>
         Int -- ^ maximum tree height
      -> Double -- ^ nonzero density of sparse projection vectors
      -> Int -- ^ dimension of projection vectors
      -> [v Double] -- ^ dataset
      -> Gen (RPTree Double [v Double])
tree maxDepth pnz dim xss = do
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

-- | Like 'tree' but here we partition at the median of the inner product values instead
tree' :: (Inner SVector v) =>
         Int
      -> Double
      -> Int
      -> [v Double]
      -> Gen (RPTree Double (V.Vector (v Double)))
tree' maxDepth pnz dim xss = do
  -- sample all projection vectors
  rvs <- V.replicateM maxDepth (sparse pnz dim stdNormal)
  let
    loop ixLev xs =
      if ixLev >= maxDepth || length xs <= 100
        then Tip xs
        else
          let
            r = rvs VG.! ixLev
            (thr, ll, rr) = partitionAtMedian r xs
            tl = loop (ixLev + 1) ll
            tr = loop (ixLev + 1) rr
          in Bin thr tl tr
  let rpt = loop 0 (V.fromList xss)
  pure $ RPTree rvs rpt







-- | Retrieve points nearest to the query
getLeaf :: (Inner SVector v, Ord d, VU.Unbox d, Num d) =>
           RPTree d xs
        -> v d -- ^ query point
        -> xs
getLeaf (RPTree rvs tt) x = flip evalState 0 $ go tt
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






-- ulid :: MonadIO m => a -> m (ULID a)
-- ulid x = ULID <$> pure x <*> liftIO UU.getULID
-- data ULID a = ULID { uData :: a , uULID :: UU.ULID } deriving (Eq, Show)
-- instance (Eq a) => Ord (ULID a) where
--   ULID _ u1 <= ULID _ u2 = u1 <= u2
