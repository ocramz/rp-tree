{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
-- {-# language MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-top-binds #-}
module Data.RPTree (
  -- * Construction
  forest
  -- * Query
  , knn
  -- , nearest
  -- * Validation
  , recall
  -- * Access
  , levels, points, leaves, candidates
  -- * Types
  -- ** RPTree
  , RPTree, RPForest
  -- -- *** internal
  -- , RPT
  -- -- ** RT
  -- , RT
  -- *
  , SVector, fromListSv
  , DVector, fromListDv
  -- * inner product
  , Inner(..), Scale(..)
  --   -- ** helpers for implementing Inner instances
  --   -- *** inner product
  -- , innerSS, innerSD
  --   -- *** L2 distance
  -- , metricSSL2, metricSDL2
  -- * Conduit
  , dataSource
  -- * Random generation
  -- ** vector
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
import Data.Sequence (Seq, (|>))
import qualified Data.Map as M (Map, fromList, toList, foldrWithKey, insert, insertWith)
import qualified Data.Set as S (Set, fromList, intersection, insert)
-- deepseq
import Control.DeepSeq (NFData(..))
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- -- psqueues
-- import qualified Data.IntPSQ as PQ (IntPSQ, insert, fromList, findMin, minView)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM, (!), map, freeze, thaw, take, drop, unzip)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sortBy)

import Data.RPTree.Conduit (forest, dataSource)
import Data.RPTree.Gen (sparse, dense)
import Data.RPTree.Internal (RPTree(..), RPForest, RPT(..), levels, points, leaves, RT(..), Inner(..), Scale(..), (/.), innerSD, innerSS, metricSSL2, metricSDL2, SVector(..), fromListSv, DVector(..), fromListDv, partitionAtMedian, Margin, getMargin, sortByVG)

import Data.RPTree.Draw (draw, writeCsv)


-- | k nearest neighbors
knn :: (Ord p, Inner SVector v, VU.Unbox d, Real d) =>
       (v2 -> v d -> p) -- ^ distance function
    -> Int -- ^ k neighbors
    -> RPForest d (V.Vector v2) -- ^ random projection forest
    -> v d -- ^ query point
    -> V.Vector (p, v2) -- ^ ordered in increasing distance order
knn distf k tts q = sortByVG fst cs
  where
    cs = VG.map (\x -> (x `distf` q, x)) $ VG.take k $ fold $ (`candidates` q) <$> tts


-- | average recall-at-k, computed over a set of trees
recall :: (Inner u v, Inner SVector v, VU.Unbox a, Ord a,
            Ord (u a), Floating a) =>
          RPForest a (V.Vector (u a))
       -> Int -- ^ k : number of nearest neighbors to consider
       -> v a -- ^ query point
       -> Double
recall tt k q = sum rs / fromIntegral n
  where
    rs = fmap (\t -> recall1 t k q) tt
    n = length tt

recall1 :: (Inner SVector v, Inner u v, VU.Unbox a, Ord a, Ord (u a), Floating a) =>
          RPTree a (V.Vector (u a))
       -> Int -- ^ k : number of nearest neighbors to consider
       -> v a  -- ^ query point
       -> Double
recall1 = recallWith metricL2

recallWith :: (Fractional a1, Inner SVector v, Ord d, VU.Unbox d,
                Num d, Ord a3, Ord a2) =>
              (a2 -> v d -> a3) -> RPTree d (V.Vector a2) -> Int -> v d -> a1
recallWith distf tt k q = fromIntegral (length aintk) / fromIntegral k
  where
    xs = points tt
    dists = sortBy (comparing snd) $ toList $ fmap (\x -> (x, x `distf` q)) xs
    kk = S.fromList $ map fst $ take k dists
    aa = set $ candidates tt q
    aintk = aa `S.intersection` kk

set :: (Foldable t, Ord a) => t a -> S.Set a
set = foldl (flip S.insert) mempty



-- | Retrieve points nearest to the query
--
-- in case of a narrow margin, collect both branches of the tree
candidates :: (Inner SVector v, VU.Unbox d, Ord d, Num d, Semigroup xs) =>
              RPTree d xs
           -> v d -- ^ query point
           -> xs
candidates (RPTree rvs tt) x = go 0 tt
  where
    go _     (Tip xs)                     = xs
    go ixLev (Bin thr margin ltree rtree) = do
      let
        (mglo, mghi) = getMargin margin
        r = rvs VG.! ixLev
        proj = r `inner` x
        i' = succ ixLev
      if | proj < thr &&
           mglo > mghi -> go i' ltree <> go i' rtree
         | proj < thr  -> go i' ltree
         | proj > thr &&
           mglo < mghi -> go i' ltree <> go i' rtree
         | otherwise   -> go i' rtree






-- pqSeq :: Ord a => PQ.IntPSQ a b -> Seq (a, b)
-- pqSeq pqq = go pqq mempty
--   where
--     go pq acc = case PQ.minView pq of
--       Nothing -> acc
--       Just (_, p, v, rest) -> go rest $ acc |> (p, v)


-- newtype Counts a = Counts {
--   unCounts :: M.Map a (Sum Int) } deriving (Eq, Show, Semigroup, Monoid)
-- keepCounts :: Int -- ^ keep entry iff counts are larger than this value
--            -> Counts a
--            -> [(a, Int)]
-- keepCounts thr cs = M.foldrWithKey insf mempty c
--   where
--     insf k v acc
--       | v >= thr = (k, v) : acc
--       | otherwise = acc
--     c = getSum `fmap` unCounts cs
-- counts :: (Foldable t, Ord a) => t a -> Counts a
-- counts = foldl count mempty
-- count :: Ord a => Counts a -> a -> Counts a
-- count (Counts mm) x = Counts $ M.insertWith mappend x (Sum 1) mm


-- forest :: Inner SVector v =>
--           Int -- ^ # of trees
--        -> Int -- ^ maximum tree height
--        -> Double -- ^ nonzero density of sparse projection vectors
--        -> Int -- ^ dimension of projection vectors
--        -> V.Vector (v Double) -- ^ dataset
--        -> Gen [RPTree Double (V.Vector (v Double))]
-- forest nt maxDepth pnz dim xss =
--   replicateM nt (tree maxDepth pnz dim xss)

-- -- | Build a random projection tree
-- --
-- -- Optimization: instead of sampling one projection vector per branch, we sample one per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
-- tree :: (Inner SVector v) =>
--          Int -- ^ maximum tree height
--       -> Double -- ^ nonzero density of sparse projection vectors
--       -> Int -- ^ dimension of projection vectors
--       -> V.Vector (v Double) -- ^ dataset
--       -> Gen (RPTree Double (V.Vector (v Double)))
-- tree maxDepth pnz dim xss = do
--   -- sample all projection vectors
--   rvs <- V.replicateM maxDepth (sparse pnz dim stdNormal)
--   let
--     loop ixLev xs = do
--       if ixLev >= maxDepth || length xs <= 100
--         then
--           pure $ Tip xs
--         else
--         do
--           let
--             r = rvs VG.! ixLev
--             (thr, margin, ll, rr) = partitionAtMedian r xs
--           treel <- loop (ixLev + 1) ll
--           treer <- loop (ixLev + 1) rr
--           pure $ Bin thr margin treel treer
--   rpt <- loop 0 xss
--   pure $ RPTree rvs rpt





-- -- | Partition at median inner product
-- treeRT :: (Monad m, Inner SVector v) =>
--            Int
--         -> Int
--         -> Double
--         -> Int
--         -> V.Vector (v Double)
--         -> GenT m (RT SVector Double (V.Vector (v Double)))
-- treeRT maxDepth minLeaf pnz dim xss = loop 0 xss
--   where
--     loop ixLev xs = do
--       if ixLev >= maxDepth || length xs <= minLeaf
--         then
--           pure $ RTip xs
--         else
--         do
--           r <- sparse pnz dim stdNormal
--           let
--             (_, mrg, ll, rr) = partitionAtMedian r xs
--           treel <- loop (ixLev + 1) ll
--           treer <- loop (ixLev + 1) rr
--           pure $ RBin r mrg treel treer







-- -- | Like 'tree' but here we partition at the median of the inner product values instead
-- tree' :: (Inner SVector v) =>
--          Int
--       -> Double
--       -> Int
--       -> V.Vector (v Double)
--       -> Gen (RPTree Double (V.Vector (v Double)))
-- tree' maxDepth pnz dim xss = do
--   -- sample all projection vectors
--   rvs <- V.replicateM maxDepth (sparse pnz dim stdNormal)
--   let
--     loop ixLev xs =
--       if ixLev >= maxDepth || length xs <= 100
--         then Tip xs
--         else
--           let
--             r = rvs VG.! ixLev
--             (thr, margin, ll, rr) = partitionAtMedian r xs
--             tl = loop (ixLev + 1) ll
--             tr = loop (ixLev + 1) rr
--           in Bin thr margin tl tr
--   let rpt = loop 0 xss
--   pure $ RPTree rvs rpt


-- -- | Partition uniformly at random between inner product extreme values
-- treeRT :: (Monad m, Inner SVector v) =>
--           Int -- ^ max tree depth
--        -> Int -- ^ min leaf size
--        -> Double -- ^ nonzero density
--        -> Int -- ^ embedding dimension
--        -> V.Vector (v Double) -- ^ data
--        -> GenT m (RT SVector Double (V.Vector (v Double)))
-- treeRT maxDepth minLeaf pnz dim xss = loop 0 xss
--   where
--     loop ixLev xs = do
--       if ixLev >= maxDepth || length xs <= minLeaf
--         then
--           pure $ RTip xs
--         else
--         do
--           -- sample projection vector
--           r <- sparse pnz dim stdNormal
--           let
--             -- project the dataset
--             projs = map (\x -> (x, r `inner` x)) xs
--             hi = snd $ maximumBy (comparing snd) projs
--             lo = snd $ minimumBy (comparing snd) projs
--           -- sample a threshold
--           thr <- uniformR lo hi
--           let
--             (ll, rr) = partition (\xp -> snd xp < thr) projs
--           treel <- loop (ixLev + 1) (map fst ll)
--           treer <- loop (ixLev + 1) (map fst rr)
--           pure $ RBin r treel treer


-- -- | Partition wrt a plane _|_ to the segment connecting two points sampled at random
-- --
-- -- (like annoy@@)
-- treeRT2 :: (Monad m, Ord d, Fractional d, Inner v v, VU.Unbox d, Num d) =>
--            Int
--         -> Int
--         -> [v d]
--         -> GenT m (RT v d [v d])
-- treeRT2 maxd minl xss = loop 0 xss
--   where
--     loop ixLev xs = do
--       if ixLev >= maxd || length xs <= minl
--         then
--           pure $ RTip xs
--         else
--         do
--           x12 <- sampleWOR 2 xs
--           let
--             (x1:x2:_) = x12
--             r = x1 ^-^ x2
--             (ll, rr) = partition (\x -> (r `inner` (x ^-^ x1) < 0)) xs
--           treel <- loop (ixLev + 1) ll
--           treer <- loop (ixLev + 1) rr
--           pure $ RBin r treel treer










-- ulid :: MonadIO m => a -> m (ULID a)
-- ulid x = ULID <$> pure x <*> liftIO UU.getULID
-- data ULID a = ULID { uData :: a , uULID :: UU.ULID } deriving (Eq, Show)
-- instance (Eq a) => Ord (ULID a) where
--   ULID _ u1 <= ULID _ u2 = u1 <= u2
