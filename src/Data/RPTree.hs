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
  -- * Types
  , RPTree
  -- *
  , SVector, fromList
  -- * inner product
  , InnerS(..),
    -- ** helpers for implementing InnerS instances
    innerSS, innerSD
  -- * random generation
  , Gen, evalGen
  -- ** distributions
  , bernoulli, normal, stdNormal, uniformR, stdUniform, exponential
  ) where

import Control.Monad (replicateM)
import Data.Foldable (Foldable(..), maximumBy, minimumBy)
import Data.List (partition)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import GHC.Word (Word64)

-- containers
import Data.Sequence (Seq, (|>))
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
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM)
import qualified Data.Vector.Generic as VG ((!))
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

import Data.RPTree.Gen (Gen, evalGen, normal, stdNormal, stdUniform, exponential, bernoulli, uniformR, sparse)
import Data.RPTree.Internal (RPTree(..), RPT(..), InnerS(..), innerSD, innerSS, SVector(..), fromList)

import Data.RPTree.Draw (draw)


-- | Build a random projection tree
--
-- Optimization: instead of sampling one random vector per branch, we sample one per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
build :: (InnerS v) =>
         Int -- ^ maximum tree depth
      -> Double -- ^ nonzero density of sparse projection vectors
      -> Int -- ^ dimension of projection vectors
      -> [v Double] -- ^ dataset
      -> Gen (RPTree v Double)
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
            projs = map (\x -> (x, innerS r x)) xs
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
nearest :: (InnerS v, VU.Unbox a, Ord a, Num a) =>
           RPTree u a
        -> v a -- ^ query point
        -> [u a]
nearest (RPTree rvs tt) x = flip evalState 0 $ go tt
  where
    go (Tip xs) = pure xs
    go (Bin thr ll rr) = do
      ixLev <- get
      let
        r = rvs VG.! ixLev
      put (ixLev + 1)
      if r `innerS` x < thr
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




-- test data

tt0 :: RPTree P Double
tt0 = evalGen 1337 $ build 5 0.5 2 (genDataset 6 2)

genDataset :: Int -> Int -> [P Double]
genDataset m d = evalGen 1234 $ replicateM m (genP d)

genP :: Int -> Gen (P Double)
genP d = P <$> VG.replicateM d stdNormal

newtype P a = P (VU.Vector a) deriving (Eq, Show)
instance InnerS P where
  innerS sv1 (P v) = innerSD sv1 v
