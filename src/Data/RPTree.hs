{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree (
  build
  , RPTree
  -- *
  , SVector, fromList
  , InnerS(..)
  -- * random generation
  , Gen, evalGen
  -- ** distributions
  , bernoulli, normal, stdNormal, uniformR, stdUniform, exponential
  ) where

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
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length)
import qualified Data.Vector.Generic as VG ((!))
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

data RPT v a =
  Bin {
  _rpThreshold :: ! a
  , _rpL :: !(RPT v a)
  , _rpR :: !(RPT v a) }
  | Tip [v a]
  deriving (Eq, Show, Generic, Functor)
instance (NFData a, NFData (v a)) => NFData (RPT v a)

data RPTree v a = RPTree {
  _rpVectors :: Seq (SVector a) -- ^ one random projection vector per tree level
  , _rpTree :: RPT v a
                         } deriving (Eq, Show, Generic)
instance (NFData a, NFData (v a)) => NFData (RPTree v a)

-- | Build a random projection tree
build :: (InnerS v Double) =>
         Int -- ^ maximum tree depth
      -> Double -- ^ nonzero density of sparse projection vectors
      -> Int -- ^ dimension of projection vectors
      -> [v Double] -- ^ dataset
      -> Gen (RPTree v Double)
build maxDepth pnz dim xss = do
  (rpt, (_, rs)) <- flip runStateT (0, mempty) $ loop xss
  pure $ RPTree rs rpt
  where
    loop xs = do
      (ixLev, rAcc) <- get
      if ixLev >= maxDepth
        then
        pure $ Tip xs
        else
        do
          r <- lift $ sparse pnz dim stdNormal
          let
            projs = map (\x -> (x, innerS r x)) xs
            hi = snd $ maximumBy (comparing snd) projs
            lo = snd $ minimumBy (comparing snd) projs
          thr <- lift $ uniformR lo hi
          let
            (ll, rr) = partition (\xp -> snd xp < thr) projs
            rAcc' = rAcc |> r
          put (ixLev + 1, rAcc')
          tl <- loop $ map fst ll
          tr <- loop $ map fst rr
          pure $ Bin thr tl tr





-- | Inner product between a sparse vector and another type of vector
class InnerS v a where
  innerS :: SVector a -> v a -> a

instance (VU.Unbox a, Num a) => InnerS SVector a where
  innerS sv1 (SV _ sv2) = innerSS sv1 sv2
instance (VU.Unbox a, Num a) => InnerS VU.Vector a where
  innerS = innerSD

-- | Sparse vectors with unboxed components
data SVector a = SV { svDim :: !Int, svVec :: VU.Vector (Int, a) } deriving (Eq, Show, Generic)
instance NFData (SVector a)

fromList :: VU.Unbox a => Int -> [(Int, a)] -> SVector a
fromList n ll = SV n $ VU.fromList ll

-- | sparse-sparse inner product
innerSS :: (VG.Vector v (Int, a), VU.Unbox a, Num a) =>
           SVector a -> v (Int, a) -> a
innerSS (SV _ vv1) vv2 = go 0 0
  where
    nz1 = VG.length vv1
    nz2 = VG.length vv2
    go i1 i2
      | i1 >= nz1 || i2 >= nz2 = 0
      | otherwise =
          let
            (il, xl) = vv1 VG.! i1
            (ir, xr) = vv2 VG.! i2
          in case il `compare` ir of
            EQ -> (xl * xr +) $ go (succ i1) (succ i2)
            LT -> go (succ i1) i2
            GT -> go i1 (succ i2)

-- | sparse-dense inner product
innerSD :: (Num a, VG.Vector v a, VU.Unbox a) =>
           SVector a -> v a -> a
innerSD (SV _ vv1) vv2 = go 0
  where
    nz1 = VG.length vv1
    nz2 = VG.length vv2
    go i
      | i >= nz1 || i >= nz2 = 0
      | otherwise =
          let
            (il, xl) = vv1 VG.! i
            xr       = vv2 VG.! il
          in
            (xl * xr +) $ go (succ i)

-- | Generate a sparse vector with a given nonzero density and components sampled from the supplied random generator
sparse :: VU.Unbox a =>
          Double -- ^ nonzero density
       -> Int -- ^ size
       -> Gen a -- ^ random generator of vector components
       -> Gen (SVector a)
sparse p sz rand = SV sz <$> sparseVG p sz rand

-- | Pure random generator
--
-- wraps 'splitmix' state passing inside a 'State' monad
newtype Gen a = Gen { unGen :: State SMGen a } deriving (Functor, Applicative, Monad, MonadState SMGen)

class MonadGen m where
  liftGen :: Gen a -> m a


evalGen :: Word64 -- ^ random seed
        -> Gen a
        -> a
evalGen seed gg = evalState (unGen gg) (mkSMGen seed)



sparseVG :: (VG.Vector v (Int, a)) =>
            Double -> Int -> Gen a -> Gen (v (Int, a))
sparseVG p sz rand = VG.unfoldrM mkf 0
  where
    mkf i
      | i >= sz = pure Nothing
      | otherwise = do
          flag <- bernoulli p
          if flag
            then
            do
              x <- rand
              pure $ Just ((i, x), succ i)
            else
              mkf (succ i)

bernoulli :: Double -> Gen Bool
bernoulli p = withGen (bernoulliF p)

uniformR :: Double -- ^ low
         -> Double -- ^ high
         -> Gen Double
uniformR lo hi = scale <$> stdUniform
  where
    scale x = x * (hi - lo) + lo

-- standard normal
stdNormal :: Gen Double
stdNormal = normal 0 1

stdUniform :: Gen Double
stdUniform = withGen nextDouble

normal :: Double -> Double -> Gen Double
normal mu sig = withGen (normalF mu sig)

exponential :: Double -> Gen Double
exponential l = withGen (exponentialF l)

withGen :: (SMGen -> (a, SMGen)) -> Gen a
withGen f = Gen $ do
  gen <- get
  let
    (b, gen') = f gen
  put gen'
  pure b

exponentialF :: Double -> SMGen -> (Double, SMGen)
exponentialF l g = (exponentialICDF l x, g') where (x, g') = nextDouble g

normalF :: Double -> Double -> SMGen -> (Double, SMGen)
normalF mu sig g = (normalICDF mu sig x, g') where (x, g') = nextDouble g

bernoulliF :: Double -> SMGen -> (Bool, SMGen)
bernoulliF p g = (x < p , g') where (x, g') = nextDouble g


-- | inverse CDF of normal rv
normalICDF :: InvErf a =>
              a -- ^ mean
           -> a -- ^ std dev
           -> a -> a
normalICDF mu sig p = mu + sig * sqrt 2 * inverf (2 * p - 1)

-- | inverse CDF of exponential rv
exponentialICDF :: Floating a =>
                   a -- ^ rate
                -> a -> a
exponentialICDF l p = (- 1 / l) * log (1 - p)
