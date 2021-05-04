{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Gen where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import GHC.Word (Word64)

-- erf
import Data.Number.Erf (InvErf(..))
-- mtl
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.State (MonadState(..), modify)
-- splitmix
import System.Random.SplitMix (SMGen, mkSMGen, splitSMGen, nextInt, nextInteger, nextDouble)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM)
import qualified Data.Vector.Generic as VG ((!))
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

import Data.RPTree.Internal (RPTree(..), RPT(..), SVector(..), fromListSv, DVector(..))

-- | Generate a sparse random vector with a given nonzero density and components sampled from the supplied random generator
sparse :: (Monad m, VU.Unbox a) =>
          Double -- ^ nonzero density
       -> Int -- ^ size
       -> GenT m a -- ^ random generator of vector components
       -> GenT m (SVector a)
sparse p sz rand = SV sz <$> sparseVG p sz rand

dense :: (Monad m, VG.Vector VU.Vector a) =>
         Int -> GenT m a -> GenT m (DVector a)
dense sz rand = DV <$> denseVG sz rand

-- | Random generator
--
-- wraps 'splitmix' state-passing inside a 'StateT' monad
newtype GenT m a = GenT { unGen :: StateT SMGen m a } deriving (Functor, Applicative, Monad, MonadState SMGen, MonadTrans, MonadIO)

type Gen = GenT Identity

-- | Monadic evaluation
evalGenT :: Monad m =>
            Word64 -- ^ random seed
         -> GenT m a -> m a
evalGenT seed gg = evalStateT (unGen gg) (mkSMGen seed)

-- | Pure evaluation
evalGen :: Word64 -- ^ random seed
        -> Gen a
        -> a
evalGen seed gg = evalState (unGen gg) (mkSMGen seed)

-- | Sample a dense random vector
denseVG :: (VG.Vector v a, Monad m) =>
           Int -- ^ vector dimension
        -> m a
        -> m (v a)
denseVG sz rand = VG.unfoldrM mkf 0
  where
    mkf i
      | i >= sz = pure Nothing
      | otherwise = do
          x <- rand
          pure $ Just (x, succ i)

-- | Sample a sparse random vector
sparseVG :: (Monad m, VG.Vector v (Int, a)) =>
            Double -- ^ nonzero density
         -> Int  -- ^ vector dimension
         -> GenT m a
         -> GenT m (v (Int, a))
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

-- | Bernoulli trial
bernoulli :: Monad m => Double -> GenT m Bool
bernoulli p = withGen (bernoulliF p)

-- | Uniform between two values
uniformR :: Monad m =>
            Double -- ^ low
         -> Double -- ^ high
         -> GenT m Double
uniformR lo hi = scale <$> stdUniform
  where
    scale x = x * (hi - lo) + lo

-- | Discrete uniform between two values
uniformRI :: (Monad m, Integral i) =>
             Integer -- ^ low
          -> Integer -- ^ high
          -> GenT m i
uniformRI lo hi = fromIntegral <$> withGen (nextInteger lo hi)

-- | Standard normal
stdNormal :: Monad m => GenT m Double
stdNormal = normal 0 1

-- | Uniform in [0, 1)
stdUniform :: Monad m => GenT m Double
stdUniform = withGen nextDouble

-- | Beta distribution, from two standard uniform samples
beta :: Monad m => Double -> Double -> GenT m Double
beta a b = go
  where
    go = do
      (y1, y2) <- sample
      if
        y1 + y2 <= 1
        then pure (y1 / (y1 + y2))
        else go
    sample = f <$> stdUniform <*> stdUniform
      where
        f u1 u2 = (u1 ** (1/a), u2 ** (1/b))

-- | Normal distribution
normal :: Monad m => Double -- ^ mean
       -> Double -- ^ std.dev.
       -> GenT m Double
normal mu sig = withGen (normalF mu sig)

-- | Exponential distribution
exponential :: Monad m => Double -- ^ rate parameter
            -> GenT m Double
exponential l = withGen (exponentialF l)

-- | Wrap a 'splitmix' PRNG function
withGen :: Monad m =>
           (SMGen -> (a, SMGen))
        -> GenT m a
withGen f = GenT $ do
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
