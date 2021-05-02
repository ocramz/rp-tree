{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Gen where

import GHC.Word (Word64)

-- erf
import Data.Number.Erf (InvErf(..))
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- splitmix
import System.Random.SplitMix (SMGen, mkSMGen, nextInt, nextInteger, nextDouble)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM)
import qualified Data.Vector.Generic as VG ((!))
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

import Data.RPTree.Internal (RPTree(..), RPT(..), SVector(..), fromList)

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

normal :: Double -- ^ mean
       -> Double -- ^ std.dev.
       -> Gen Double
normal mu sig = withGen (normalF mu sig)

exponential :: Double -- ^ rate parameter
            -> Gen Double
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
