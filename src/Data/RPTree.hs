{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree where

import Data.Foldable (Foldable(..))
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
import Control.Monad.Trans.State (State, runState, evalState)
-- vector
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length)
import qualified Data.Vector.Generic as VG ((!))
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList)
import qualified Data.Vector.Storable as VS (Vector)

data RPTree v a =
  Bin {
  _rpVector :: !(v a)
  , _rpThreshold :: ! a
  , _rpL :: !(RPTree v a)
  , _rpR :: !(RPTree v a) }
  | Tip [a]
  deriving (Eq, Show, Generic, Functor)
instance (NFData a, NFData (v a)) => NFData (RPTree v a)

empty :: RPTree v a
empty = Tip mempty





class Inner v where
  inner :: SVector a -> v a -> a

-- | Sparse vectors with unboxed components
data SVector a = SV { svDim :: !Int, svVector :: VU.Vector (Int, a) } deriving (Eq, Show)

fromList :: VU.Unbox a => Int -> [(Int, a)] -> SVector a
fromList n ll = SV n $ VU.fromList ll

innerSv :: (Num a, VU.Unbox a) => SVector a -> SVector a -> a
innerSv (SV _ vv1) (SV _ vv2) = go 0 0
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

normal :: Double -> Double -> Gen Double
normal mu sig = withGen (normalF mu sig)

withGen :: (SMGen -> (a, SMGen)) -> Gen a
withGen f = Gen $ do
  gen <- get
  let
    (b, gen') = f gen
  put gen'
  pure b


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
