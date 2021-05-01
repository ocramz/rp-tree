{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Internal where

import GHC.Generics (Generic)

-- deepseq
import Control.DeepSeq (NFData(..))
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), unfoldrM, length, replicateM)
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
  _rpVectors :: V.Vector (SVector a) -- ^ one random projection vector per tree level
  , _rpTree :: RPT v a
                         } deriving (Eq, Show, Generic)
instance (NFData a, NFData (v a)) => NFData (RPTree v a)


class InnerS v where
  innerS :: (VU.Unbox a, Num a) => SVector a -> v a -> a
instance InnerS SVector where
  innerS sv1 (SV _ sv2) = innerSS sv1 sv2
instance InnerS VU.Vector where
  innerS = innerSD

-- -- | Inner product between a sparse vector and another type of vector
-- class InnerS v a where
--   innerS :: SVector a -> v a -> a

-- instance (VU.Unbox a, Num a) => InnerS SVector a where
--   innerS sv1 (SV _ sv2) = innerSS sv1 sv2
-- instance (VU.Unbox a, VG.Vector v a, Num a) => InnerS v a where
--   innerS = innerSD

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
