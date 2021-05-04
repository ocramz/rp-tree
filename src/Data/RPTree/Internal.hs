{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Internal where

import Data.Foldable (fold)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.ST (runST)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Ord (comparing)
import GHC.Generics (Generic)

-- deepseq
import Control.DeepSeq (NFData(..))
-- microlens
import Lens.Micro (Traversal', (.~), (^..), folded)
import Lens.Micro.TH (makeLensesFor, makeLensesWith, lensRules, generateSignatures)
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), map, sum, unfoldr, unfoldrM, length, replicateM, (!), take, drop, unzip, freeze, thaw, foldl, foldr, toList)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList, toList)
import qualified Data.Vector.Storable as VS (Vector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sortBy)




-- | Sparse vectors with unboxed components
data SVector a = SV { svDim :: !Int, svVec :: VU.Vector (Int, a) } deriving (Eq, Ord, Generic)
instance (VU.Unbox a, Show a) => Show (SVector a) where
  show (SV n vv) = unwords ["SV", show n, show (VU.toList vv)]
instance NFData (SVector a)

fromListSv :: VU.Unbox a => Int -> [(Int, a)] -> SVector a
fromListSv n ll = SV n $ VU.fromList ll

-- | Dense vectors with unboxed components
newtype DVector a = DV { dvVec :: VU.Vector a } deriving (Eq, Ord, Generic)
instance (VU.Unbox a, Show a) => Show (DVector a) where
  show (DV vv) = unwords ["DV", show (VU.toList vv)]

fromListDv :: VU.Unbox a => [a] -> DVector a
fromListDv ll = DV $ VU.fromList ll
toListDv :: (VU.Unbox a) => DVector a -> [a]
toListDv (DV v) = VU.toList v

-- | Internal
--
-- one projection vector per node (like @annoy@)
data RT d a =
  RBin (SVector d) (RT d a) (RT d a)
  | RTip { _rData :: a } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
makeLensesFor [("_rData", "rData")] ''RT
instance (NFData v, NFData a) => NFData (RT v a)


-- | Internal
--
-- one projection vector per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
data RPT d a =
  Bin {
  _rpThreshold :: !d
  , _rpL :: !(RPT d a)
  , _rpR :: !(RPT d a) }
  | Tip { _rpData :: a }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
makeLensesFor [("_rpData", "rpData")] ''RPT
instance (NFData v, NFData a) => NFData (RPT v a)

-- | Random projection trees
--
-- The first type parameter corresponds to a floating point scalar value, the second is the type of the data collected at the leaves of the tree (e.g. lists of vectors)
--
-- We keep them separate to leverage the Functor instance for postprocessing and visualization
data RPTree d a = RPTree {
  _rpVectors :: V.Vector (SVector d) -- ^ one random projection vector per tree level
  , _rpTree :: RPT d a
                         } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
makeLensesFor [("_rpTree", "rpTree")] ''RPTree
instance (NFData a, NFData d) => NFData (RPTree d a)

rpTreeData :: Traversal' (RPTree d a) a
rpTreeData = rpTree . rpData

leaves :: RPTree d a -> [a]
leaves = (^.. rpTreeData)

-- | Number of tree levels
levels :: RPTree d a -> Int
levels (RPTree v _) = VG.length v

-- | Set of data points used to construct the index
points :: Monoid m => RPTree d m -> m
points (RPTree _ t) = fold t

-- points in 2d
data P a = P !a !a deriving (Eq, Show)

-- | Inner product spaces
--
-- This typeclass is provided as a convenience for library users to interface their own vector types.
class Inner u v where
  inner :: (VU.Unbox a, Num a) => u a -> v a -> a
  metricL2 :: (VU.Unbox a, Floating a) => u a -> v a -> a
instance Inner SVector SVector where
  inner (SV _ v1) (SV _ v2) = innerSS v1 v2
  metricL2 (SV _ v1) (SV _ v2) = metricSSL2 v1 v2
instance Inner SVector VU.Vector where
  inner (SV _ v1) v2 = innerSD v1 v2
  metricL2 (SV _ v1) v2 = metricSDL2 v1 v2
instance Inner SVector DVector where
  inner (SV _ v1) (DV v2) = innerSD v1 v2
  metricL2 (SV _ v1) (DV v2) = metricSDL2 v1 v2


-- | sparse-sparse inner product
innerSS :: (VG.Vector u (Int, a), VG.Vector v (Int, a), VU.Unbox a, Num a) =>
           u (Int, a) -> v (Int, a) -> a
innerSS vv1 vv2 = go 0 0
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
innerSD :: (Num a, VG.Vector u (Int, a), VG.Vector v a, VU.Unbox a) =>
           u (Int, a) -> v a -> a
innerSD vv1 vv2 = go 0
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



-- | Vector distance induced by the L2 norm (sparse-sparse)
metricSSL2 :: (Floating a, VG.Vector v a, VU.Unbox a, VG.Vector u (Int, a), VG.Vector v (Int, a)) =>
              u (Int, a) -> v (Int, a) -> a
metricSSL2 u v = sqrt $ VG.sum $ VG.map (\(_, x) -> x ** 2) duv
  where
    duv = u `diffSS` v

-- | Vector distance induced by the L2 norm (sparse-dense)
metricSDL2 :: (Floating a, VU.Unbox a, VG.Vector u (Int, a), VG.Vector v a) =>
              u (Int, a)
           -> v a -> a
metricSDL2 u v = sqrt $ VG.sum $ VG.map (** 2) duv
  where
    duv = u `diffSD` v


diffSD :: (VG.Vector u (Int, a), VG.Vector v a, VU.Unbox a, Num a) =>
          u (Int, a) -> v a -> v a
diffSD = binSD (-)

-- -- | Vector difference
-- (.-.) :: (VU.Unbox a, Num a) => SVector a -> SVector a -> SVector a
diffSS :: (VG.Vector u (Int, a), VG.Vector v (Int, a), VU.Unbox a, Num a) =>
          u (Int, a) -> v (Int, a) -> v (Int, a)
diffSS = binSS (-) 0

-- | Binary operation on 'SVector' s
binSS :: (VG.Vector u (Int, a), VG.Vector v (Int, a), VU.Unbox a) =>
         (a -> a -> a) -> a -> u (Int, a) -> v (Int, a) -> v (Int, a)
binSS f z vv1 vv2 = VG.unfoldr go (0, 0)
  where
    nz1 = VG.length vv1
    nz2 = VG.length vv2
    go (i1, i2)
      | i1 >= nz1 || i2 >= nz2 = Nothing
      | otherwise =
          let
            (il, xl) = vv1 VG.! i1
            (ir, xr) = vv2 VG.! i2
          in case il `compare` ir of
            EQ -> Just ((il, f xl xr), (succ i1, succ i2))
            LT -> Just ((il, f xl z ), (succ i1, i2     ))
            GT -> Just ((ir, f z  xr), (i1     , succ i2))



binSD :: (VG.Vector u (Int, a), VG.Vector v a, VU.Unbox a) =>
         (a -> a -> a) -> u (Int, a) -> v a -> v a
binSD f vv1 vv2 = VG.unfoldr go 0
  where
    nz1 = VG.length vv1
    nz2 = VG.length vv2
    go i
      | i >= nz1 || i >= nz2 = Nothing
      | otherwise = Just (y, succ i)
          where
            (il, xl) = vv1 VG.! i
            xr       = vv2 VG.! il
            y = f xl xr



sortByVG :: (VG.Vector v a, Ord b) => (a -> b) -> v a -> v a
sortByVG f v = runST $ do
  vm <- VG.thaw v
  V.sortBy (comparing f) vm
  VG.freeze vm

partitionAtMedian :: (Ord a, Inner u v, VU.Unbox a, Num a) =>
                     u a -- ^ projection vector
                  -> V.Vector (v a) -- ^ dataset
                  -> (a, V.Vector (v a), V.Vector (v a)) -- ^ median, smaller, larger
partitionAtMedian r xs =
  (thr, VG.take n xs', VG.drop n xs')
  where
    thr = inns VG.! n
    n = VG.length xs `div` 2
    projs = sortByVG snd $ VG.map (\x -> (x, r `inner` x)) xs
    (xs', inns) = VG.unzip projs



-- -- | Label a value with a unique identifier
-- -- labelId
-- newtype LabelT m a = LabelT {unLabelT :: StateT Integer m a} deriving (Functor, Applicative, Monad, MonadState Integer, MonadIO)
-- type Label = LabelT Identity
-- runLabelT :: (Monad m) => LabelT m a -> m a
-- runLabelT = flip evalStateT 0 . unLabelT
-- label :: Monad m => a -> LabelT m (Id a)
-- label x = LabelT $ do { i <- get ; put (i + 1); pure (Id x i)}
-- data Id a = Id { _idD :: a , _idL :: !Integer } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
-- instance NFData a => NFData (Id a)
-- makeLensesFor [("_idD", "idD")] ''Id
-- instance (Eq a) => Ord (Id a) where
--   Id _ u1 <= Id _ u2 = u1 <= u2
