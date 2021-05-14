{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE UndecidableInstances #-}
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

import Control.Exception (Exception(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.ST (runST)
import Data.Function ((&))
import Data.Foldable (fold, foldl')
import Data.Functor.Identity (Identity(..))
import Data.List (nub)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import Data.Semigroup (Min(..), Max(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- containers
import qualified Data.IntMap.Strict as IM (IntMap)
-- deepseq
import Control.DeepSeq (NFData(..))
-- microlens
import Lens.Micro (Traversal', (.~), (^..), folded)
import Lens.Micro.TH (makeLensesFor, makeLensesWith, lensRules, generateSignatures)
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- serialise
import Codec.Serialise (Serialise(..))
-- transformers
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT, State, runState, evalState)
-- vector
import qualified Data.Vector as V (Vector, replicateM, fromList)
import qualified Data.Vector.Generic as VG (Vector(..), map, sum, unfoldr, unfoldrM, length, replicateM, (!), take, drop, unzip, freeze, thaw, foldl, foldr, toList, zipWith, last, head)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList, toList)
import qualified Data.Vector.Storable as VS (Vector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sortBy)


-- | Exceptions
data RPTError =
  EmptyResult String
  deriving (Eq, Typeable)
instance Show RPTError where
  show = \case
    EmptyResult str -> unwords [str, ": empty result"]
instance Exception RPTError

-- | Bounds around the cutting plane
data Margin a = Margin {
  cMarginLow :: Max a -- ^ lower bound on the cut point
  , cMarginHigh :: Min a -- ^ upper bound
                   } deriving (Eq, Show, Generic)
instance (Serialise a) => Serialise (Margin a)
getMargin :: Margin a -> (a, a)
getMargin (Margin ml mh) = (getMax ml, getMin mh)
instance (NFData a) => NFData (Margin a)
-- | Used for updating in a streaming setting
instance (Ord a) => Semigroup (Margin a) where
  Margin lo1 hi1 <> Margin lo2 hi2 = Margin (lo1 <> lo2) (hi1 <> hi2)


-- | Sparse vectors with unboxed components
data SVector a = SV { svDim :: !Int, svVec :: VU.Vector (Int, a) } deriving (Eq, Ord, Generic)
instance (VU.Unbox a, Serialise a) => Serialise (SVector a)
instance (VU.Unbox a, Show a) => Show (SVector a) where
  show (SV n vv) = unwords ["SV", show n, show (VU.toList vv)]
instance NFData (SVector a)

fromListSv :: VU.Unbox a => Int -> [(Int, a)] -> SVector a
fromListSv n ll = SV n $ VU.fromList ll
-- | (Unsafe) Pack a 'SVector' from its vector dimension and components
--
-- Note : the relevant invariants are not checked :
--
-- * vector components are _assumed_ to be in increasing order
--
-- * vector dimension is larger than any component index
fromVectorSv :: Int -- ^ vector dimension
             -> VU.Vector (Int, a) -- ^ vector components (in increasing order)
             -> SVector a
fromVectorSv = SV

-- | Dense vectors with unboxed components
newtype DVector a = DV { dvVec :: VU.Vector a } deriving (Eq, Ord, Generic)
instance (VU.Unbox a, Serialise a) => Serialise (DVector a)
instance (VU.Unbox a, Show a) => Show (DVector a) where
  show (DV vv) = unwords ["DV", show (VU.toList vv)]
instance NFData (DVector a)

fromListDv :: VU.Unbox a => [a] -> DVector a
fromListDv ll = DV $ VU.fromList ll
fromVectorDv :: VU.Vector a -> DVector a
fromVectorDv = DV
toListDv :: (VU.Unbox a) => DVector a -> [a]
toListDv (DV v) = VU.toList v

-- | Internal
--
-- one projection vector per node (like @annoy@)
data RT v d a =
  RBin !d !(v d) !(RT v d a) !(RT v d a)
  | RTip { _rData :: !a } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
makeLensesFor [("_rData", "rData")] ''RT
instance (NFData (v d), NFData d, NFData a) => NFData (RT v d a)



-- | Internal
--
-- one projection vector per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
data RPT d a =
  Bin {
  _rpThreshold :: !d
  , _rpMargin :: !(Margin d)
  , _rpL :: !(RPT d a)
  , _rpR :: !(RPT d a) }
  | Tip { _rpData :: a }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
instance (Serialise a, Serialise d) => Serialise (RPT d a)
makeLensesFor [("_rpData", "rpData")] ''RPT
instance (NFData v, NFData a) => NFData (RPT v a)

-- | Random projection trees
--
-- The first type parameter corresponds to a floating point scalar value, the second is the type of the data collected at the leaves of the tree (e.g. lists of vectors)
--
-- We keep them separate to leverage the Functor instance for postprocessing and visualization
--
-- One projection vector per tree level (as suggested in https://www.cs.helsinki.fi/u/ttonteri/pub/bigdata2016.pdf )
data RPTree d a = RPTree {
  _rpVectors :: V.Vector (SVector d) -- ^ one random projection vector per tree level
  , _rpTree :: RPT d a
                         } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
instance (Serialise d, Serialise a, VU.Unbox d) => Serialise (RPTree d a)
makeLensesFor [("_rpTree", "rpTree")] ''RPTree
instance (NFData a, NFData d) => NFData (RPTree d a)

-- | A random projection forest is an ordered set of 'RPTree's
--
-- This supports efficient updates of the ensemble in the streaming/online setting.
type RPForest d a = IM.IntMap (RPTree d a)

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

-- -- points in 2d
-- data P a = P !a !a deriving (Eq, Show)

-- | Scale a vector
class Scale v where
  (.*) :: (VU.Unbox a, Num a) => a -> v a -> v a
instance Scale SVector where
  a .* (SV n vv) = SV n $ scaleS a vv
instance Scale VU.Vector where
  a .* v1 = scaleD a v1
instance Scale DVector where
  a .* (DV v1) = DV $ scaleD a v1

-- | Inner product spaces
--
-- This typeclass is provided as a convenience for library users to interface their own vector types.
class (Scale u, Scale v) => Inner u v where
  inner :: (VU.Unbox a, Num a) => u a -> v a -> a
  metricL2 :: (VU.Unbox a, Floating a) => u a -> v a -> a
  (^+^) :: (VU.Unbox a, Num a) => u a -> v a -> u a
  (^-^) :: (VU.Unbox a, Num a) => u a -> v a -> u a

instance Inner SVector SVector where
  inner (SV _ v1) (SV _ v2) = innerSS v1 v2
  metricL2 (SV _ v1) (SV _ v2) = metricSSL2 v1 v2
  (SV n v1) ^+^ (SV _ v2) = SV n $ sumSS v1 v2
  (SV n v1) ^-^ (SV _ v2) = SV n $ diffSS v1 v2
instance Inner SVector VU.Vector where
  inner (SV _ v1) v2 = innerSD v1 v2
  metricL2 (SV _ v1) v2 = metricSDL2 v1 v2
  (SV n v1) ^+^ v2 = SV n $ sumSD v1 v2
  (SV n v1) ^-^ v2 = SV n $ diffSD v1 v2
instance Inner SVector DVector where
  inner (SV _ v1) (DV v2) = innerSD v1 v2
  metricL2 (SV _ v1) (DV v2) = metricSDL2 v1 v2
  (SV n v1) ^+^ (DV v2) = SV n $ sumSD v1 v2
  (SV n v1) ^-^ (DV v2) = SV n $ diffSD v1 v2
instance Inner DVector DVector where
  inner (DV v1) (DV v2) = innerDD v1 v2
  metricL2 (DV v1) (DV v2) = metricDDL2 v1 v2
  DV v1 ^+^ DV v2 = DV $ VG.zipWith (+) v1 v2
  DV v1 ^-^ DV v2 = DV $ VG.zipWith (-) v1 v2

(/.) :: (Scale v, VU.Unbox a, Fractional a) => v a -> a -> v a
v /. a = (1 / a) .* v

normalize :: (VU.Unbox a, Inner v v, Floating a) => v a -> v a
normalize v = v /. metricL2 v v


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

innerDD :: (VG.Vector v a, Num a) => v a -> v a -> a
innerDD v1 v2 = VG.sum $ VG.zipWith (*) v1 v2


-- | Vector distance induced by the L2 norm (sparse-sparse)
metricSSL2 :: (Floating a, VG.Vector u a, VU.Unbox a, VG.Vector u (Int, a), VG.Vector v (Int, a)) =>
              u (Int, a) -> v (Int, a) -> a
metricSSL2 u v = sqrt $ VG.sum $ VG.map (\(_, x) -> x ** 2) duv
  where
    duv = u `diffSS` v

-- | Vector distance induced by the L2 norm (sparse-dense)
metricSDL2 :: (Floating a, VG.Vector v1 a, VU.Unbox a,
                VG.Vector v1 (Int, a), VG.Vector v2 a) =>
              v1 (Int, a) -> v2 a -> a
metricSDL2 u v = sqrt $ VG.sum $ VG.map (\(_, x) -> x ** 2) duv
  where
    duv = u `diffSD` v

-- | Vector distance induced by the L2 norm (dense-dense)
metricDDL2 :: (Floating a, VG.Vector v a) => v a -> v a -> a
metricDDL2 u v = sqrt $ VG.sum $ VG.map (** 2) duv
  where
    duv = VG.zipWith (-) u v

scaleD :: (VG.Vector v b, Num b) => b -> v b -> v b
scaleD a vv = VG.map (* a) vv

scaleS :: (VG.Vector v (a, b), Num b) => b -> v (a, b) -> v (a, b)
scaleS a vv = VG.map (\(i, x) -> (i, a * x)) vv

-- | Vector sum
sumSD :: (VG.Vector u (Int, a), VG.Vector v a, VU.Unbox a, Num a) =>
         u (Int, a) -> v a -> u (Int, a)
sumSD = binSD (-)

-- | Vector sum
sumSS :: (VG.Vector u (Int, a), VG.Vector v (Int, a), VU.Unbox a, Num a) =>
         u (Int, a) -> v (Int, a) -> u (Int, a)
sumSS = binSS (+) 0

-- | Vector difference
diffSD :: (VG.Vector u (Int, a), VG.Vector v a, VU.Unbox a, Num a) =>
          u (Int, a) -> v a -> u (Int, a)
diffSD = binSD (-)

-- | Vector difference
diffSS :: (VG.Vector u (Int, a), VG.Vector v (Int, a), VU.Unbox a, Num a) =>
          u (Int, a) -> v (Int, a) -> u (Int, a)
diffSS = binSS (-) 0

-- | Binary operation on 'SVector' s
binSS :: (VG.Vector u (Int, a), VG.Vector v (Int, a), VU.Unbox a) =>
         (a -> a -> a) -> a -> u (Int, a) -> v (Int, a) -> u (Int, a)
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


-- FIXME the return type of a sparse-dense binary operation depends on the operator itself (S * D = S , S + D = D ), so 'binSD' must be changed
binSD :: (VG.Vector u (Int, a), VG.Vector v a, VU.Unbox a) =>
         (a -> a -> a) -> u (Int, a) -> v a -> u (Int, a)
binSD f vv1 vv2 = VG.unfoldr go 0
  where
    nz1 = VG.length vv1
    nz2 = VG.length vv2
    go i
      | i >= nz1 || i >= nz2 = Nothing
      | otherwise = Just ((il, y), succ i)
          where
            (il, xl) = vv1 VG.! i
            xr       = vv2 VG.! il
            y = f xl xr


{-# SCC partitionAtMedian #-}
-- | Partition the data wrt the median value of the inner product
partitionAtMedian :: (Ord a, Inner u v, VU.Unbox a, Fractional a) =>
                     u a -- ^ projection vector
                  -> V.Vector (v a) -- ^ dataset (3 or more elements)
                  -> (a, Margin a, V.Vector (v a), V.Vector (v a)) -- ^ median, margin, smaller, larger
partitionAtMedian r xs = (thr, margin, ll, rr)
  where
    (ll, rr) = (VG.take nh xs', VG.drop nh xs')
    -- (pjl, pjr) = (VG.head inns, VG.last inns) -- (min, max) inner product values
    (mgl, mgr) = (inns VG.! (nh - 1), inns VG.! (nh + 1))
    margin = Margin (Max mgl) (Min mgr)
    thr = inns VG.! nh -- inner product threshold,  mgl < thr < mgr
    n = VG.length xs -- total data size
    nh = n `div` 2 -- size of left partition
    projs = sortByVG snd $ VG.map (\x -> (x, r `inner` x)) xs
    (xs', inns) = VG.unzip projs

sortByVG :: (VG.Vector v a, Ord b) => (a -> b) -> v a -> v a
sortByVG f v = runST $ do
  vm <- VG.thaw v
  V.sortBy (comparing f) vm
  VG.freeze vm







-- data Avg a = Avg {
--   avgCount :: !(Sum Int)
--   , avgSum :: !(Sum a)
--                  }
-- average :: (Foldable t, Fractional a) => t a -> a
-- average = getAvg . foldl' bumpAvg mempty
-- bumpAvg :: Num a => Avg a -> a -> Avg a
-- bumpAvg aa x = Avg (Sum 1) (Sum x) <> aa
-- instance (Num a) => Semigroup (Avg a) where
--   Avg c0 s0 <> Avg c1 s1 = Avg (c0<>c1) (s0<>s1)
-- instance (Num a) => Monoid (Avg a) where
--   mempty = Avg mempty mempty
-- getAvg :: Fractional a => Avg a -> a
-- getAvg (Avg c s) = getSum s / fromIntegral (getSum c)


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
