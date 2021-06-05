{-# LANGUAGE FlexibleContexts #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Draw where

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.List (intercalate)
import Text.Printf (PrintfArg, printf)

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)
-- bytestring
import qualified Data.ByteString.Lazy    as LBS (ByteString, writeFile)
import qualified Data.ByteString.Builder as BSB (Builder, toLazyByteString, string7, charUtf8)
-- containers
import qualified Data.Set as S (Set, insert, fromList)
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- transformers
import Control.Monad.Trans.State (State, evalState)
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), map, sum, unfoldr, unfoldrM, length, replicateM, (!))
import qualified Data.Vector.Unboxed as VU (Unbox)

import Data.RPTree.Internal (RPTree(..), RPT(..), DVector, toListDv)




-- | Encode dataset as CSV and save into file
-- writeCsv :: (Show a, Show b, VU.Unbox a) =>
--             FilePath
--          -> [(DVector a, b)] 
--          -> IO ()
writeCsv :: (Foldable t, VU.Unbox a, Show a, Show b) =>
            FilePath
         -> t (V.Vector (DVector a, b)) -- ^ data point, label
         -> IO ()
writeCsv fp ds = LBS.writeFile fp $ BSB.toLazyByteString $ toCsvV ds


toCsv :: (Foldable t, Show a, Show b, VU.Unbox a) =>
         t (DVector a, b) -> BSB.Builder
toCsv = foldMap (\(r, i) -> toCsvRow r i <> BSB.charUtf8 '\n' )
-- toCsv rs = mconcat [toCsvRow r i <> BSB.charUtf8 '\n' | (r, i) <- rs]

toCsvV :: (Foldable t, VU.Unbox a, Show a, Show b) =>
          t (V.Vector (DVector a, b)) -> BSB.Builder
toCsvV = foldMap (\v -> foldMap (\(r, i) -> toCsvRow r i <> BSB.charUtf8 '\n' ) v)

toCsvRow :: (Show a, Show b, VU.Unbox a) =>
            DVector a
         -> b
         -> BSB.Builder
toCsvRow dv i = BSB.string7 $ intercalate "," [show x, show y, show i]
  where
    (x:y:_) = toListDv dv

-- | Render a tree to stdout
--
-- Useful for debugging
--
-- This should be called only for small trees, otherwise the printed result quickly overflows the screen and becomes hard to read.
--
-- NB : prints distance information rounded to two decimal digits
draw :: (Show a, Boxed a, PrintfArg v) => RPTree v l a -> IO ()
draw = drawRPT . _rpTree

drawRPT :: (Show a, Boxed a, PrintfArg v) => RPT v l a -> IO ()
drawRPT = putStrLn . toStringRPT

toStringRPT :: (Show a, Boxed a, PrintfArg v) => RPT v l a -> String
toStringRPT = B.render . toBox

toBox :: (Show a, Boxed a, PrintfArg v) => RPT v l a -> B.Box
toBox = \case
  (Bin _ thr _ tl tr) ->
    txt (node thr) `stack` (toBox tl `byside` toBox tr)
  Tip _ xs -> boxed xs -- tipData xs -- txt $ show x
  where
    node x = printf "%5.2f" x -- (show x)

class Boxed a where
  boxed :: a -> B.Box
instance (Show a) => Boxed [a] where
  boxed = foldl (\bx x -> bx `stack` txt (show x)) $ B.emptyBox 0 0
instance Boxed () where
  boxed _ = txt "*"

tipData :: (Show a, Foldable t) => t a -> B.Box
tipData = foldl (\bx x -> bx `stack` txt (show x)) $ B.emptyBox 1 1

txt :: String -> B.Box
txt t = spc `byside` B.text t `byside` spc
  where spc = B.emptyBox 1 1

byside :: B.Box -> B.Box -> B.Box
byside l r = B.hcat B.top [l, r]

stack :: B.Box -> B.Box -> B.Box
stack t b = B.vcat B.center1 [t, b]



-- -- tree to graphviz dot format

toDot :: String -> RPT d x a -> LBS.ByteString
toDot name tt = BSB.toLazyByteString $ open <> x <> close
  where
    x = foldl insf mempty $ toEdges tt
      where
        insf acc (Edge i1 i2) = acc <> BSB.string7 (unwords [show i1, "->", show i2, "\n"] )
    open = BSB.string7 $ unwords ["digraph" , name, "{"]
    close = BSB.string7 "}"

data Edge = Edge Int Int deriving (Eq, Ord, Show)

-- toDot :: RPT d Int a -> Stack Edge
toEdges :: RPT d x a -> S.Set Edge
toEdges = S.fromList . go [] [] . labelBranches
  where
    go s acc = \case
      Tip i _ -> acc
      Bin i _ _ tl tr ->
        let
          acc' = maybe acc (\i0 -> push (Edge i0 i) acc) (pop s)
          s' = push i s
        in
          go s' acc' tl <> go s' acc tr



labelBranches :: Bitraversable t => t x d -> t Int d
labelBranches = flip evalState 0 . bitraverse counter pure

counter :: (MonadState Int m) => x -> m Int
counter _ = do
  i <- get
  modify succ
  pure i

type Stack a = [a]
push :: a -> Stack a -> Stack a
push = (:)
pop :: Stack a -> Maybe a
pop xs
  | null xs = Nothing
  | otherwise = Just $ head xs


-- tt0 :: RPT Integer () [a]
tt0 = Bin [] 0 mempty tl t
  where
    tl = Bin [] 1 mempty (Bin [] 2 mempty t t) (Bin [] 3 mempty t t)
    t = Tip [] []
