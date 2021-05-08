{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Draw where

import Data.List (intercalate)
import Text.Printf (PrintfArg, printf)

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)
-- bytestring
import qualified Data.ByteString.Lazy    as LBS (ByteString, writeFile)
import qualified Data.ByteString.Builder as BSB (Builder, toLazyByteString, string7, charUtf8)
-- -- mtl
-- import Control.Monad.State (MonadState(..))
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), map, sum, unfoldr, unfoldrM, length, replicateM, (!))
import qualified Data.Vector.Unboxed as VU (Unbox)

import Data.RPTree.Internal (RPTree(..), RPT(..), DVector, toListDv)




-- | Encode dataset as CSV and save into file
writeCsv :: (Show a, Show b, VU.Unbox a) =>
            FilePath
         -> [(DVector a, b)] -- ^ data point, label
         -> IO ()
writeCsv fp ds = LBS.writeFile fp $ BSB.toLazyByteString $ toCsv ds

toCsvRow :: (Show a, Show b, VU.Unbox a) =>
            DVector a
         -> b
         -> BSB.Builder
toCsvRow dv i = BSB.string7 $ intercalate "," [show x, show y, show i]
  where
    (x:y:_) = toListDv dv

toCsv :: (Show a, Show b, VU.Unbox a) =>
         [(DVector a, b)] -> BSB.Builder
toCsv rs = mconcat [toCsvRow r i <> BSB.charUtf8 '\n' | (r, i) <- rs]

-- | Render a tree to stdout
--
-- Useful for debugging
--
-- This should be called only for small trees, otherwise the printed result quickly overflows the screen and becomes hard to read.
--
-- NB : prints distance information rounded to two decimal digits
draw :: (Show a, Boxed a, PrintfArg v) => RPTree v a -> IO ()
draw = drawRPT . _rpTree

drawRPT :: (Show a, Boxed a, PrintfArg v) => RPT v a -> IO ()
drawRPT = putStrLn . toStringRPT

toStringRPT :: (Show a, Boxed a, PrintfArg v) => RPT v a -> String
toStringRPT = B.render . toBox

toBox :: (Show a, Boxed a, PrintfArg v) => RPT v a -> B.Box
toBox = \case
  (Bin thr _ tl tr) ->
    txt (node thr) `stack` (toBox tl `byside` toBox tr)
  Tip xs -> boxed xs -- tipData xs -- txt $ show x
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
