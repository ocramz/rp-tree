{-# language LambdaCase #-}
module Data.RPTree.Draw where

import Text.Printf (PrintfArg, printf)

import Data.RPTree.Internal (RPTree(..), RPT(..))

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)

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
  (Bin thr tl tr) ->
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
