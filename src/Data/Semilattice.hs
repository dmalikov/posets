{-# LANGUAGE UnicodeSyntax #-}
module Data.Semilattice where

import Data.Maybe (isJust)

import Data.Poset

-- | Semilattice is a Poset if infimum for every two elements exists
--
isSemilattice ∷ Poset → Bool
isSemilattice p@(Poset es _) = and
  [ isJust $ infimum' p a b | a ← es, b ← es ]
