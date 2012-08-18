{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset.Semilattice where

import Data.Maybe (isJust)

import Data.Poset

-- | Semilattice is a Poset if infimum for every two elements exists
--
isSemilattice ∷ Eq α ⇒ Poset α → Bool
isSemilattice p@(Poset es _) = and
  [ isJust $ infimum' p a b | a ← es, b ← es ]
