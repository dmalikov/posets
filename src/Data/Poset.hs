{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset where

import Data.Function (on)
import Data.Poset.Graph (graph, connectWith)
import Data.List (intersect, nub)
import Data.Maybe (listToMaybe)

-- | Binary relation
-- `relation a b` is True iff `a ρ b`.
--
-- | Binary relation ρ should be:
-- |   reflexive: α ρ α ;
-- |   antisymmetric: if α ρ β and β ρ α ⇒ α = β ;
-- |   transitive: if α ρ β and β ρ γ ⇒ α ρ γ .
--
type Relation = Int → Int → Bool

data Poset = Poset [Int] Relation

-- | Build poset from a list of pairs connected by a binary relation
--
fromPairs ∷ [Int] → [(Int,Int)] → Poset
fromPairs es rs = Poset es $ \a b → (a,b) `elem` rs

-- | fromPairs with satisfying reflexivity and transitivity
--
fromPairsE ∷ [Int] → [(Int,Int)] → Poset
fromPairsE es rs = Poset es $ \a b → (a,b) `elem` expandedRelations
  where expandedRelations = nub [ (a,b) | a ← es, b ← connectWith g a ]
        g = graph rs

-- | Get poset elements
--
elements ∷ Poset → [Int]
elements (Poset es _) = es

-- | Get poset relation
--
relation ∷ Poset → Relation
relation (Poset _ ρ) = ρ

-- | Check Poset reflexivity
--
isReflexive ∷ Poset → Bool
isReflexive (Poset es ρ) = and [ a `ρ` a  | a ← es ]

-- | Check Poset antisymmetry
--
isAntisymmetric ∷ Poset → Bool
isAntisymmetric (Poset es ρ) = and
  [ a == b | a ← es, b ← es
  , a `ρ` b
  , b `ρ` a
  ]

-- | Check Poset transitivity
--
isTransitive ∷ Poset → Bool
isTransitive (Poset es ρ) = and
  [ a `ρ` c | a ← es, b ← es, c ← es
  , a `ρ` b
  , b `ρ` c
  ]

-- | Check poset correctness
--
isValid ∷ Poset → Bool
isValid p = isReflexive p && isAntisymmetric p && isTransitive p

-- | Find LowerCone of Poset element
--  LowerCone is a set of elements connected with element by Binary Relation ρ
--
lowerCone ∷ Poset → Int → [Int]
lowerCone (Poset es ρ) a = [ b | b ← es, b `ρ` a ]

-- | infimums of Poset is an intersection of lowerCones of all elements
--
infimums ∷ Poset → [Int]
infimums p@(Poset es _) = foldl1 intersect $ map (lowerCone p) es

-- | infimum is an infimums with `Maybe' handle
--
infimum ∷ Poset → Maybe Int
infimum = listToMaybe . infimums

-- | Find infinum of 2 elements of Poset
--
infimums' ∷ Poset → Int → Int → [Int]
infimums' p = intersect `on` lowerCone p

infimum' ∷ Poset → Int → Int → Maybe Int
infimum' p a b = listToMaybe $ infimums' p a b
