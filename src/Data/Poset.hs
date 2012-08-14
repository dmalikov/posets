{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset where

import Data.Function (on)
import Data.Graph (buildG, reachable)
import Data.List (intersect, nub)
import Data.Maybe (listToMaybe)

data Poset = Poset
-- | Set of elements
--
  { elements ∷ [Int]

-- | Binary relation ρ setted by list of connected pairs of elements
--
-- | ρ should be:
-- |   reflexive: α ρ α ;
-- |   antisymmetric: if α ρ β and β ρ α ⇒ α = β ;
-- |   transitive: if α ρ β and β ρ γ ⇒ α ρ γ
--
  , relations ∷ [(Int,Int)]

  } deriving (Eq, Show, Read)


-- | Relation between two spicified elements of current Poset
--
binaryRelation ∷ Poset → Int → Int → Bool
binaryRelation (Poset es rs) a b = and
  [ a `elem` es, b `elem` es, (a,b) `elem` rs ]

-- | Relation between two spicified elements of expanded Poset
--
binaryRelationE ∷ Poset → Int → Int → Bool
binaryRelationE = binaryRelation . expand

-- | expand Poset - apply reflexivity and transitivity
--
expand ∷ Poset → Poset
expand (Poset es rs) = Poset es allPairs
  where allPairs = nub [ (a,b) | a ← es, b ← reachable graph a ]
        graph = buildG (1, length es) rs

-- | Check Poset reflexivity
--
isReflexive ∷ Poset → Bool
isReflexive (Poset es rs) = and
  [ (a,a) `elem` rs | a ← es ]

-- | Check Poset antisymmetry
--
isAntisymmetric ∷ Poset → Bool
isAntisymmetric (Poset es rs) = and
  [ a == b | a ← es, b ← es
  , (a,b) `elem` rs
  , (b,a) `elem` rs
  ]

-- | Check Poset transitivity
--
isTransitive ∷ Poset → Bool
isTransitive p@(Poset es _) = and
  [ a `ρ` c | a ← es, b ← es, c ← es
  , a `ρ` b
  , b `ρ` c
  ] where ρ = binaryRelation p

-- | Check poset correctness
--
isValid ∷ Poset → Bool
isValid p = isReflexive p && isAntisymmetric p && isTransitive p

-- | Check correctness of expanded Poset
--
isValidE ∷ Poset → Bool
isValidE = isValid . expand

-- | Find LowerCone of Poset element
--  LowerCone is a set of elements connected with element by Binary Relation ρ
--
lowerCone ∷ Poset → Int → [Int]
lowerCone (Poset es rs) = reachable $ buildG (1, length es) rs

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
infimums' p = intersect `on` (lowerCone p)

infimum' ∷ Poset → Int → Int → Maybe Int
infimum' p a b = listToMaybe $ infimums' p a b
