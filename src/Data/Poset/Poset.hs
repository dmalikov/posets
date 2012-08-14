{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset where

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
  [ a `br` c | a ← es, b ← es, c ← es
  , a `br` b
  , b `br` c
  ] where br = binaryRelation p

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

-- | Infinums of Poset is an intersection of lowerCones of all elements
--
infinums ∷ Poset → [Int]
infinums p@(Poset es _) = foldl1 intersect $ map (lowerCone p) es

-- | Infinum is an infinums with `Maybe' handle
--
infinum ∷ Poset → Maybe Int
infinum = listToMaybe . infinums
