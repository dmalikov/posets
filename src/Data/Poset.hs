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
type Relation α = α → α → Bool

data Poset α = Poset [α] (Relation α)

-- | Build poset from a list of pairs connected by a binary relation
--
fromPairs ∷ Eq α ⇒ [α] → [(α,α)] → Poset α
fromPairs es rs = Poset es $ \a b → (a,b) `elem` rs

-- | fromPairs with satisfying reflexivity and transitivity
--
fromPairsE ∷ Eq α ⇒ [α] → [(α,α)] → Poset α
fromPairsE es rs = Poset es $ \a b → (a,b) `elem` expandedRelations
  where expandedRelations = nub [ (a,b) | a ← es, b ← connectWith g a ]
        g = graph rs

-- | Get poset elements
--
elements ∷ Eq α ⇒ Poset α → [α]
elements (Poset es _) = es

-- | Get poset relation
--
relation ∷ Eq α ⇒ Poset α → Relation α
relation (Poset _ ρ) = ρ

-- | Check Poset reflexivity
--
isReflexive ∷ Eq α ⇒ Poset α → Bool
isReflexive (Poset es ρ) = and [ a `ρ` a  | a ← es ]

-- | Check Poset antisymmetry
--
isAntisymmetric ∷ Eq α ⇒ Poset α → Bool
isAntisymmetric (Poset es ρ) = and
  [ a == b | a ← es, b ← es
  , a `ρ` b
  , b `ρ` a
  ]

-- | Check Poset transitivity
--
isTransitive ∷ Eq α ⇒ Poset α → Bool
isTransitive (Poset es ρ) = and
  [ a `ρ` c | a ← es, b ← es, c ← es
  , a `ρ` b
  , b `ρ` c
  ]

-- | Check poset correctness
--
isValid ∷ Eq α ⇒ Poset α → Bool
isValid p = isReflexive p && isAntisymmetric p && isTransitive p

-- | Find LowerCone of Poset element
--  LowerCone is a set of elements connected with element by Binary Relation ρ
--
lowerCone ∷ Eq α ⇒ Poset α → α → [α]
lowerCone (Poset es ρ) a = [ b | b ← es, b `ρ` a ]

-- | infimums of Poset is an intersection of lowerCones of all elements
--
infimums ∷ Eq α ⇒ Poset α → [α]
infimums p@(Poset es _) = foldl1 intersect $ map (lowerCone p) es

-- | infimum is an infimums with `Maybe' handle
--
infimum ∷ Eq α ⇒ Poset α → Maybe α
infimum = listToMaybe . infimums

-- | Find infinum of 2 elements of Poset
--
infimums' ∷ Eq α ⇒ Poset α → α → α → [α]
infimums' p = intersect `on` lowerCone p

infimum' ∷ Eq α ⇒ Poset α → α → α → Maybe α
infimum' p a b = listToMaybe $ infimums' p a b
