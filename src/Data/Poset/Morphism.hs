{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset.Morphism where

import Data.Poset
import Prelude hiding ((>>))

import qualified Data.IntMap as IM

-- | Morph states for «morphism»
-- Morph is a map from Poset to Poset
--
type Morph = IM.IntMap Int

-- | Build Morph from list
--
fromList ∷ [(Int,Int)] → Morph
fromList = IM.fromList

-- | Apply Morph to Poset element
--
infixr 6 >>

(>>) ∷ Morph → Int → Int
m >> a = m IM.! a

-- | Check for isotone property
-- a ρ b ⇒ Morph(a) ρ Morph(b)
--
isotone ∷ Morph → Poset → Bool
isotone m p@(Poset es _) = and
  [ (m >> a) `ρ` (m >> b) | a ← es, b ← es ]
    where ρ = binaryRelation p

-- | Check for reducibility
-- Morph(a) ρ a
--
reducible ∷ Morph → Poset → Bool
reducible m p@(Poset es _) = and
  [ (m >> a) `ρ` a | a ← es ]
    where ρ = binaryRelation p

-- | Check for idempotency
-- Morph ∘ Morph ≡ Morph
--
idempotent ∷ Morph → Poset → Bool
idempotent m (Poset es _) = and
  [ (m >> m >> a) == (m >> a) | a ← es ]

-- | Check for fixity
-- b ≡ Morph(b) AND a ρ b ⇒ a ≡ Morph(a)
--
fixed ∷ Morph → Poset → Bool
fixed m p@(Poset es _) = and
  [ a == (m >> a) | b ← es, b == (m >> b), a ← es, a `ρ` b ]
    where ρ = binaryRelation p

